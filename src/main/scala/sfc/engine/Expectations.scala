package sfc.engine

import sfc.config.{Config, RunConfig}

case class ExpectationsState(
  expectedInflation: Double = 0.025,
  expectedRate: Double = 0.0575,
  credibility: Double = 0.8,
  forecastError: Double = 0.0,
  forwardGuidanceRate: Double = 0.0575
)

object Expectations:
  def zero: ExpectationsState = ExpectationsState()

  def initial: ExpectationsState = ExpectationsState(
    expectedInflation = Config.NbpTargetInfl,
    expectedRate = Config.NbpInitialRate,
    credibility = Config.ExpCredibilityInit,
    forecastError = 0.0,
    forwardGuidanceRate = Config.NbpInitialRate
  )

  def step(prev: ExpectationsState, realizedInflation: Double,
           currentRate: Double, unemployment: Double,
           rc: RunConfig): ExpectationsState =

    // 1. Forecast error
    val error = realizedInflation - prev.expectedInflation

    // 2. Adaptive update
    val adaptive = prev.expectedInflation + Config.ExpLambda * error

    // 3. Anchoring: blend target with adaptive based on credibility
    val target = if rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
    val expected = prev.credibility * target + (1.0 - prev.credibility) * adaptive

    // 4. Credibility update (asymmetric: harder to build than to lose)
    val absDeviation = Math.abs(realizedInflation - target)
    val threshold = Config.ExpCredibilityThreshold
    val speed = Config.ExpCredibilitySpeed
    val rawCredibility = if absDeviation <= threshold then
      // Below threshold: build credibility
      prev.credibility + speed * (1.0 - prev.credibility) *
        (threshold - absDeviation) / threshold
    else
      // Above threshold: erode credibility
      prev.credibility - speed * prev.credibility *
        (absDeviation - threshold) / threshold
    val newCredibility = Math.max(0.01, Math.min(1.0, rawCredibility))

    // 5. Forward guidance rate (when enabled)
    val neutral = if rc.isEurozone then Config.EcbNeutralRate else Config.NbpNeutralRate
    val alpha = if rc.isEurozone then Config.EcbAlpha else Config.TaylorAlpha
    val delta = if rc.isEurozone then 0.0 else Config.TaylorDelta
    val nairu = Config.NbpNairu
    val rawOutputGap = (unemployment - nairu) / nairu
    val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))

    val fgRate = if Config.NbpForwardGuidance then
      val rawFg = neutral + alpha * (expected - target) - delta * outputGap
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, rawFg))
    else currentRate

    // 6. Expected rate
    val adaptiveRate = prev.expectedRate + Config.ExpLambda * (currentRate - prev.expectedRate)
    val expRate = if Config.NbpForwardGuidance then
      0.6 * fgRate + 0.4 * adaptiveRate
    else adaptiveRate

    ExpectationsState(
      expectedInflation = expected,
      expectedRate = expRate,
      credibility = newCredibility,
      forecastError = error,
      forwardGuidanceRate = fgRate
    )
