package sfc.agents

import sfc.config.Config

case class NbpState(
  referenceRate: Double,
  govBondHoldings: Double = 0.0,
  qeActive: Boolean = false,
  qeCumulative: Double = 0.0
)

object CentralBankLogic:

  /** Bond yield = refRate + termPremium + fiscalRiskPremium - qeCompression - foreignDemandEffect */
  def bondYield(refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double): Double =
    if !Config.GovBondMarket then refRate
    else
      val termPremium = Config.GovTermPremium
      val fiscalRisk = Config.GovFiscalRiskBeta * Math.max(0.0, debtToGdp - 0.40)
      val qeCompress = 0.5 * nbpBondGdpShare
      val foreignDemand = if nfa > 0 then 0.005 else 0.0
      Math.max(0.0, refRate + termPremium + fiscalRisk - qeCompress - foreignDemand)

  /** Should NBP activate QE? Rate at floor + inflation below target - 1pp */
  def shouldActivateQe(refRate: Double, inflation: Double): Boolean =
    Config.NbpQe &&
    refRate <= Config.RateFloor + 0.0025 &&
    inflation < Config.NbpTargetInfl - 0.01

  /** Should NBP taper QE? Inflation returned above target */
  def shouldTaperQe(inflation: Double): Boolean =
    inflation > Config.NbpTargetInfl

  /** Execute monthly QE purchase. Returns (newNbpState, purchaseAmount). */
  def executeQe(nbp: NbpState, bankBondHoldings: Double, annualGdp: Double): (NbpState, Double) =
    if !nbp.qeActive then (nbp, 0.0)
    else
      val maxByGdp = Config.NbpQeMaxGdpShare * annualGdp - nbp.govBondHoldings
      val available = bankBondHoldings
      val purchase = Math.max(0.0, Math.min(Config.NbpQePace, Math.min(maxByGdp, available)))
      val newNbp = nbp.copy(
        govBondHoldings = nbp.govBondHoldings + purchase,
        qeCumulative = nbp.qeCumulative + purchase
      )
      (newNbp, purchase)
