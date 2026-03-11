package sfc.engine.mechanisms

import sfc.agents.*
import sfc.config.SimParams
import sfc.types.*

import scala.util.Random

/** Cross-sector labour mobility: friction-based transitions, voluntary quits,
  * wage penalties.
  *
  * Workers changing sectors face friction costs calibrated to a 6×6 symmetric
  * transition matrix (GUS BAEL 2023 inter-industry flows). Friction affects:
  * retraining duration/cost, success probability, and post-transition wage.
  *
  * Target sector selection is probabilistic, weighted by destination wage,
  * vacancy count, and inverse friction (gravity-like model).
  *
  * Sectors: BPO(0), Manufacturing(1), Retail(2), Healthcare(3), Public(4),
  * Agriculture(5).
  */
object SectoralMobility:

  // ---- Calibration constants ----
  private val MaxWagePenalty   = 0.3 // cross-sector wage penalty: up to 30% of friction
  private val FrictionSuccDisc = 0.5 // friction discount on retraining success probability
  private val NumSectors       = 6

  case class State(
      crossSectorHires: Int,     // workers hired into a different sector this step
      voluntaryQuits: Int,       // workers who quit to seek better sector
      sectorMobilityRate: Double, // cross-sector hires / total employed
  )

  object State:
    val zero: State = State(0, 0, 0.0)

  def zero: State = State.zero

  /** Default 6×6 symmetric transition friction matrix. f(i,j) ∈ [0,1]: 0 =
    * frictionless, 1 = near-impossible. GUS BAEL 2023 inter-industry flows.
    */
  val DefaultFrictionMatrix: Vector[Vector[Double]] = Vector(
    //       BPO   Mfg   Ret   Hlt   Pub   Agr
    Vector(0.0, 0.6, 0.3, 0.8, 0.5, 0.9), // BPO
    Vector(0.6, 0.0, 0.5, 0.7, 0.6, 0.4), // Mfg
    Vector(0.3, 0.5, 0.0, 0.5, 0.4, 0.6), // Ret
    Vector(0.8, 0.7, 0.5, 0.0, 0.4, 0.8), // Hlt
    Vector(0.5, 0.6, 0.4, 0.4, 0.0, 0.7), // Pub
    Vector(0.9, 0.4, 0.6, 0.8, 0.7, 0.0), // Agr
  )

  /** Compute number of vacancies per sector. */
  def sectorVacancies(households: Vector[Household.State], firms: Vector[Firm.State])(using SimParams): Array[Int] =
    val workerCounts = new Array[Int](NumSectors)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(_, sectorIdx, _) => workerCounts(sectorIdx.toInt) += 1
        case _                                  =>

    val sectorDemand = new Array[Int](NumSectors)
    for f <- firms if Firm.isAlive(f) do sectorDemand(f.sector.toInt) += Firm.workerCount(f)

    val vac = new Array[Int](NumSectors)
    for s <- 0 until NumSectors do vac(s) = Math.max(0, sectorDemand(s) - workerCounts(s))
    vac

  /** Compute average wage per sector from employed households. */
  def sectorWages(households: Vector[Household.State]): Array[Double] =
    val sums   = new Array[Double](NumSectors)
    val counts = new Array[Int](NumSectors)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(_, sectorIdx, wage) =>
          sums(sectorIdx.toInt) += wage.toDouble
          counts(sectorIdx.toInt) += 1
        case _                                     =>
    val result = new Array[Double](NumSectors)
    for s <- 0 until NumSectors do result(s) = if counts(s) > 0 then sums(s) / counts(s) else 0.0
    result

  /** Probabilistic target sector selection (gravity model). score(to) =
    * wage(to) × (vacancies(to) + 1)^vacancyWeight × (1 − friction(from,to)).
    */
  def selectTargetSector(
      from: Int,
      wages: Array[Double],
      vacancies: Array[Int],
      matrix: Vector[Vector[Double]],
      vacancyWeight: Double,
      rng: Random,
  )(using p: SimParams): Int =
    val scores = new Array[Double](NumSectors)
    var total  = 0.0
    for to <- 0 until NumSectors if to != from do
      val s = Math.max(0.0, wages(to)) *
        Math.pow(vacancies(to).toDouble + 1.0, vacancyWeight) *
        (1.0 - matrix(from)(to))
      scores(to) = s
      total += s

    if total <= 0.0 then
      val others = (0 until NumSectors).filter(_ != from)
      others(rng.nextInt(others.length))
    else
      val r   = rng.nextDouble() * total
      var cum = 0.0
      var sel = -1
      var i   = 0
      while i < NumSectors && sel < 0 do
        if i != from && scores(i) > 0 then
          cum += scores(i)
          if cum >= r then sel = i
        i += 1
      if sel < 0 then (0 until NumSectors).find(to => to != from && scores(to) > 0).getOrElse(if from == 0 then 1 else 0)
      else sel

  /** Adjust retraining duration and cost by friction level. */
  def frictionAdjustedParams(friction: Double, durationMult: Double, costMult: Double)(using
      p: SimParams,
  ): (Int, Double) =
    val adjDuration = Math.round(p.household.retrainingDuration * (1.0 + friction * durationMult)).toInt
    val adjCost     = p.household.retrainingCost.toDouble * (1.0 + friction * costMult)
    (adjDuration, adjCost)

  /** Cross-sector wage penalty: proportional to friction, max 30%. */
  def crossSectorWagePenalty(friction: Double): Double =
    1.0 - friction * MaxWagePenalty

  /** Friction-adjusted success probability for retraining. */
  def frictionAdjustedSuccess(baseProbSkillHealth: Double, friction: Double): Double =
    baseProbSkillHealth * (1.0 - friction * FrictionSuccDisc)
