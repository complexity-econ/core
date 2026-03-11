package sfc.engine.mechanisms

import sfc.agents.*
import sfc.config.SimParams
import sfc.types.*

import scala.util.Random

/** Endogenous firm entry (#35): recycle bankrupt firm slots.
  *
  * Dead firms are replaced stochastically based on sector profitability
  * signals, entry barriers (GUS CEIDG/KRS 2024), and AI-native startup
  * probability. New firms are assigned a sector (profit-weighted random
  * choice), bank (affinity-weighted), network neighbors (random from living
  * firms), and initial endowments (capital, inventory, green K).
  *
  * FDI M&A conversion (monthly domestic → foreign ownership flip) is also
  * handled here as it operates on the same firm vector.
  */
object FirmEntry:

  case class Input(
      firms: Vector[Firm.State],
      automationRatio: Double,
      hybridRatio: Double,
      rng: Random,
  )

  case class Output(
      firms: Vector[Firm.State],
      births: Int,
  )

  /** Apply FDI M&A conversion: flip qualifying domestic firms to foreign. */
  def applyFdiMa(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    if p.flags.fdi && p.fdi.maProb.toDouble > 0 then
      firms.map { f =>
        if Firm.isAlive(f) && !f.foreignOwned &&
          f.initialSize >= p.fdi.maSizeMin &&
          rng.nextDouble() < p.fdi.maProb.toDouble
        then f.copy(foreignOwned = true)
        else f
      }
    else firms

  /** Recycle bankrupt slots into new entrant firms. */
  def process(in: Input)(using p: SimParams): Output =
    if !p.flags.firmEntry then Output(in.firms, 0)
    else
      val profitSignals = computeProfitSignals(in.firms)
      val sectorWeights = computeSectorWeights(profitSignals)
      val totalWeight   = sectorWeights.sum
      val totalAdoption = in.automationRatio + in.hybridRatio
      val livingIds     = in.firms.filter(Firm.isAlive).map(_.id.toInt)
      var births        = 0

      val result = in.firms.map { f =>
        if !Firm.isAlive(f) then
          val entryProb = p.firm.entryRate.toDouble * p.firm.entrySectorBarriers(f.sector.toInt) *
            Math.max(0.0, 1.0 + profitSignals(f.sector.toInt) * p.firm.entryProfitSens)
          if in.rng.nextDouble() < entryProb then
            births += 1
            spawnFirm(f, livingIds, sectorWeights, totalWeight, totalAdoption, in.rng)
          else f
        else f
      }
      Output(result, births)

  // --- Private helpers ---

  /** Profit signals per sector: normalized deviation of sector avg cash from
    * global avg.
    */
  private def computeProfitSignals(firms: Vector[Firm.State])(using p: SimParams): Array[Double] =
    val living        = firms.filter(Firm.isAlive)
    val sectorCashSum = Array.fill(p.sectorDefs.length)(0.0)
    val sectorCashCnt = Array.fill(p.sectorDefs.length)(0)
    for f <- living do
      sectorCashSum(f.sector.toInt) += f.cash.toDouble
      sectorCashCnt(f.sector.toInt) += 1
    val sectorAvgCash =
      p.sectorDefs.indices.map(s => if sectorCashCnt(s) > 0 then sectorCashSum(s) / sectorCashCnt(s) else 0.0).toArray
    val globalAvgCash = if living.nonEmpty then living.map(_.cash.toDouble).sum / living.length else 1.0
    sectorAvgCash.map(c => Math.max(-1.0, Math.min(2.0, (c - globalAvgCash) / Math.max(1.0, Math.abs(globalAvgCash)))))

  /** Entry weights per sector: profit signal × barrier filter. */
  private def computeSectorWeights(profitSignals: Array[Double])(using p: SimParams): Array[Double] =
    p.sectorDefs.indices.map { s =>
      Math.max(0.01, (1.0 + profitSignals(s) * p.firm.entryProfitSens) * p.firm.entrySectorBarriers(s))
    }.toArray

  /** Pick a sector via weighted random choice. */
  private def pickSector(sectorWeights: Array[Double], totalWeight: Double, rng: Random): Int =
    val roll  = rng.nextDouble() * totalWeight
    var cumul = 0.0
    var idx   = 0
    while idx < sectorWeights.length - 1 && { cumul += sectorWeights(idx); cumul <= roll } do idx += 1
    idx

  /** Spawn a new firm in a bankrupt slot. */
  private def spawnFirm(
      slot: Firm.State,
      livingIds: Vector[Int],
      sectorWeights: Array[Double],
      totalWeight: Double,
      totalAdoption: Double,
      rng: Random,
  )(using p: SimParams): Firm.State =
    val newSector = pickSector(sectorWeights, totalWeight, rng)
    val firmSize  = Math.max(1, rng.between(1, 10))
    val sizeMult  = firmSize.toDouble / p.pop.workersPerFirm

    val isAiNative = totalAdoption > p.firm.entryAiThreshold.toDouble &&
      rng.nextDouble() < p.firm.entryAiProb.toDouble
    val dr         =
      if isAiNative then rng.between(0.50, 0.90)
      else Math.max(0.02, Math.min(0.30, p.sectorDefs(newSector).baseDigitalReadiness.toDouble + rng.nextGaussian() * 0.10))

    val startWorkers = 0
    val tech         = if isAiNative then
      val hw = Math.max(1, (startWorkers * 0.6).toInt)
      TechState.Hybrid(hw, 0.5 + rng.nextDouble() * 0.3)
    else TechState.Traditional(startWorkers)

    val nNeighbors   = Math.min(6, livingIds.length)
    val newNeighbors =
      if nNeighbors > 0 then rng.shuffle(livingIds.toList).take(nNeighbors).map(FirmId(_)).toVector
      else Vector.empty[FirmId]

    val newBankId    = Banking.assignBank(SectorIdx(newSector), Banking.DefaultConfigs, rng)
    val foreignOwned = p.flags.fdi && rng.nextDouble() < p.fdi.foreignShares.map(_.toDouble)(newSector)

    val capitalStock  =
      if p.flags.physCap then firmSize.toDouble * p.capital.klRatios.map(_.toDouble)(newSector)
      else 0.0
    val initInventory = if p.flags.inventory then
      val cap = p.firm.baseRevenue.toDouble * sizeMult * p.sectorDefs(newSector).revenueMultiplier
      cap * p.capital.inventoryTargetRatios.map(_.toDouble)(newSector) * p.capital.inventoryInitRatio.toDouble
    else 0.0
    val initGreenK    =
      if p.flags.energy then firmSize.toDouble * p.climate.greenKLRatios.map(_.toDouble)(newSector) * p.climate.greenInitRatio.toDouble
      else 0.0

    Firm.State(
      id = slot.id,
      cash = PLN(p.firm.entryStartupCash.toDouble * sizeMult),
      debt = PLN.Zero,
      tech = tech,
      riskProfile = Ratio(rng.between(0.1, 0.9)),
      innovationCostFactor = rng.between(0.8, 1.5),
      digitalReadiness = Ratio(dr),
      sector = SectorIdx(newSector),
      neighbors = newNeighbors,
      bankId = newBankId,
      equityRaised = PLN.Zero,
      initialSize = firmSize,
      capitalStock = PLN(capitalStock),
      bondDebt = PLN.Zero,
      foreignOwned = foreignOwned,
      inventory = PLN(initInventory),
      greenCapital = PLN(initGreenK),
    )
