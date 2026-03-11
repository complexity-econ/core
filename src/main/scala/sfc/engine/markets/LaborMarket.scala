package sfc.engine.markets

import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.mechanisms.SectoralMobility
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

/** Labor market: wage determination, separations, job search, wage updating.
  *
  * Wage adjustment via excess demand (Phillips curve); individual wages
  * heterogeneous by sector, skill, health, education, and immigrant status,
  * normalized so mean employed wage equals the macro market wage.
  *
  * Separations: workers displaced by automation (Traditional→Hybrid/Automated)
  * or firm bankruptcy, with education-based retention priority. Job search:
  * skill-ranked matching with same-sector priority and cross-sector friction
  * penalty (when sectoral mobility enabled).
  *
  * Calibration: GUS BAEL (Labor Force Survey) 2024, NBP wage statistics.
  */
object LaborMarket:

  /** Aggregate wage clearing result. */
  case class WageResult(wage: PLN, employed: Int)

  /** Job search matching result. */
  case class JobSearchResult(households: Vector[Household.State], crossSectorHires: Int)

  // --- Aggregate wage clearing ---

  /** Logistic labor supply curve: fraction of population willing to work at
    * given wage. Steepness controlled by p.household.laborSupplySteepness.
    */
  private def laborSupplyRatio(wage: Double, resWage: Double)(using p: SimParams): Double =
    val x = p.household.laborSupplySteepness * (wage / resWage - 1.0)
    1.0 / (1.0 + Math.exp(-x))

  /** Aggregate wage clearing: adjust market wage via excess demand, then
    * compute employment. New wage = max(reservationWage, prevWage × (1 +
    * excessDemand × adjSpeed)).
    */
  def updateLaborMarket(prevWage: PLN, resWage: PLN, laborDemand: Int, totalPopulation: Int)(using
      p: SimParams,
  ): WageResult =
    val supplyAtPrev = (totalPopulation * laborSupplyRatio(prevWage.toDouble, resWage.toDouble)).toInt
    val excessDemand = (laborDemand - supplyAtPrev).toDouble / totalPopulation
    val wageGrowth   = excessDemand * p.household.wageAdjSpeed.toDouble
    val newWage      = PLN(Math.max(resWage.toDouble, prevWage.toDouble * (1.0 + wageGrowth)))
    val newSupply    = (totalPopulation * laborSupplyRatio(newWage.toDouble, resWage.toDouble)).toInt
    val employed     = Math.min(laborDemand, newSupply)
    WageResult(newWage, employed)

  // --- Separations ---

  /** Separate workers from firms that automated or went bankrupt this step.
    * Education-ranked retention: highest-educated workers kept first when firm
    * transitions to Hybrid or Automated (skeleton crew). Returns updated
    * households with newly unemployed workers.
    */
  def separations(
      households: Vector[Household.State],
      prevFirms: Vector[Firm.State],
      newFirms: Vector[Firm.State],
  )(using SimParams): Vector[Household.State] =
    // Build set of firm indices that lost workers this step
    val firmLostWorkers = (0 until newFirms.length).filter { i =>
      val prevAlive      = Firm.isAlive(prevFirms(i))
      val newAlive       = Firm.isAlive(newFirms(i))
      val newlyAutomated = (prevFirms(i).tech, newFirms(i).tech) match
        case (_: TechState.Automated, _) => false // already automated
        case (_, _: TechState.Automated) => true  // just automated
        case _                           => false
      val hybridReduced  = (prevFirms(i).tech, newFirms(i).tech) match
        case (TechState.Traditional(w1), TechState.Hybrid(w2, _)) => w2 < w1
        case _                                                    => false
      (!newAlive && prevAlive) || newlyAutomated || hybridReduced
    }.toSet

    // For firms that went to Hybrid, compute how many workers they retained
    val hybridRetained: Map[Int, Int] = newFirms
      .filter(f => firmLostWorkers.contains(f.id.toInt))
      .flatMap: f =>
        f.tech match
          case TechState.Hybrid(w, _) => Some(f.id.toInt -> w)
          case _                      => None
      .toMap

    // For automated firms, only skeleton crew stays
    val automatedRetained: Map[Int, Int] = newFirms
      .filter(f => firmLostWorkers.contains(f.id.toInt))
      .flatMap: f =>
        f.tech match
          case _: TechState.Automated => Some(f.id.toInt -> Firm.skeletonCrew(f))
          case _                      => None
      .toMap

    // Build retain sets: sort workers by (-education, -skill), take top maxRetain
    val firmToWorkers                     = scala.collection.mutable.Map[Int, scala.collection.mutable.ArrayBuffer[Int]]()
    for (hh, idx) <- households.zipWithIndex do
      hh.status match
        case HhStatus.Employed(firmId, _, _) if firmLostWorkers.contains(firmId.toInt) =>
          firmToWorkers.getOrElseUpdate(firmId.toInt, scala.collection.mutable.ArrayBuffer.empty) += idx
        case _                                                                         =>
    val eduRetainSets: Map[Int, Set[Int]] = firmToWorkers.map { (firmId, indices) =>
      val sorted    = indices.sortBy(i => (-households(i).education, -households(i).skill.toDouble))
      val maxRetain = hybridRetained.getOrElse(firmId, automatedRetained.getOrElse(firmId, 0))
      firmId -> sorted.take(maxRetain).toSet
    }.toMap

    households.zipWithIndex.map { (hh, idx) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, _) if firmLostWorkers.contains(firmId.toInt) =>
          if eduRetainSets.getOrElse(firmId.toInt, Set.empty).contains(idx) then hh
          else hh.copy(status = HhStatus.Unemployed(0), lastSectorIdx = sectorIdx)
        case _                                                                                 => hh
    }

  // --- Job search ---

  /** Job search: unemployed households bid for open positions. Matching:
    * highest effective skill (skill × (1 - healthPenalty)) fills vacancies
    * first. Same-sector gets priority; cross-sector hires incur friction
    * penalty when sectoral mobility is enabled. Returns updated households and
    * cross-sector hire count.
    */
  def jobSearch(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      marketWage: PLN,
      rng: Random,
  )(using p: SimParams): JobSearchResult =
    val vacancies = computeVacancies(households, firms)
    if vacancies.isEmpty then JobSearchResult(households, 0)
    else matchWorkers(households, firms, vacancies, marketWage)

  /** Update wages for all employed households based on current market wage.
    * Individual wages are heterogeneous (sector × skill × health × immigrant
    * discount × education premium) but normalized so mean employed wage =
    * marketWage (macro consistency).
    */
  def updateWages(households: Vector[Household.State], marketWage: PLN)(using
      p: SimParams,
  ): Vector[Household.State] =
    // Compute raw relative wages for employed
    val rawWages = households.map: hh =>
      hh.status match
        case HhStatus.Employed(_, sectorIdx, _) =>
          val immigrantDiscount =
            if hh.isImmigrant && p.flags.immigration then 1.0 - p.immigration.wageDiscount.toDouble
            else 1.0
          Firm.effectiveWageMult(sectorIdx).toDouble *
            hh.skill.toDouble * (1.0 - hh.healthPenalty.toDouble) * immigrantDiscount *
            p.social.eduWagePremium(hh.education)
        case _                                  => 0.0
    val employed = households.indices.filter(i => households(i).status.isInstanceOf[HhStatus.Employed])
    val rawMean  =
      if employed.nonEmpty then employed.kahanSumBy(i => rawWages(i)) / employed.length
      else 1.0
    val scale    = if rawMean > 0 then 1.0 / rawMean else 1.0

    households.zipWithIndex.map: (hh, i) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, _) =>
          val newWage = marketWage * (rawWages(i) * scale)
          hh.copy(status = HhStatus.Employed(firmId, sectorIdx, newWage))
        case _                                       => hh

  // --- Private helpers ---

  /** Build mutable vacancy map: firmId → number of open positions. */
  private def computeVacancies(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
  )(using SimParams): scala.collection.mutable.Map[Int, Int] =
    val workerCounts = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(fid, _, _) => workerCounts(fid.toInt) += 1
        case _                            =>
    val vacancies    = scala.collection.mutable.Map[Int, Int]()
    for f <- firms if Firm.isAlive(f) do
      val needed = Firm.workerCount(f) - workerCounts(f.id.toInt)
      if needed > 0 then vacancies(f.id.toInt) = needed
    vacancies

  /** Core matching loop: rank unemployed by effective skill, assign to
    * vacancies with same-sector priority and cross-sector friction penalty.
    */
  private def matchWorkers(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      vacancies: scala.collection.mutable.Map[Int, Int],
      marketWage: PLN,
  )(using p: SimParams): JobSearchResult =
    // Rank unemployed by effective skill (skill × (1 - healthPenalty))
    val unemployedIndices = households.indices
      .filter(i => households(i).status.isInstanceOf[HhStatus.Unemployed])
      .sortBy { i =>
        val hh = households(i)
        -(hh.skill.toDouble * (1.0 - hh.healthPenalty.toDouble))
      }

    val result           = households.toArray
    var crossSectorHires = 0

    // Pre-sort vacancy firms by sector sigma descending
    val vacancyFirmsBySector   = vacancies.keys.toArray
      .groupBy(fid => firms(fid).sector.toInt)
    val vacancyFirmsByPriority = vacancies.keys.toArray
      .sortBy(fid => -p.sectorDefs(firms(fid).sector.toInt).sigma)

    for idx <- unemployedIndices do
      if vacancies.nonEmpty then
        val hh         = result(idx)
        val prevSector =
          if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt
          else firms(hh.id.toInt % firms.length).sector.toInt

        // Try same-sector first (bonus), then fall back to global priority order
        val bestFirmId = vacancyFirmsBySector
          .getOrElse(prevSector, Array.empty[Int])
          .find(fid => vacancies.contains(fid))
          .orElse(vacancyFirmsByPriority.find(fid => vacancies.contains(fid)))

        bestFirmId.foreach { fid =>
          val f              = firms(fid)
          val sectorMult     = Firm.effectiveWageMult(f.sector).toDouble
          val isCrossSector  = f.sector.toInt != prevSector
          val penalty        =
            if p.flags.sectoralMobility && isCrossSector then SectoralMobility.crossSectorWagePenalty(p.labor.frictionMatrix(prevSector)(f.sector.toInt))
            else 1.0
          val individualWage =
            marketWage * (sectorMult * hh.skill.toDouble * (1.0 - hh.healthPenalty.toDouble) * penalty *
              p.social.eduWagePremium(hh.education))
          if isCrossSector then crossSectorHires += 1
          result(idx) = hh.copy(
            status = HhStatus.Employed(FirmId(fid), f.sector, individualWage),
            lastSectorIdx = f.sector,
          )
          val remaining      = vacancies(fid) - 1
          if remaining <= 0 then vacancies.remove(fid)
          else vacancies(fid) = remaining
        }

    JobSearchResult(result.toVector, crossSectorHires)
