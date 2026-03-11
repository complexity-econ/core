package sfc.engine.markets

import sfc.config.SimParams
import sfc.types.*
import sfc.util.KahanSum.*

/** Global Value Chain trade: per-sector exports/imports with partner
  * differentiation.
  *
  * Models Poland's deep external sector as 12 foreign firm proxies (6 sectors ×
  * 2 partners: EU, non-EU). Each proxy carries base export demand, import
  * supply, price index, and supply-chain disruption state.
  *
  * Export demand: foreign GDP growth × real ER elasticity × automation boost ×
  * (1 − disruption). Import demand: sector output × GVC depth × differentiated
  * ER pass-through (EU vs non-EU). Disruptions recover at configurable rate.
  *
  * Calibration: GUS foreign trade data 2024, NBP BoP statistics, OECD TiVA.
  */
object GvcTrade:

  // --- Named constants ---
  private val NumPartners           = 2
  private val AutomationExportBoost = 0.15 // export uplift per unit automation ratio
  private val MinErEffect           = 0.1  // floor on ER pass-through multiplier
  private val MonthsPerYear         = 12.0

  case class ForeignFirm(
      id: Int,
      sectorId: Int,
      partnerId: Int, // 0=EU, 1=Non-EU
      baseExportDemand: PLN,
      baseImportSupply: PLN,
      priceIndex: Double,
      disruption: Ratio,
  )

  case class State(
      foreignFirms: Vector[ForeignFirm],
      totalExports: PLN = PLN.Zero,
      totalIntermImports: PLN = PLN.Zero,
      sectorExports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
      sectorImports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
      disruptionIndex: Ratio = Ratio.Zero,
      foreignPriceIndex: Double = 1.0,
      tradeConcentration: Ratio = Ratio.Zero,
      exportDemandShockMag: Ratio = Ratio.Zero,
      importCostIndex: Double = 1.0,
  )

  def zero: State = State(Vector.empty)

  def initial(using p: SimParams): State =
    val euShare       = p.gvc.euTradeShare.toDouble
    val nonEuShare    = 1.0 - euShare
    val partnerShares = Vector(euShare, nonEuShare)
    val exportBase    = p.openEcon.exportBase.toDouble
    val exportShares  = p.gvc.exportShares.map(_.toDouble)
    val depths        = p.gvc.depth.map(_.toDouble)

    val firms = for
      s  <- (0 until 6).toVector
      pi <- (0 until NumPartners).toVector
    yield
      val ps = partnerShares(pi)
      ForeignFirm(
        id = s * NumPartners + pi,
        sectorId = s,
        partnerId = pi,
        baseExportDemand = PLN(exportBase * exportShares(s) * ps),
        baseImportSupply = PLN(exportBase * depths(s) * ps),
        priceIndex = 1.0,
        disruption = Ratio.Zero,
      )

    State(firms, foreignPriceIndex = 1.0, tradeConcentration = Ratio(euShare * euShare + nonEuShare * nonEuShare))

  case class StepInput(
      prev: State,
      sectorOutputs: Vector[Double],
      priceLevel: Double,
      exchangeRate: Double,
      autoRatio: Double,
      month: Int,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val nSectors = in.prev.sectorExports.size

    // 1. Evolve foreign price
    val monthlyForeignInflation = p.gvc.foreignInflation.toDouble / MonthsPerYear
    val newForeignPrice         = in.prev.foreignPriceIndex * (1.0 + monthlyForeignInflation)

    // 2. Apply demand shock + recover disruptions
    val shockActive = p.gvc.demandShockMonth > 0 && in.month >= p.gvc.demandShockMonth
    val shockMag    = if shockActive then p.gvc.demandShockSize.toDouble else 0.0

    val updatedFirms = in.prev.foreignFirms.map: ff =>
      val afterShock =
        if shockActive && in.month == p.gvc.demandShockMonth &&
          p.gvc.demandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * (1.0 - p.gvc.demandShockSize.toDouble))
        else ff

      val newDisruption = Ratio(afterShock.disruption.toDouble * (1.0 - p.gvc.disruptionRecovery.toDouble))
      val newPrice      = afterShock.priceIndex * (1.0 + monthlyForeignInflation)
      afterShock.copy(disruption = newDisruption, priceIndex = newPrice)

    // 3. Foreign GDP growth factor
    val foreignGdpFactor = Math.pow(1.0 + p.gvc.foreignGdpGrowth.toDouble / MonthsPerYear, in.month.toDouble)

    // 4. Real exchange rate effect (same formula as OpenEconomy)
    val realExRateEffect =
      val nominalER = in.exchangeRate / p.forex.baseExRate
      val realPrice = if in.priceLevel > 0 && nominalER > 0 then in.priceLevel / nominalER else 1.0
      Math.pow(1.0 / Math.max(MinErEffect, realPrice), p.openEcon.exportPriceElasticity)

    // 5. Sector-specific exports
    val sectorExports = (0 until nSectors)
      .map: s =>
        val sectorFirms     = updatedFirms.filter(_.sectorId == s)
        val demand          = sectorFirms.kahanSumBy(_.baseExportDemand.toDouble) * foreignGdpFactor
        val sectorAutoBoost = 1.0 + in.autoRatio * AutomationExportBoost
        val avgDisruption   =
          if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
          else 0.0
        PLN(demand * realExRateEffect * sectorAutoBoost * (1.0 - avgDisruption))
      .toVector
    val totalExports  = PLN(sectorExports.map(_.toDouble).kahanSum)

    // 6. Sector-specific intermediate imports
    val depths             = p.gvc.depth.map(_.toDouble)
    val sectorImports      = (0 until nSectors)
      .map: s =>
        val realOutput    =
          if in.priceLevel > 0 then in.sectorOutputs(s) / in.priceLevel
          else in.sectorOutputs(s)
        val baseDemand    = realOutput * depths(s)
        val sectorFirms   = updatedFirms.filter(_.sectorId == s)
        val erEffect      =
          val totalSupply = sectorFirms.kahanSumBy(_.baseImportSupply.toDouble)
          if totalSupply > 0 then
            val euWeight    = sectorFirms.filter(_.partnerId == 0).kahanSumBy(_.baseImportSupply.toDouble) / totalSupply
            val nonEuWeight = 1.0 - euWeight
            val erDeviation = in.exchangeRate / p.forex.baseExRate - 1.0
            1.0 + euWeight * erDeviation * p.gvc.euErPassthrough.toDouble +
              nonEuWeight * erDeviation * p.gvc.erPassthrough.toDouble
          else 1.0
        val avgDisruption =
          if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
          else 0.0
        PLN(baseDemand * Math.max(MinErEffect, erEffect) * (1.0 - avgDisruption))
      .toVector
    val totalIntermImports = PLN(sectorImports.map(_.toDouble).kahanSum)

    // 7. Metrics
    val weightedDisruption =
      if updatedFirms.nonEmpty then
        val totalDemand = updatedFirms.kahanSumBy(_.baseExportDemand.toDouble)
        if totalDemand > 0 then updatedFirms.kahanSumBy(ff => ff.disruption.toDouble * ff.baseExportDemand.toDouble) / totalDemand
        else 0.0
      else 0.0

    val euShare = p.gvc.euTradeShare.toDouble

    State(
      foreignFirms = updatedFirms,
      totalExports = totalExports,
      totalIntermImports = totalIntermImports,
      sectorExports = sectorExports,
      sectorImports = sectorImports,
      disruptionIndex = Ratio(weightedDisruption),
      foreignPriceIndex = newForeignPrice,
      tradeConcentration = Ratio(euShare * euShare + (1.0 - euShare) * (1.0 - euShare)),
      exportDemandShockMag = Ratio(shockMag),
      importCostIndex = newForeignPrice,
    )
