package sfc.engine.markets

import sfc.accounting.{BopState, ForexState}
import sfc.agents.Nbp
import sfc.config.SimParams
import sfc.types.PLN
import sfc.util.KahanSum.*

/** Open economy: trade, BoP, exchange rate, NFA dynamics.
  *
  * Structural trade model with real exchange rate elasticity (Marshall-Lerner),
  * imported intermediates via per-sector import content, and ULC-driven export
  * competitiveness. BoP identity: CA + KA + ΔReserves = 0.
  *
  * Exchange rate: floating, BoP-driven adjustment with NFA risk premium. FDI:
  * base flow with automation boost and NFA dampening. Portfolio flows: interest
  * rate differential + risk premium. NFA: CA + valuation effect (partial ER
  * pass-through on foreign assets).
  *
  * Calibration: NBP BoP statistics 2024, GUS national accounts.
  */
object OpenEconomy:

  // --- Named constants ---
  private val MonthsPerYear        = 12.0
  private val MinRealPrice         = 0.1 // floor on real price to avoid explosion
  private val FdiAutoBoost         = 0.3 // FDI uplift per unit automation ratio
  private val FdiNfaDampening      = 0.5 // FDI reduction per unit negative NFA/GDP
  private val ValuationPassThrough = 0.3 // ER pass-through to foreign asset valuation

  /** Simplified foreign sector update (when OPEN_ECON disabled). */
  def updateForeign(
      prev: ForexState,
      importConsumption: Double,
      techImports: Double,
      autoRatio: Double,
      domesticRate: Double,
      gdp: Double,
  )(using p: SimParams): ForexState =
    val techComp  = 1.0 + autoRatio * p.forex.exportAutoBoost.toDouble
    val totalImp  = importConsumption + techImports
    val exComp    = prev.exchangeRate / p.forex.baseExRate
    val exports   = p.forex.exportBase.toDouble * exComp * techComp
    val tradeBal  = exports - totalImp
    val rateDiff  = domesticRate - p.forex.foreignRate.toDouble
    val capAcct   = rateDiff * p.forex.irpSensitivity * gdp
    val bop       = tradeBal + capAcct
    val bopRatio  = if gdp > 0 then bop / gdp else 0.0
    val exRateChg = -p.forex.exRateAdjSpeed.toDouble * bopRatio
    val newRate   = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
    ForexState(newRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))

  /** Output of a single open-economy step. */
  case class Result(
      forex: ForexState,                     // updated FX state (ER, imports, exports, trade balance)
      bop: BopState,                         // full balance-of-payments snapshot
      importedIntermediates: Vector[Double], // per-sector imported intermediate goods (PLN)
      valuationEffect: Double,               // NFA valuation change from ER movement
      fxIntervention: Nbp.FxInterventionResult // NBP FX intervention result (reserves, EUR traded)
      = Nbp.FxInterventionResult(0.0, PLN.Zero, PLN.Zero),
  )

  case class StepInput(
      prevBop: BopState,
      prevForex: ForexState,
      importCons: Double,
      techImports: Double,
      autoRatio: Double,
      domesticRate: Double,
      gdp: Double,
      priceLevel: Double,
      sectorOutputs: Vector[Double],
      month: Int,
      nbpFxReserves: Double = 0.0,
      gvcExports: Option[Double] = None,
      gvcIntermImports: Option[Vector[Double]] = None,
      remittanceOutflow: Double = 0.0,
      euFundsMonthly: Double = 0.0,
      diasporaInflow: Double = 0.0,
      tourismExport: Double = 0.0,
      tourismImport: Double = 0.0,
  )

  def step(in: StepInput)(using p: SimParams): Result =
    val exports                               = computeExports(in)
    val totalExportsIncTour                   = exports + in.tourismExport
    val importedInterm                        = computeImportedIntermediates(in)
    val totalImportedInterm                   = importedInterm.kahanSum
    val totalImports                          = in.importCons + in.techImports + totalImportedInterm + in.tourismImport
    val tradeBalance                          = totalExportsIncTour - totalImports
    val (ca, primaryIncome, secondaryIncome)  = computeCurrentAccount(in, tradeBalance)
    val (capitalAccount, fdi, portfolioFlows) = computeCapitalAccount(in)
    val deltaReserves                         = -(ca + capitalAccount)
    val fxResult                              = Nbp.fxIntervention(in.prevForex.exchangeRate, in.nbpFxReserves, in.gdp, p.flags.nbpFxIntervention)
    val newExRate                             = computeExchangeRate(in, ca, capitalAccount, fxResult.erEffect)
    val valuationEffect                       = computeValuationEffect(in.prevBop, in.prevForex.exchangeRate, newExRate)
    val newNfa                                = in.prevBop.nfa.toDouble + ca + valuationEffect

    val newForex = ForexState(newExRate, PLN(totalImports), PLN(totalExportsIncTour), PLN(tradeBalance), PLN(in.techImports))

    val newBop = BopState(
      nfa = PLN(newNfa),
      foreignAssets = PLN(in.prevBop.foreignAssets.toDouble + Math.max(0.0, capitalAccount)),
      foreignLiabilities = PLN(in.prevBop.foreignLiabilities.toDouble + Math.max(0.0, -capitalAccount)),
      currentAccount = PLN(ca),
      capitalAccount = PLN(capitalAccount),
      tradeBalance = PLN(tradeBalance),
      primaryIncome = PLN(primaryIncome),
      secondaryIncome = PLN(secondaryIncome),
      fdi = PLN(fdi),
      portfolioFlows = PLN(portfolioFlows),
      reserves = PLN(in.prevBop.reserves.toDouble + deltaReserves),
      exports = PLN(totalExportsIncTour),
      totalImports = PLN(totalImports),
      importedIntermediates = PLN(totalImportedInterm),
    )

    Result(newForex, newBop, importedInterm, valuationEffect, fxResult)

  // --- Private helpers ---

  /** Export demand: foreign GDP growth × real ER elasticity × ULC effect. */
  private def computeExports(in: StepInput)(using p: SimParams): Double =
    in.gvcExports.getOrElse {
      val foreignGdpFactor = Math.pow(1.0 + p.openEcon.foreignGdpGrowth.toDouble / MonthsPerYear, in.month.toDouble)
      val ulcEffect        = 1.0 + in.autoRatio * p.openEcon.ulcExportBoost
      val realExRate       = realExchangeRateEffect(in.prevForex.exchangeRate, in.priceLevel)
      p.openEcon.exportBase.toDouble * foreignGdpFactor * realExRate * ulcEffect
    }

  /** Real ER effect on exports (Marshall-Lerner). */
  private def realExchangeRateEffect(exchangeRate: Double, priceLevel: Double)(using p: SimParams): Double =
    val nominalER = exchangeRate / p.forex.baseExRate
    val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
    Math.pow(1.0 / Math.max(MinRealPrice, realPrice), p.openEcon.exportPriceElasticity)

  /** Per-sector imported intermediates: real output × import content × ER
    * effect.
    */
  private def computeImportedIntermediates(in: StepInput)(using p: SimParams): Vector[Double] =
    in.gvcIntermImports.getOrElse {
      val nSectors      = p.sectorDefs.length
      val erNetEffect   = Math.pow(in.prevForex.exchangeRate / p.forex.baseExRate, 1.0 - p.openEcon.erElasticity)
      val importContent = p.openEcon.importContent.map(_.toDouble)
      (0 until nSectors)
        .map: s =>
          val realOutput = if in.priceLevel > 0 then in.sectorOutputs(s) / in.priceLevel else in.sectorOutputs(s)
          realOutput * importContent(s) * erNetEffect
        .toVector
    }

  /** Current account: trade balance + primary income + secondary income. */
  private def computeCurrentAccount(in: StepInput, tradeBalance: Double)(using p: SimParams): (Double, Double, Double) =
    val primaryIncome   = in.prevBop.nfa.toDouble * p.openEcon.nfaReturnRate.toDouble / MonthsPerYear
    val secondaryIncome = in.euFundsMonthly - in.remittanceOutflow + in.diasporaInflow
    val ca              = tradeBalance + primaryIncome + secondaryIncome
    (ca, primaryIncome, secondaryIncome)

  /** Capital account: FDI + portfolio flows. */
  private def computeCapitalAccount(in: StepInput)(using p: SimParams): (Double, Double, Double) =
    val nfaGdpRatio    = if in.gdp > 0 then in.prevBop.nfa.toDouble / (in.gdp * MonthsPerYear) else 0.0
    val fdi            = p.openEcon.fdiBase.toDouble * (1.0 + in.autoRatio * FdiAutoBoost) *
      (1.0 - Math.max(0.0, -nfaGdpRatio) * FdiNfaDampening)
    val portfolioFlows =
      val rateDiff    = in.domesticRate - p.forex.foreignRate.toDouble
      val riskPremium = -p.openEcon.riskPremiumSensitivity * nfaGdpRatio
      val monthlyGdp  = if in.gdp > 0 then in.gdp else 1.0
      (rateDiff + riskPremium) * p.openEcon.portfolioSensitivity * monthlyGdp
    (fdi + portfolioFlows, fdi, portfolioFlows)

  /** BoP-driven exchange rate with NFA risk premium and FX intervention. */
  private def computeExchangeRate(
      in: StepInput,
      ca: Double,
      capitalAccount: Double,
      fxErEffect: Double,
  )(using p: SimParams): Double =
    val nfaGdpRatio = if in.gdp > 0 then in.prevBop.nfa.toDouble / (in.gdp * MonthsPerYear) else 0.0
    val bopGdpRatio = if in.gdp > 0 then (ca + capitalAccount) / in.gdp else 0.0
    val nfaRisk     = p.openEcon.riskPremiumSensitivity * Math.min(0.0, nfaGdpRatio)
    val erChange    = p.forex.exRateAdjSpeed.toDouble * (-bopGdpRatio + nfaRisk) + fxErEffect
    Math.max(p.openEcon.erFloor, Math.min(p.openEcon.erCeiling, in.prevForex.exchangeRate * (1.0 + erChange)))

  /** NFA valuation effect from ER changes (partial pass-through). */
  private def computeValuationEffect(prevBop: BopState, prevExRate: Double, newExRate: Double): Double =
    val erChange = (newExRate - prevExRate) / prevExRate
    prevBop.foreignAssets.toDouble * erChange * ValuationPassThrough
