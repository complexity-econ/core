package sfc.engine

import sfc.config.{Config, SECTORS, RunConfig}
import sfc.sfc.{ForexState, BopState}

case class OpenEconResult(
  forex: ForexState,
  bop: BopState,
  importedIntermediates: Vector[Double],  // per-sector import cost (6 elements)
  valuationEffect: Double                 // exact valuation effect used in NFA update
)

object OpenEconomy:

  def step(prevBop: BopState, prevForex: ForexState,
           importCons: Double, techImports: Double,
           autoRatio: Double, domesticRate: Double, gdp: Double,
           priceLevel: Double, sectorOutputs: Vector[Double],
           month: Int, rc: RunConfig): OpenEconResult =

    val nSectors = 6

    // A. Export demand (structural)
    val foreignGdpFactor = Math.pow(1.0 + Config.OeForeignGdpGrowth / 12.0, month.toDouble)
    val erCompetitiveness = if rc.isEurozone then 1.0
      else Math.pow(prevForex.exchangeRate / Config.BaseExRate, Config.OeExportPriceElasticity)
    val ulcEffect = 1.0 + autoRatio * Config.OeUlcExportBoost
    val priceCompetitiveness = if priceLevel > 0 then
      Math.pow(1.0 / priceLevel, Config.OeExportPriceElasticity)
    else 1.0
    val exports = Config.ExportBase * foreignGdpFactor * priceCompetitiveness *
      erCompetitiveness * ulcEffect

    // B. Imported intermediates (cross-border I-O)
    val erImportEffect = if rc.isEurozone then 1.0
      else Math.pow(Config.BaseExRate / prevForex.exchangeRate, Config.OeErElasticity)
    val importedInterm = (0 until nSectors).map { s =>
      sectorOutputs(s) * Config.OeImportContent(s) * erImportEffect * priceLevel
    }.toVector
    val totalImportedInterm = importedInterm.sum

    // C. Total imports
    val totalImports = importCons + techImports + totalImportedInterm

    // D. Trade balance
    val tradeBalance = exports - totalImports

    // E. Current account
    val primaryIncome = prevBop.nfa * Config.OeNfaReturnRate / 12.0
    val secondaryIncome = Config.OeEuTransfers
    val currentAccount = tradeBalance + primaryIncome + secondaryIncome

    // F. Capital account
    val nfaGdpRatio = if gdp > 0 then prevBop.nfa / (gdp * 12.0) else 0.0
    val fdi = if rc.isEurozone then
      Config.OeFdiBase * (1.0 + autoRatio * 0.5) // EUR: more FDI from tech adoption
    else
      Config.OeFdiBase * (1.0 + autoRatio * 0.3) * (1.0 - Math.max(0.0, -nfaGdpRatio) * 0.5)
    val portfolioFlows = if rc.isEurozone then 0.0  // No capital arbitrage in single currency
    else
      val rateDiff = domesticRate - Config.ForeignRate
      val riskPremium = -Config.OeRiskPremiumSensitivity * nfaGdpRatio
      val monthlyGdp = if gdp > 0 then gdp else 1.0
      (rateDiff + riskPremium) * Config.OePortfolioSensitivity * monthlyGdp
    val capitalAccount = fdi + portfolioFlows

    // G. BoP identity: CA + KA + ΔReserves = 0
    val deltaReserves = -(currentAccount + capitalAccount)

    // H. Exchange rate
    val newExRate = if rc.isEurozone then Config.BaseExRate
    else
      val bopGdpRatio = if gdp > 0 then (currentAccount + capitalAccount) / gdp else 0.0
      val nfaRisk = Config.OeRiskPremiumSensitivity * Math.min(0.0, nfaGdpRatio)
      val erChange = Config.ExRateAdjSpeed * (-bopGdpRatio + nfaRisk)
      Math.max(Config.OeErFloor,
        Math.min(Config.OeErCeiling, prevForex.exchangeRate * (1.0 + erChange)))

    // I. NFA update
    val valuationEffect = if rc.isEurozone then 0.0
    else
      val erChange = (newExRate - prevForex.exchangeRate) / prevForex.exchangeRate
      prevBop.foreignAssets * erChange * 0.3  // Partial pass-through of ER valuation
    val newNfa = prevBop.nfa + currentAccount + valuationEffect

    // Gross positions update
    val newForeignAssets = prevBop.foreignAssets + Math.max(0.0, capitalAccount)
    val newForeignLiabilities = prevBop.foreignLiabilities + Math.max(0.0, -capitalAccount)

    val newForex = ForexState(newExRate, totalImports, exports, tradeBalance, techImports)

    val newBop = BopState(
      nfa = newNfa,
      foreignAssets = newForeignAssets,
      foreignLiabilities = newForeignLiabilities,
      currentAccount = currentAccount,
      capitalAccount = capitalAccount,
      tradeBalance = tradeBalance,
      primaryIncome = primaryIncome,
      secondaryIncome = secondaryIncome,
      fdi = fdi,
      portfolioFlows = portfolioFlows,
      reserves = prevBop.reserves + deltaReserves,
      exports = exports,
      totalImports = totalImports,
      importedIntermediates = totalImportedInterm
    )

    OpenEconResult(newForex, newBop, importedInterm, valuationEffect)
