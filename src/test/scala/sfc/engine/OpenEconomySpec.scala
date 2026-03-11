package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BopState, ForexState}
import sfc.config.SimParams
import sfc.engine.markets.OpenEconomy
import sfc.types.*

class OpenEconomySpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val baseForex         = ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero)
  private val baseSectorOutputs = Vector(30000.0, 160000.0, 450000.0, 60000.0, 220000.0, 80000.0)
  private val gdp               = 1e9

  private def baseInput(
      prevBop: BopState = BopState.zero,
      prevForex: ForexState = baseForex,
      autoRatio: Double = 0.0,
      month: Int = 30,
  ) = OpenEconomy.StepInput(
    prevBop = prevBop,
    prevForex = prevForex,
    importCons = 1e7,
    techImports = 5e6,
    autoRatio = autoRatio,
    domesticRate = p.monetary.initialRate.toDouble,
    gdp = gdp,
    priceLevel = 1.0,
    sectorOutputs = baseSectorOutputs,
    month = month,
  )

  // ---- Export tests ----

  "OpenEconomy.step" should "produce positive exports" in {
    val r = OpenEconomy.step(baseInput())
    r.bop.exports.toDouble should be > 0.0
  }

  it should "increase exports with higher automation (ULC effect)" in {
    val r0 = OpenEconomy.step(baseInput(autoRatio = 0.0))
    val r1 = OpenEconomy.step(baseInput(autoRatio = 0.5))
    r1.bop.exports.toDouble should be > r0.bop.exports.toDouble
  }

  it should "increase exports with weaker PLN (higher ER)" in {
    val weakPln = baseForex.copy(exchangeRate = 5.5)
    val r0      = OpenEconomy.step(baseInput(prevForex = baseForex))
    val r1      = OpenEconomy.step(baseInput(prevForex = weakPln))
    r1.bop.exports.toDouble should be > r0.bop.exports.toDouble
  }

  // ---- Import tests ----

  it should "produce positive total imports" in {
    val r = OpenEconomy.step(baseInput())
    r.bop.totalImports.toDouble should be > 0.0
  }

  it should "produce per-sector imported intermediates" in {
    val r = OpenEconomy.step(baseInput())
    r.importedIntermediates.length shouldBe 6
    r.importedIntermediates.forall(_ >= 0.0) shouldBe true
  }

  it should "have manufacturing highest import content" in {
    val r = OpenEconomy.step(baseInput())
    r.importedIntermediates(1) should be > r.importedIntermediates(4)
  }

  // ---- BoP identity ----

  it should "satisfy BoP identity: CA + KA + deltaReserves = 0" in {
    val r      = OpenEconomy.step(baseInput())
    val bopSum = r.bop.currentAccount.toDouble + r.bop.capitalAccount.toDouble +
      (r.bop.reserves - BopState.zero.reserves).toDouble
    Math.abs(bopSum) should be < 1.0
  }

  // ---- Current account ----

  it should "include EU transfers in current account" in {
    val r = OpenEconomy.step(baseInput().copy(euFundsMonthly = p.openEcon.euTransfers.toDouble))
    r.bop.secondaryIncome.toDouble shouldBe p.openEcon.euTransfers.toDouble +- 0.01
  }

  it should "compute trade balance as exports - imports" in {
    val r          = OpenEconomy.step(baseInput())
    val expectedTb = (r.bop.exports - r.bop.totalImports).toDouble
    r.bop.tradeBalance.toDouble shouldBe expectedTb +- 0.01
  }

  // ---- NFA tracking ----

  it should "update NFA from current account" in {
    val r = OpenEconomy.step(baseInput())
    r.bop.nfa.toDouble should not be 0.0
  }

  it should "accumulate NFA across steps" in {
    val r1 = OpenEconomy.step(baseInput(month = 30))
    val r2 = OpenEconomy.step(baseInput(prevBop = r1.bop, prevForex = r1.forex, month = 31))
    r2.bop.nfa.toDouble should not be r1.bop.nfa.toDouble
  }

  // ---- Exchange rate bounds ----

  it should "keep exchange rate within floor and ceiling" in {
    val r = OpenEconomy.step(baseInput())
    r.forex.exchangeRate should be >= p.openEcon.erFloor
    r.forex.exchangeRate should be <= p.openEcon.erCeiling
  }
