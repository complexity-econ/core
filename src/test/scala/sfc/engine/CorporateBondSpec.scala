package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config

class CorporateBondSpec extends AnyFlatSpec with Matchers:

  private val initState = CorporateBondMarket.initial

  "CorporateBondMarket.zero" should "return all-zero state" in {
    val z = CorporateBondMarket.zero
    z.outstanding shouldBe 0.0
    z.bankHoldings shouldBe 0.0
    z.ppkHoldings shouldBe 0.0
    z.otherHoldings shouldBe 0.0
    z.corpBondYield shouldBe 0.0
    z.lastIssuance shouldBe 0.0
    z.lastAmortization shouldBe 0.0
    z.lastCouponIncome shouldBe 0.0
    z.lastDefaultLoss shouldBe 0.0
    z.lastDefaultAmount shouldBe 0.0
  }

  "CorporateBondMarket.initial" should "have stock = CorpBondInitStock" in {
    initState.outstanding shouldBe (Config.CorpBondInitStock +- 1.0)
  }

  it should "allocate holders summing to 100%" in {
    val total = initState.bankHoldings + initState.ppkHoldings + initState.otherHoldings
    total shouldBe (initState.outstanding +- 1.0)
  }

  it should "have bank holdings = stock * CorpBondBankShare" in {
    initState.bankHoldings shouldBe (initState.outstanding * Config.CorpBondBankShare +- 1.0)
  }

  it should "have ppk holdings = stock * CorpBondPpkShare" in {
    initState.ppkHoldings shouldBe (initState.outstanding * Config.CorpBondPpkShare +- 1.0)
  }

  "computeYield" should "equal govYield + spread when nplRatio = 0" in {
    val y = CorporateBondMarket.computeYield(0.06, 0.0)
    y shouldBe (0.06 + Config.CorpBondSpread +- 0.001)
  }

  it should "widen with NPL ratio" in {
    val y0 = CorporateBondMarket.computeYield(0.06, 0.0)
    val y1 = CorporateBondMarket.computeYield(0.06, 0.05)
    y1 should be > y0
  }

  it should "cap spread at 10%" in {
    val y = CorporateBondMarket.computeYield(0.06, 1.0)
    // spread capped at 0.10
    y should be <= (0.06 + 0.10 + 0.001)
  }

  "computeCoupon" should "be proportional to holdings" in {
    val (total, bank, ppk) = CorporateBondMarket.computeCoupon(initState)
    total should be > 0.0
    bank shouldBe (initState.bankHoldings * initState.corpBondYield / 12.0 +- 0.01)
    ppk shouldBe (initState.ppkHoldings * initState.corpBondYield / 12.0 +- 0.01)
  }

  it should "return zeros for zero outstanding" in {
    val (total, bank, ppk) = CorporateBondMarket.computeCoupon(CorporateBondMarket.zero)
    total shouldBe 0.0
    bank shouldBe 0.0
    ppk shouldBe 0.0
  }

  "amortization" should "equal outstanding / maturity" in {
    val a = CorporateBondMarket.amortization(initState)
    a shouldBe (initState.outstanding / Config.CorpBondMaturity +- 0.01)
  }

  "processDefaults" should "return zeros when no defaults" in {
    val (gross, loss, bankL, ppkL) = CorporateBondMarket.processDefaults(initState, 0.0)
    gross shouldBe 0.0
    loss shouldBe 0.0
    bankL shouldBe 0.0
    ppkL shouldBe 0.0
  }

  it should "allocate defaults proportionally to holdings" in {
    val defaultAmt = 1000.0
    val (gross, loss, bankL, ppkL) = CorporateBondMarket.processDefaults(initState, defaultAmt)
    gross shouldBe defaultAmt
    loss shouldBe (defaultAmt * (1.0 - Config.CorpBondRecovery) +- 0.01)
    val bankFrac = initState.bankHoldings / initState.outstanding
    bankL shouldBe (defaultAmt * bankFrac * (1.0 - Config.CorpBondRecovery) +- 0.01)
  }

  "processIssuance" should "increase all holder buckets" in {
    val issuance = 5000.0
    val result = CorporateBondMarket.processIssuance(initState, issuance)
    result.outstanding shouldBe (initState.outstanding + issuance +- 0.01)
    result.bankHoldings shouldBe (initState.bankHoldings + issuance * Config.CorpBondBankShare +- 0.01)
    result.ppkHoldings shouldBe (initState.ppkHoldings + issuance * Config.CorpBondPpkShare +- 0.01)
    result.lastIssuance shouldBe issuance
  }

  it should "not change state for zero issuance" in {
    val result = CorporateBondMarket.processIssuance(initState, 0.0)
    result.outstanding shouldBe initState.outstanding
    result.lastIssuance shouldBe 0.0
  }

  "step" should "reduce outstanding by amortization + defaults" in {
    val prevOutstanding = initState.outstanding
    val result = CorporateBondMarket.step(initState, 0.06, 0.0, 0.0, 0.0)
    // No issuance, no defaults — only amortization reduces outstanding
    val amort = prevOutstanding / Config.CorpBondMaturity
    result.outstanding shouldBe (prevOutstanding - amort +- 1.0)
  }

  it should "satisfy SFC Identity 12: delta = issuance - amort - default" in {
    val issuance = 2000.0
    val default = 500.0
    val result = CorporateBondMarket.step(initState, 0.06, 0.02, default, issuance)
    val amort = initState.outstanding / Config.CorpBondMaturity
    val expectedChange = issuance - amort - default
    val actualChange = result.outstanding - initState.outstanding
    actualChange shouldBe (expectedChange +- 1.0)
  }

  "Config.CorpBondSpread" should "be 250 bps by default" in {
    Config.CorpBondSpread shouldBe 0.025
  }

  "Config.CorpBondRecovery" should "be 30% by default" in {
    Config.CorpBondRecovery shouldBe 0.30
  }

  "Config.CorpBondMaturity" should "be 60 months by default" in {
    Config.CorpBondMaturity shouldBe 60.0
  }

  "Config.CorpBondMinSize" should "be 50 workers by default" in {
    Config.CorpBondMinSize shouldBe 50
  }

  "computeAbsorption" should "return 1.0 when spread = base and CAR comfortable" in {
    val state = initState.copy(creditSpread = Config.CorpBondSpread)
    val absorption = CorporateBondMarket.computeAbsorption(state, 1000.0, 0.15, 0.08)
    absorption shouldBe 1.0
  }

  it should "return 1.0 for zero issuance" in {
    val absorption = CorporateBondMarket.computeAbsorption(initState, 0.0, 0.05, 0.08)
    absorption shouldBe 1.0
  }

  it should "decrease with widening spread" in {
    val normalState = initState.copy(creditSpread = Config.CorpBondSpread)
    val wideState = initState.copy(creditSpread = Config.CorpBondSpread + 0.05)
    val a1 = CorporateBondMarket.computeAbsorption(normalState, 1000.0, 0.15, 0.08)
    val a2 = CorporateBondMarket.computeAbsorption(wideState, 1000.0, 0.15, 0.08)
    a2 should be < a1
  }

  it should "return 0.3 floor at extreme spread" in {
    val extremeState = initState.copy(creditSpread = Config.CorpBondSpread + 0.15)
    val absorption = CorporateBondMarket.computeAbsorption(extremeState, 1000.0, 0.15, 0.08)
    absorption shouldBe (0.3 +- 0.001)
  }

  it should "decrease when CAR near minimum" in {
    val state = initState.copy(creditSpread = Config.CorpBondSpread)
    val a1 = CorporateBondMarket.computeAbsorption(state, 1000.0, 0.15, 0.08)
    val a2 = CorporateBondMarket.computeAbsorption(state, 1000.0, 0.09, 0.08)
    a2 should be < a1
  }

  it should "return 0.3 when CAR at or below minimum" in {
    val state = initState.copy(creditSpread = Config.CorpBondSpread)
    val absorption = CorporateBondMarket.computeAbsorption(state, 1000.0, 0.08, 0.08)
    absorption shouldBe (0.3 +- 0.001)
  }

  "BankState.car" should "include corpBondHoldings at 50% risk weight" in {
    import sfc.accounting.BankState
    val bank = BankState(totalLoans = 1000.0, nplAmount = 0.0, capital = 200.0,
      deposits = 5000.0, corpBondHoldings = 400.0)
    // RWA = 1000 + 400 * 0.5 = 1200; CAR = 200 / 1200 = 0.1667
    bank.car shouldBe (200.0 / 1200.0 +- 0.001)
  }
