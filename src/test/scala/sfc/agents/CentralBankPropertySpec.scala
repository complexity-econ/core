package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.Config

class CentralBankPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- bondYield properties ---

  "CentralBankLogic.bondYield" should "be >= 0 for all inputs" in {
    forAll(genRate, Gen.choose(0.0, 2.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = CentralBankLogic.bondYield(refRate, debtToGdp, nbpBondGdpShare, nfa)
        y should be >= 0.0
    }
  }

  it should "be monotonic in debtToGdp (higher debt → higher yield)" in {
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, baseDebt: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val low = CentralBankLogic.bondYield(refRate, baseDebt, nbpBondGdpShare, nfa)
        val high = CentralBankLogic.bondYield(refRate, baseDebt + 0.10, nbpBondGdpShare, nfa)
        high should be >= (low - 1e-10)
    }
  }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in {
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.30), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, baseQe: Double, nfa: Double) =>
        val low = CentralBankLogic.bondYield(refRate, debtToGdp, baseQe + 0.10, nfa)
        val high = CentralBankLogic.bondYield(refRate, debtToGdp, baseQe, nfa)
        high should be >= (low - 1e-10)
    }
  }

  // --- executeQe properties ---

  "CentralBankLogic.executeQe" should "always return purchase >= 0" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: NbpState, bankBonds: Double, gdp: Double) =>
        val (_, purchase) = CentralBankLogic.executeQe(nbp, bankBonds, gdp)
        purchase should be >= 0.0
    }
  }

  it should "not exceed bankBondHoldings" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: NbpState, bankBonds: Double, gdp: Double) =>
        val (_, purchase) = CentralBankLogic.executeQe(nbp, bankBonds, gdp)
        purchase should be <= (bankBonds + 1e-6)
    }
  }

  it should "not exceed max GDP share limit when active" in {
    forAll(Gen.choose(0.0, 0.25), Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (rate: Double, bankBonds: Double, gdp: Double) =>
        val nbp = NbpState(rate, 0.0, qeActive = true, 0.0)
        val (newNbp, _) = CentralBankLogic.executeQe(nbp, bankBonds, gdp)
        newNbp.govBondHoldings should be <= (Config.NbpQeMaxGdpShare * gdp + 1e-6)
    }
  }

  it should "preserve bond clearing through QE (bank - purchase + nbp + purchase = total)" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: NbpState, bankBonds: Double, gdp: Double) =>
        val totalBefore = bankBonds + nbp.govBondHoldings
        val (newNbp, purchase) = CentralBankLogic.executeQe(nbp, bankBonds, gdp)
        val totalAfter = (bankBonds - purchase) + newNbp.govBondHoldings
        totalAfter shouldBe (totalBefore +- 1e-6)
    }
  }

  // --- shouldActivateQe properties ---

  "CentralBankLogic.shouldActivateQe" should "imply rate near floor when true" in {
    forAll(genRate, genInflation) { (refRate: Double, inflation: Double) =>
      if CentralBankLogic.shouldActivateQe(refRate, inflation) then
        refRate should be <= (Config.RateFloor + 0.0025)
    }
  }

  // --- shouldTaperQe properties ---

  "CentralBankLogic.shouldTaperQe" should "be consistent with inflation vs target" in {
    forAll(genInflation) { (inflation: Double) =>
      val shouldTaper = CentralBankLogic.shouldTaperQe(inflation)
      if shouldTaper then inflation should be > Config.NbpTargetInfl
    }
  }
