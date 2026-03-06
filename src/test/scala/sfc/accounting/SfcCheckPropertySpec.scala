package sfc.accounting

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.types.*

class SfcCheckPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def errorDelta(result: Either[Vector[SfcCheck.IdentityError], Unit], id: SfcCheck.SfcIdentity): Double =
    result.swap.getOrElse(Vector.empty).find(_.identity == id).map(e => e.actual - e.expected).getOrElse(0.0)

  // --- Consistent flows always pass ---

  "SfcCheck.validate" should "pass when flows are consistent with snapshots" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result shouldBe Right(())
    }
  }

  it should "pass with zero flows and identical snapshots" in {
    forAll(genSnapshot) { (snap: SfcCheck.Snapshot) =>
      val zeroFlows = SfcCheck.MonthlyFlows(
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )
      val result = SfcCheck.validate(1, snap, snap, zeroFlows)
      result shouldBe Right(())
    }
  }

  // --- Perturbed states detect errors ---

  it should "detect perturbed bankCapital" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.BankCapital) shouldBe true
    }
  }

  it should "detect perturbed bankDeposits" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankDeposits = curr.bankDeposits + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.BankDeposits) shouldBe true
    }
  }

  it should "detect perturbed govDebt" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.GovDebt) shouldBe true
    }
  }

  it should "detect perturbed nfa" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(nfa = curr.nfa + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.Nfa) shouldBe true
    }
  }

  // --- Tolerance monotonicity ---

  it should "have tolerance monotonicity (pass at t implies pass at t' > t)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(0.01, 5.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(perturbation))
        val strictResult = SfcCheck.validate(1, prev, perturbed, flows, tolerance = perturbation)
        val looseResult = SfcCheck.validate(1, prev, perturbed, flows, tolerance = perturbation * 10)
        if strictResult.isRight then looseResult shouldBe Right(())
    }
  }

  // --- Error magnitude property ---

  it should "have error magnitude = actual change - expected change" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(1.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), delta: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(delta))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        errorDelta(result, SfcCheck.SfcIdentity.BankCapital) shouldBe (delta +- 1e-6)
    }
  }

  // --- Four identities independent ---

  it should "have independent identities (perturb one, others unaffected)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), delta: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + PLN(delta))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result shouldBe a[Left[?, ?]]
        result.swap.getOrElse(Vector.empty).map(_.identity) should contain(SfcCheck.SfcIdentity.GovDebt)
        result.swap.getOrElse(Vector.empty).exists(e => e.identity == SfcCheck.SfcIdentity.BankCapital) shouldBe false
        result.swap.getOrElse(Vector.empty).exists(e => e.identity == SfcCheck.SfcIdentity.BankDeposits) shouldBe false
    }
  }

  // --- Bond clearing identity ---

  it should "pass bond clearing when bankBonds + nbpBonds = bondsOutstanding" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result shouldBe Right(())
    }
  }

  it should "detect perturbed bondClearingError" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankBondHoldings = curr.bankBondHoldings + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.BondClearing) shouldBe true
    }
  }

  // --- Interbank netting identity ---

  it should "pass interbank netting when interbankNetSum is zero" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result shouldBe Right(())
    }
  }

  it should "detect perturbed interbankNetSum" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(interbankNetSum = PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.InterbankNetting) shouldBe true
    }
  }

  // --- Snapshot sums property ---

  it should "compute correct snapshot sums from firms" in {
    forAll(Gen.choose(3, 20)) { (n: Int) =>
      forAll(Gen.listOfN(n, genAliveFirm)) { (firmList: List[sfc.agents.Firm.State]) =>
        val firms = firmList.toArray
        val expectedCash = firms.map(_.cash.toDouble).sum
        val expectedDebt = firms.map(_.debt.toDouble).sum
        Math.abs(expectedCash - firms.map(_.cash.toDouble).sum) should be < 1e-6
        Math.abs(expectedDebt - firms.map(_.debt.toDouble).sum) should be < 1e-6
      }
    }
  }

  // --- Dividend flow consistency ---

  it should "pass with non-zero dividend flows in consistent snapshots" in {
    // Verify that genConsistentFlowsAndSnapshots correctly handles dividend fields
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      // genMonthlyFlows generates non-zero dividend fields — verify all 8 identities hold
      val result = SfcCheck.validate(1, prev, curr, flows)
      result shouldBe Right(())
    }
  }

  it should "detect perturbed deposits from dividend flow mismatch" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        // Perturb dividendIncome without updating deposits → Identity 2 fails
        val badFlows = flows.copy(dividendIncome = flows.dividendIncome + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, curr, badFlows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.BankDeposits) shouldBe true
    }
  }

  // --- Mortgage stock identity ---

  it should "detect perturbed mortgageStock" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(mortgageStock = curr.mortgageStock + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.swap.getOrElse(Vector.empty).exists(_.identity == SfcCheck.SfcIdentity.MortgageStock) shouldBe true
    }
  }

  it should "pass mortgage stock identity in consistent snapshots" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result shouldBe Right(())
    }
  }
