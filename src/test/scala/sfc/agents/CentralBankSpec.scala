package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config
import sfc.types.*

class CentralBankSpec extends AnyFlatSpec with Matchers:

  // --- bondYield ---

  "CentralBankLogic.bondYield" should "include capped fiscal risk premium" in {
    // When bond market off, yield = refRate (no risk premium, no QE)
    // This test depends on Config.GovBondMarket which is true by default.
    // We test the positive path instead.
    val y = CentralBankLogic.bondYield(0.05, 0.50, 0.0, 0.0)
    // debtToGdp=0.50 > 0.40 → raw fiscalRisk = 2.0 * 0.10 = 0.20, capped at 0.10
    // yield = 0.05 + 0.005 + 0.10 - 0 - 0 = 0.155
    y shouldBe 0.155 +- 0.001
  }

  it should "increase with debtToGdp (fiscal risk premium)" in {
    val low = CentralBankLogic.bondYield(0.05, 0.30, 0.0, 0.0)
    val high = CentralBankLogic.bondYield(0.05, 0.70, 0.0, 0.0)
    high should be > low
  }

  it should "decrease with nbpBondGdpShare (QE compression)" in {
    val noQe = CentralBankLogic.bondYield(0.05, 0.50, 0.0, 0.0)
    val withQe = CentralBankLogic.bondYield(0.05, 0.50, 0.20, 0.0)
    withQe should be < noQe
  }

  it should "apply foreign demand discount when NFA > 0" in {
    val nfaNeg = CentralBankLogic.bondYield(0.05, 0.50, 0.0, -1000.0)
    val nfaPos = CentralBankLogic.bondYield(0.05, 0.50, 0.0, 1000.0)
    nfaPos should be < nfaNeg
  }

  it should "have a floor at 0" in {
    // Very high QE compression → yield should not go negative
    val y = CentralBankLogic.bondYield(0.01, 0.30, 0.50, 1000.0)
    y should be >= 0.0
  }

  it should "have zero fiscal risk when debtToGdp <= 0.40" in {
    val y1 = CentralBankLogic.bondYield(0.05, 0.30, 0.0, 0.0)
    val y2 = CentralBankLogic.bondYield(0.05, 0.40, 0.0, 0.0)
    // Both below threshold → same yield (only termPremium differs)
    y1 shouldBe y2
  }

  // --- shouldActivateQe ---

  "CentralBankLogic.shouldActivateQe" should "be true at ZLB with deflation" in {
    // Config.NbpQe defaults to true (NBP March 2020 precedent)
    CentralBankLogic.shouldActivateQe(Config.RateFloor, -0.05) shouldBe true
  }

  // --- shouldTaperQe ---

  "CentralBankLogic.shouldTaperQe" should "be true when inflation exceeds target" in {
    CentralBankLogic.shouldTaperQe(Config.NbpTargetInfl + 0.01) shouldBe true
  }

  it should "be false when inflation below target" in {
    CentralBankLogic.shouldTaperQe(Config.NbpTargetInfl - 0.01) shouldBe false
  }

  // --- executeQe ---

  "CentralBankLogic.executeQe" should "return 0 purchase when not active" in {
    val nbp = NbpState(0.05, PLN(1000.0), qeActive = false)
    val (newNbp, purchase) = CentralBankLogic.executeQe(nbp, 5000.0, 1e10)
    purchase shouldBe 0.0
    newNbp.govBondHoldings shouldBe nbp.govBondHoldings
  }

  it should "not exceed available bank bond holdings" in {
    val nbp = NbpState(0.05, PLN.Zero, qeActive = true)
    val bankBonds = 100.0
    val (_, purchase) = CentralBankLogic.executeQe(nbp, bankBonds, 1e12)
    purchase should be <= bankBonds
  }

  it should "not exceed max GDP share" in {
    val nbp = NbpState(0.05, PLN.Zero, qeActive = true)
    val annualGdp = 1000.0
    val maxByGdp = Config.NbpQeMaxGdpShare * annualGdp
    val (_, purchase) = CentralBankLogic.executeQe(nbp, 1e12, annualGdp)
    purchase should be <= maxByGdp
  }

  it should "accumulate in qeCumulative" in {
    val nbp = NbpState(0.05, PLN.Zero, qeActive = true)
    val (newNbp, purchase) = CentralBankLogic.executeQe(nbp, 1e12, 1e12)
    purchase should be > 0.0
    newNbp.qeCumulative.toDouble shouldBe purchase
  }

  // --- NbpState defaults ---

  "NbpState" should "have backward-compatible constructor" in {
    val nbp = NbpState(0.0575)
    nbp.referenceRate shouldBe 0.0575
    nbp.govBondHoldings shouldBe PLN.Zero
    nbp.qeActive shouldBe false
    nbp.qeCumulative shouldBe PLN.Zero
  }
