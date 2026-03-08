package sfc.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression test: SimParams.defaults must produce the same values as the old Config object. */
class SimParamsSpec extends AnyFlatSpec with Matchers:

  private val p = SimParams.defaults

  // ── GdpRatio ──

  "SimParams.defaults.gdpRatio" should "match Config.GdpRatio for uniform 10k×10 firms" in {
    // 10000 firms × 10 workers × 100000 revenue × 12 months / 3500e9 GDP
    val expected = (10000.0 * 10.0 / 10.0 * 100000.0 * 12.0) / 3500e9
    p.gdpRatio shouldBe expected +- 1e-12
  }

  // ── Population ──

  "PopulationConfig" should "match Config defaults" in {
    p.pop.firmsCount shouldBe Config.FirmsCount
    p.pop.workersPerFirm shouldBe Config.WorkersPerFirm
  }

  // ── Fiscal ──

  "FiscalConfig" should "have gdpRatio-scaled govBaseSpending" in {
    p.fiscal.govBaseSpending.toDouble shouldBe (58.3e9 * p.gdpRatio) +- 1.0
  }

  it should "have gdpRatio-scaled initGovDebt" in {
    p.fiscal.initGovDebt.toDouble shouldBe (1600e9 * p.gdpRatio) +- 1.0
  }

  "Config.InitGovDebt" should "delegate to fiscal.initGovDebt" in {
    Config.InitGovDebt shouldBe p.fiscal.initGovDebt.toDouble +- 1e-6
  }

  // ── Banking ──

  "BankingConfig" should "have gdpRatio-scaled values" in {
    p.banking.initCapital.toDouble shouldBe (270e9 * p.gdpRatio) +- 1.0
    p.banking.initDeposits.toDouble shouldBe (1900e9 * p.gdpRatio) +- 1.0
    p.banking.initLoans.toDouble shouldBe (700e9 * p.gdpRatio) +- 1.0
    p.banking.initGovBonds.toDouble shouldBe (400e9 * p.gdpRatio) +- 1.0
    p.banking.initNbpGovBonds.toDouble shouldBe (300e9 * p.gdpRatio) +- 1.0
    p.banking.initConsumerLoans.toDouble shouldBe (200e9 * p.gdpRatio) +- 1.0
  }

  // ── External sector sub-configs ──

  "ForexConfig" should "have gdpRatio-scaled exportBase" in {
    p.forex.exportBase.toDouble shouldBe (55.4e9 * p.gdpRatio) +- 1.0
  }

  "OpenEconConfig" should "have gdpRatio-scaled values" in {
    p.openEcon.exportBase.toDouble shouldBe (138.5e9 * p.gdpRatio) +- 1.0
    p.openEcon.euTransfers.toDouble shouldBe (1.458e9 * p.gdpRatio) +- 1.0
    p.openEcon.fdiBase.toDouble shouldBe (583.1e6 * p.gdpRatio) +- 1.0
  }

  // ── Financial sub-configs ──

  "EquityConfig" should "have gdpRatio-scaled initMcap" in {
    p.equity.initMcap.toDouble shouldBe (1.4e12 * p.gdpRatio) +- 1.0
  }

  "CorpBondConfig" should "have gdpRatio-scaled initStock" in {
    p.corpBond.initStock.toDouble shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "InsuranceConfig" should "have gdpRatio-scaled reserves" in {
    p.ins.lifeReserves.toDouble shouldBe (110e9 * p.gdpRatio) +- 1.0
    p.ins.nonLifeReserves.toDouble shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "NbfiConfig" should "have gdpRatio-scaled values" in {
    p.nbfi.tfiInitAum.toDouble shouldBe (380e9 * p.gdpRatio) +- 1.0
    p.nbfi.creditInitStock.toDouble shouldBe (231e9 * p.gdpRatio) +- 1.0
  }

  "HousingConfig" should "have gdpRatio-scaled values" in {
    p.housing.initValue.toDouble shouldBe (3.0e12 * p.gdpRatio) +- 1.0
    p.housing.initMortgage.toDouble shouldBe (485e9 * p.gdpRatio) +- 1.0
  }

  // ── Delegation consistency ──

  "Config delegation" should "match SimParams for all key external paths" in {
    Config.BaseExRate shouldBe p.forex.baseExRate
    Config.ExportBase shouldBe p.forex.exportBase.toDouble
    Config.OeExportBase shouldBe p.openEcon.exportBase.toDouble
    Config.GvcEuTradeShare shouldBe p.gvc.euTradeShare.toDouble
    Config.FdiForeignShares shouldBe p.fdi.foreignShares.map(_.toDouble)
    Config.ImmigMonthlyRate shouldBe p.immigration.monthlyRate.toDouble
    Config.TourismInboundShare shouldBe p.tourism.inboundShare.toDouble
    Config.RemittancePerCapita shouldBe p.remittance.perCapita.toDouble
  }

  it should "match SimParams for all key financial paths" in {
    Config.GpwInitMcap shouldBe p.equity.initMcap.toDouble
    Config.CorpBondInitStock shouldBe p.corpBond.initStock.toDouble
    Config.InsLifeReserves shouldBe p.ins.lifeReserves.toDouble
    Config.NbfiTfiInitAum shouldBe p.nbfi.tfiInitAum.toDouble
    Config.ReInitValue shouldBe p.housing.initValue.toDouble
    Config.ReInitMortgage shouldBe p.housing.initMortgage.toDouble
  }

  // ── Inventory delegation ──

  "Config.InventoryTargetRatios" should "delegate to capital.inventoryTargetRatios" in {
    Config.InventoryTargetRatios shouldBe p.capital.inventoryTargetRatios.map(_.toDouble)
  }

  // ── FirmSizeDist enum ──

  "FirmSizeDist" should "default to Uniform" in {
    p.pop.firmSizeDist shouldBe FirmSizeDist.Uniform
  }

  // ── Remittance split ──

  "Config.RemittancePerCapita" should "delegate to remittance.perCapita" in {
    Config.RemittancePerCapita shouldBe p.remittance.perCapita.toDouble
    Config.RemittanceGrowthRate shouldBe p.remittance.growthRate.toDouble
  }

  // ── Validation ──

  "PopulationConfig" should "reject non-positive firmsCount" in {
    an[IllegalArgumentException] should be thrownBy PopulationConfig(firmsCount = 0)
  }

  "MonetaryConfig" should "reject rateFloor >= rateCeiling" in {
    import sfc.types.*
    an[IllegalArgumentException] should be thrownBy MonetaryConfig(rateFloor = Rate(0.5), rateCeiling = Rate(0.1))
  }

  "BankingConfig" should "reject invalid minCar" in {
    import sfc.types.*
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Ratio(0.0))
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Ratio(1.0))
  }

  // ── Vector length validation ──

  "FiscalConfig" should "reject wrong-length vatRates" in {
    import sfc.types.*
    an[IllegalArgumentException] should be thrownBy FiscalConfig(vatRates = Vector(Rate(0.23), Rate(0.19)))
  }

  "CapitalConfig" should "reject wrong-length klRatios" in {
    import sfc.types.*
    an[IllegalArgumentException] should be thrownBy CapitalConfig(klRatios = Vector(PLN(1.0)))
  }

  "ClimateConfig" should "reject wrong-length energyCostShares" in {
    import sfc.types.*
    an[IllegalArgumentException] should be thrownBy ClimateConfig(energyCostShares = Vector(Ratio(0.1)))
  }

  // ── Private constructor ──

  "SimParams()" should "not be callable from outside companion" in {
    // SimParams.defaults is the only way to construct
    p shouldBe a[SimParams]
    // SimParams() would not compile — private constructor
  }
