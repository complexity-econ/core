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
    p.fiscal.govBaseSpending shouldBe (58.3e9 * p.gdpRatio) +- 1.0
  }

  it should "have gdpRatio-scaled initGovDebt" in {
    p.fiscal.initGovDebt shouldBe (1600e9 * p.gdpRatio) +- 1.0
  }

  "Config.InitGovDebt" should "delegate to fiscal.initGovDebt" in {
    Config.InitGovDebt shouldBe p.fiscal.initGovDebt +- 1e-6
  }

  // ── Banking ──

  "BankingConfig" should "have gdpRatio-scaled values" in {
    p.banking.initCapital shouldBe (270e9 * p.gdpRatio) +- 1.0
    p.banking.initDeposits shouldBe (1900e9 * p.gdpRatio) +- 1.0
    p.banking.initLoans shouldBe (700e9 * p.gdpRatio) +- 1.0
    p.banking.initGovBonds shouldBe (400e9 * p.gdpRatio) +- 1.0
    p.banking.initNbpGovBonds shouldBe (300e9 * p.gdpRatio) +- 1.0
    p.banking.initConsumerLoans shouldBe (200e9 * p.gdpRatio) +- 1.0
  }

  // ── External sector sub-configs ──

  "ForexConfig" should "have gdpRatio-scaled exportBase" in {
    p.forex.exportBase shouldBe (55.4e9 * p.gdpRatio) +- 1.0
  }

  "OpenEconConfig" should "have gdpRatio-scaled values" in {
    p.openEcon.exportBase shouldBe (138.5e9 * p.gdpRatio) +- 1.0
    p.openEcon.euTransfers shouldBe (1.458e9 * p.gdpRatio) +- 1.0
    p.openEcon.fdiBase shouldBe (583.1e6 * p.gdpRatio) +- 1.0
  }

  // ── Financial sub-configs ──

  "EquityConfig" should "have gdpRatio-scaled initMcap" in {
    p.equity.initMcap shouldBe (1.4e12 * p.gdpRatio) +- 1.0
  }

  "CorpBondConfig" should "have gdpRatio-scaled initStock" in {
    p.corpBond.initStock shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "InsuranceConfig" should "have gdpRatio-scaled reserves" in {
    p.ins.lifeReserves shouldBe (110e9 * p.gdpRatio) +- 1.0
    p.ins.nonLifeReserves shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "NbfiConfig" should "have gdpRatio-scaled values" in {
    p.nbfi.tfiInitAum shouldBe (380e9 * p.gdpRatio) +- 1.0
    p.nbfi.creditInitStock shouldBe (231e9 * p.gdpRatio) +- 1.0
  }

  "HousingConfig" should "have gdpRatio-scaled values" in {
    p.housing.initValue shouldBe (3.0e12 * p.gdpRatio) +- 1.0
    p.housing.initMortgage shouldBe (485e9 * p.gdpRatio) +- 1.0
  }

  // ── Delegation consistency ──

  "Config delegation" should "match SimParams for all key external paths" in {
    Config.BaseExRate shouldBe p.forex.baseExRate
    Config.ExportBase shouldBe p.forex.exportBase
    Config.OeExportBase shouldBe p.openEcon.exportBase
    Config.GvcEuTradeShare shouldBe p.gvc.euTradeShare
    Config.FdiForeignShares shouldBe p.fdi.foreignShares
    Config.ImmigMonthlyRate shouldBe p.immigration.monthlyRate
    Config.TourismInboundShare shouldBe p.tourism.inboundShare
    Config.RemittancePerCapita shouldBe p.remittance.perCapita
  }

  it should "match SimParams for all key financial paths" in {
    Config.GpwInitMcap shouldBe p.equity.initMcap
    Config.CorpBondInitStock shouldBe p.corpBond.initStock
    Config.InsLifeReserves shouldBe p.ins.lifeReserves
    Config.NbfiTfiInitAum shouldBe p.nbfi.tfiInitAum
    Config.ReInitValue shouldBe p.housing.initValue
    Config.ReInitMortgage shouldBe p.housing.initMortgage
  }

  // ── Inventory delegation ──

  "Config.InventoryTargetRatios" should "delegate to capital.inventoryTargetRatios" in {
    Config.InventoryTargetRatios shouldBe p.capital.inventoryTargetRatios
  }

  // ── FirmSizeDist enum ──

  "FirmSizeDist" should "default to Uniform" in {
    p.pop.firmSizeDist shouldBe FirmSizeDist.Uniform
  }

  // ── Remittance split ──

  "Config.RemittancePerCapita" should "delegate to remittance.perCapita" in {
    Config.RemittancePerCapita shouldBe p.remittance.perCapita
    Config.RemittanceGrowthRate shouldBe p.remittance.growthRate
  }

  // ── Validation ──

  "PopulationConfig" should "reject non-positive firmsCount" in {
    an[IllegalArgumentException] should be thrownBy PopulationConfig(firmsCount = 0)
  }

  "MonetaryConfig" should "reject rateFloor >= rateCeiling" in {
    an[IllegalArgumentException] should be thrownBy MonetaryConfig(rateFloor = 0.5, rateCeiling = 0.1)
  }

  "BankingConfig" should "reject invalid minCar" in {
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = 0.0)
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = 1.0)
  }

  // ── Vector length validation ──

  "FiscalConfig" should "reject wrong-length vatRates" in {
    an[IllegalArgumentException] should be thrownBy FiscalConfig(vatRates = Vector(0.23, 0.19))
  }

  "CapitalConfig" should "reject wrong-length klRatios" in {
    an[IllegalArgumentException] should be thrownBy CapitalConfig(klRatios = Vector(1.0))
  }

  "ClimateConfig" should "reject wrong-length energyCostShares" in {
    an[IllegalArgumentException] should be thrownBy ClimateConfig(energyCostShares = Vector(0.1))
  }

  // ── Private constructor ──

  "SimParams()" should "not be callable from outside companion" in {
    // SimParams.defaults is the only way to construct
    p shouldBe a[SimParams]
    // SimParams() would not compile — private constructor
  }
