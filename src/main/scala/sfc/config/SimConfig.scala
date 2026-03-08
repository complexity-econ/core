package sfc.config

import sfc.types.*

/** Network topology selection for comparative experiments. */
enum Topology:
  case Ws, Er, Ba, Lattice

/** Runtime configuration: values that depend on CLI arguments. Passed through runSingle and Simulation.step.
  */
case class RunConfig(
  nSeeds: Int,
  outputPrefix: String,
)

/** 4-to-6 sector definition with heterogeneous sigma (CES elasticity of substitution). sigma affects: decision
  * threshold, automation efficiency, CAPEX costs.
  */
case class SectorDef(
  name: String,
  share: Ratio, // Share of firm population (GUS BAEL 2024)
  sigma: Double, // CES elasticity of substitution
  wageMultiplier: Double, // Sector wage multiplier vs national average
  revenueMultiplier: Double,
  aiCapexMultiplier: Double,
  hybridCapexMultiplier: Double,
  baseDigitalReadiness: Ratio, // Central tendency of digitalReadiness
  hybridRetainFrac: Ratio, // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
)

/** SectorDefs — default 6-sector structure from SimParams. */
val SectorDefs: Vector[SectorDef] = SimParams.DefaultSectorDefs

/** Network topology — reads TOPOLOGY env var for backward compatibility. */
val TOPOLOGY: Topology = sys.env.getOrElse("TOPOLOGY", "ws").toLowerCase match
  case "er"      => Topology.Er
  case "ba"      => Topology.Ba
  case "lattice" => Topology.Lattice
  case _         => Topology.Ws

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise data. */
object FirmSizeDistribution:
  import scala.util.Random

  def draw(rng: Random): Int = Config.FirmSizeDist match
    case FirmSizeDist.Gus =>
      val r = rng.nextDouble()
      if r < Config.FirmSizeMicroShare then rng.between(1, 10)
      else if r < Config.FirmSizeMicroShare + Config.FirmSizeSmallShare then rng.between(10, 50)
      else if r < 1.0 - Config.FirmSizeLargeShare then rng.between(50, 250)
      else rng.between(250, Config.FirmSizeLargeMax + 1)
    case FirmSizeDist.Uniform => Config.WorkersPerFirm

/** Delegation shim: all vals delegate to `SimParams.defaults`. In PR 2 this entire object is deleted and replaced by
  * `using SimParams`.
  */
@deprecated("Use SimParams via `using` context parameter", "PR 2")
object Config:

  private val _p: SimParams = SimParams.defaults

  // ═══ POPULATION & FIRM STRUCTURE ═══
  val FirmsCount: Int = _p.pop.firmsCount
  val WorkersPerFirm: Int = _p.pop.workersPerFirm
  var TotalPopulation: Int = _p.pop.firmsCount * _p.pop.workersPerFirm
  def setTotalPopulation(n: Int): Unit = TotalPopulation = n
  val FirmSizeDist: FirmSizeDist = _p.pop.firmSizeDist
  val FirmSizeMicroShare: Double = _p.pop.firmSizeMicroShare.toDouble
  val FirmSizeSmallShare: Double = _p.pop.firmSizeSmallShare.toDouble
  val FirmSizeMediumShare: Double = _p.pop.firmSizeMediumShare.toDouble
  val FirmSizeLargeShare: Double = _p.pop.firmSizeLargeShare.toDouble
  val FirmSizeLargeMax: Int = _p.pop.firmSizeLargeMax
  val RealGdp: Double = _p.pop.realGdp.toDouble

  // Endogenous firm entry
  val FirmEntryEnabled: Boolean = _p.flags.firmEntry
  val FirmEntryRate: Double = _p.firm.entryRate.toDouble
  val FirmEntryProfitSens: Double = _p.firm.entryProfitSens
  val FirmEntrySectorBarriers: Vector[Double] = _p.firm.entrySectorBarriers
  val FirmEntryAiThreshold: Double = _p.firm.entryAiThreshold.toDouble
  val FirmEntryAiProb: Double = _p.firm.entryAiProb.toDouble
  val FirmEntryStartupCash: Double = _p.firm.entryStartupCash.toDouble

  // ═══ TIMELINE ═══
  val Duration: Int = _p.timeline.duration

  // ═══ FIRM PRODUCTION & COSTS ═══
  val BaseRevenue: Double = _p.firm.baseRevenue.toDouble
  val OtherCosts: Double = _p.firm.otherCosts.toDouble
  val AiCapex: Double = _p.firm.aiCapex.toDouble
  val HybridCapex: Double = _p.firm.hybridCapex.toDouble
  val AiOpex: Double = _p.firm.aiOpex.toDouble
  val HybridOpex: Double = _p.firm.hybridOpex.toDouble
  val AutoSkeletonCrew: Int = _p.firm.autoSkeletonCrew
  val HybridReadinessMin: Double = _p.firm.hybridReadinessMin.toDouble
  val FullAiReadinessMin: Double = _p.firm.fullAiReadinessMin.toDouble
  val DemandPassthrough: Double = _p.firm.demandPassthrough.toDouble

  // ═══ HOUSEHOLDS ═══
  val BaseWage: Double = _p.household.baseWage.toDouble
  val BaseReservationWage: Double = _p.household.baseReservationWage.toDouble
  val Mpc: Double = _p.household.mpc.toDouble
  val LaborSupplySteepness: Double = _p.household.laborSupplySteepness
  val WageAdjSpeed: Double = _p.household.wageAdjSpeed.toDouble

  // ═══ FISCAL POLICY ═══
  val CitRate: Double = _p.fiscal.citRate.toDouble
  val VatRates: Vector[Double] = _p.fiscal.vatRates.map(_.toDouble)
  val ExciseRates: Vector[Double] = _p.fiscal.exciseRates.map(_.toDouble)
  val CustomsDutyRate: Double = _p.fiscal.customsDutyRate.toDouble
  val CustomsNonEuShare: Double = _p.fiscal.customsNonEuShare.toDouble
  val GovBaseSpending: Double = _p.fiscal.govBaseSpending.toDouble
  val GovFiscalRecyclingRate: Double = _p.fiscal.govFiscalRecyclingRate.toDouble
  val GovAutoStabMult: Double = _p.fiscal.govAutoStabMult
  val GovInvestEnabled: Boolean = _p.flags.govInvest
  val GovInvestShare: Double = _p.fiscal.govInvestShare.toDouble
  val GovCapitalMultiplier: Double = _p.fiscal.govCapitalMultiplier
  val GovCurrentMultiplier: Double = _p.fiscal.govCurrentMultiplier
  val GovDepreciationRate: Double = _p.fiscal.govDepreciationRate.toDouble
  val GovInitCapital: Double = _p.fiscal.govInitCapital.toDouble
  val EuFundsEnabled: Boolean = _p.flags.euFunds
  val EuFundsTotalEur: Double = _p.fiscal.euFundsTotalEur
  val EuFundsPeriodMonths: Int = _p.fiscal.euFundsPeriodMonths
  val EuFundsStartMonth: Int = _p.fiscal.euFundsStartMonth
  val EuFundsAlpha: Double = _p.fiscal.euFundsAlpha
  val EuFundsBeta: Double = _p.fiscal.euFundsBeta
  val EuCofinanceRate: Double = _p.fiscal.euCofinanceRate.toDouble
  val EuCapitalShare: Double = _p.fiscal.euCapitalShare.toDouble
  val MinWageEnabled: Boolean = _p.flags.minWage
  val MinWageAdjustMonths: Int = _p.fiscal.minWageAdjustMonths
  val MinWageInflationIndex: Boolean = _p.fiscal.minWageInflationIndex
  val MinWageTargetRatio: Double = _p.fiscal.minWageTargetRatio.toDouble
  val MinWageConvergenceSpeed: Double = _p.fiscal.minWageConvergenceSpeed.toDouble
  val FofConsWeights: Vector[Double] = _p.fiscal.fofConsWeights.map(_.toDouble)
  val FofGovWeights: Vector[Double] = _p.fiscal.fofGovWeights.map(_.toDouble)
  val FofExportShares: Vector[Double] = _p.fiscal.fofExportShares.map(_.toDouble)
  val FofInvestWeights: Vector[Double] = _p.fiscal.fofInvestWeights.map(_.toDouble)

  // ═══ MONETARY POLICY ═══
  val NbpInitialRate: Double = _p.monetary.initialRate.toDouble
  val NbpTargetInfl: Double = _p.monetary.targetInfl.toDouble
  val NbpNeutralRate: Double = _p.monetary.neutralRate.toDouble
  val TaylorAlpha: Double = _p.monetary.taylorAlpha
  val TaylorBeta: Double = _p.monetary.taylorBeta
  val TaylorInertia: Double = _p.monetary.taylorInertia.toDouble
  val RateFloor: Double = _p.monetary.rateFloor.toDouble
  val RateCeiling: Double = _p.monetary.rateCeiling.toDouble
  val NbpMaxRateChange: Double = _p.monetary.maxRateChange.toDouble
  val NbpSymmetric: Boolean = _p.flags.nbpSymmetric
  val NbpNairu: Double = _p.monetary.nairu.toDouble
  val TaylorDelta: Double = _p.monetary.taylorDelta
  val NbpReserveRateMult: Double = _p.monetary.reserveRateMult.toDouble
  val NbpStandingFacilities: Boolean = _p.flags.nbpStandingFacilities
  val NbpDepositFacilitySpread: Double = _p.monetary.depositFacilitySpread.toDouble
  val NbpLombardSpread: Double = _p.monetary.lombardSpread.toDouble
  val NbpForwardGuidance: Boolean = _p.flags.nbpForwardGuidance

  // Unemployment benefits
  val GovUnempBenefitEnabled: Boolean = _p.flags.govUnempBenefit
  val GovBenefitM1to3: Double = _p.fiscal.govBenefitM1to3.toDouble
  val GovBenefitM4to6: Double = _p.fiscal.govBenefitM4to6.toDouble
  val GovBenefitDuration: Int = _p.fiscal.govBenefitDuration
  val GovBenefitCoverage: Double = _p.fiscal.govBenefitCoverage.toDouble

  // Government bond market
  val GovBondMarket: Boolean = _p.flags.govBondMarket
  val GovFiscalRiskBeta: Double = _p.fiscal.govFiscalRiskBeta
  val GovTermPremium: Double = _p.fiscal.govTermPremium.toDouble

  // QE
  val NbpQe: Boolean = _p.flags.nbpQe
  val NbpQePace: Double = _p.monetary.qePace.toDouble
  val NbpQeMaxGdpShare: Double = _p.monetary.qeMaxGdpShare.toDouble

  // FX intervention
  val NbpFxIntervention: Boolean = _p.flags.nbpFxIntervention
  val NbpFxBand: Double = _p.monetary.fxBand.toDouble
  val NbpFxReserves: Double = _p.monetary.fxReserves.toDouble
  val NbpFxMaxMonthly: Double = _p.monetary.fxMaxMonthly.toDouble
  val NbpFxStrength: Double = _p.monetary.fxStrength.toDouble

  // ═══ BANKING ═══
  val InitBankCapital: Double = _p.banking.initCapital.toDouble
  val InitBankDeposits: Double = _p.banking.initDeposits.toDouble
  val InitBankLoans: Double = _p.banking.initLoans.toDouble
  val InitBankGovBonds: Double = _p.banking.initGovBonds.toDouble
  val InitNbpGovBonds: Double = _p.banking.initNbpGovBonds.toDouble
  val InitGovDebt: Double = _p.fiscal.initGovDebt.toDouble
  val InitConsumerLoans: Double = _p.banking.initConsumerLoans.toDouble
  val BaseSpread: Double = _p.banking.baseSpread.toDouble
  val NplSpreadFactor: Double = _p.banking.nplSpreadFactor
  val MinCar: Double = _p.banking.minCar.toDouble
  val LoanRecovery: Double = _p.banking.loanRecovery.toDouble
  val BankProfitRetention: Double = _p.banking.profitRetention.toDouble
  val BankFailureEnabled: Boolean = _p.flags.bankFailure
  val BankReserveReq: Double = _p.banking.reserveReq.toDouble
  val BankStressThreshold: Double = _p.banking.stressThreshold.toDouble
  val CreditDiagnostics: Boolean = _p.flags.creditDiagnostics

  // Consumer credit
  val CcSpread: Double = _p.household.ccSpread.toDouble
  val CcMaxDti: Double = _p.household.ccMaxDti.toDouble
  val CcMaxLoan: Double = _p.household.ccMaxLoan.toDouble
  val CcAmortRate: Double = _p.household.ccAmortRate.toDouble
  val CcNplRecovery: Double = _p.household.ccNplRecovery.toDouble
  val CcEligRate: Double = _p.household.ccEligRate.toDouble

  // JST
  val JstEnabled: Boolean = _p.flags.jst
  val JstPitShare: Double = _p.fiscal.jstPitShare.toDouble
  val JstCitShare: Double = _p.fiscal.jstCitShare.toDouble
  val JstPropertyTax: Double = _p.fiscal.jstPropertyTax.toDouble
  val JstSubventionShare: Double = _p.fiscal.jstSubventionShare.toDouble
  val JstDotacjeShare: Double = _p.fiscal.jstDotacjeShare.toDouble
  val JstSpendingMult: Double = _p.fiscal.jstSpendingMult

  // LCR/NSFR
  val BankLcrEnabled: Boolean = _p.flags.bankLcr
  val BankLcrMin: Double = _p.banking.lcrMin
  val BankNsfrMin: Double = _p.banking.nsfrMin
  val BankDemandDepositRunoff: Double = _p.banking.demandDepositRunoff.toDouble
  val BankTermDepositFrac: Double = _p.banking.termDepositFrac.toDouble
  val InterbankTermStructure: Boolean = _p.flags.interbankTermStructure

  // ═══ SOCIAL SECURITY ═══
  val ZusEnabled: Boolean = _p.flags.zus
  val ZusContribRate: Double = _p.social.zusContribRate.toDouble
  val ZusBasePension: Double = _p.social.zusBasePension.toDouble
  val ZusScale: Double = _p.social.zusScale
  val PpkEnabled: Boolean = _p.flags.ppk
  val PpkEmployeeRate: Double = _p.social.ppkEmployeeRate.toDouble
  val PpkEmployerRate: Double = _p.social.ppkEmployerRate.toDouble
  val PpkBondAlloc: Double = _p.social.ppkBondAlloc.toDouble
  val DemEnabled: Boolean = _p.flags.demographics
  val DemRetirementRate: Double = _p.social.demRetirementRate.toDouble
  val DemWorkingAgeDecline: Double = _p.social.demWorkingAgeDecline.toDouble
  val DemInitialRetirees: Int = _p.social.demInitialRetirees

  // ═══ MACROPRUDENTIAL ═══
  val MacropruEnabled: Boolean = _p.flags.macropru
  val CcybMax: Double = _p.banking.ccybMax.toDouble
  val CcybActivationGap: Double = _p.banking.ccybActivationGap.toDouble
  val CcybReleaseGap: Double = _p.banking.ccybReleaseGap
  val OsiiPkoBp: Double = _p.banking.osiiPkoBp.toDouble
  val OsiiPekao: Double = _p.banking.osiiPekao.toDouble
  val ConcentrationLimit: Double = _p.banking.concentrationLimit.toDouble

  // KNF/BFG
  val P2rAddons: Vector[Double] = _p.banking.p2rAddons.map(_.toDouble)
  val BfgLevyRate: Double = _p.banking.bfgLevyRate.toDouble
  val BailInEnabled: Boolean = _p.flags.bailIn
  val BailInDepositHaircut: Double = _p.banking.bailInDepositHaircut.toDouble
  val BfgDepositGuarantee: Double = _p.banking.bfgDepositGuarantee.toDouble

  // ═══ FOREIGN SECTOR ═══
  val BaseExRate: Double = _p.forex.baseExRate
  val ForeignRate: Double = _p.forex.foreignRate.toDouble
  val ImportPropensity: Double = _p.forex.importPropensity.toDouble
  val ExportBase: Double = _p.forex.exportBase.toDouble
  val TechImportShare: Double = _p.forex.techImportShare.toDouble
  val IrpSensitivity: Double = _p.forex.irpSensitivity
  val ExRateAdjSpeed: Double = _p.forex.exRateAdjSpeed.toDouble
  val ExportAutoBoost: Double = _p.forex.exportAutoBoost.toDouble

  // ═══ NETWORK ═══
  val NetworkK: Int = _p.firm.networkK
  val NetworkRewireP: Double = _p.firm.networkRewireP.toDouble
  val DemoEffectThresh: Double = _p.firm.demoEffectThresh.toDouble
  val DemoEffectBoost: Double = _p.firm.demoEffectBoost.toDouble
  val SigmaLambda: Double = _p.firm.sigmaLambda
  val SigmaCapMult: Double = _p.firm.sigmaCapMult
  val RewireRho: Double = _p.firm.rewireRho.toDouble

  // ═══ I-O COUPLING ═══
  val IoEnabled: Boolean = _p.flags.io
  val IoMatrix: Vector[Vector[Double]] = _p.io.matrix
  val IoColumnSums: Vector[Double] = _p.io.columnSums
  val IoScale: Double = _p.io.scale.toDouble

  // ═══ OPEN ECONOMY ═══
  val OeEnabled: Boolean = _p.flags.openEcon
  val OeImportContent: Vector[Double] = _p.openEcon.importContent.map(_.toDouble)
  val OeErFloor: Double = _p.openEcon.erFloor
  val OeErCeiling: Double = _p.openEcon.erCeiling
  val OeExportBase: Double = _p.openEcon.exportBase.toDouble
  val OeImportPushCap: Double = _p.openEcon.importPushCap.toDouble
  val OeForeignGdpGrowth: Double = _p.openEcon.foreignGdpGrowth.toDouble
  val OeExportPriceElasticity: Double = _p.openEcon.exportPriceElasticity
  val OeImportPriceElasticity: Double = _p.openEcon.importPriceElasticity
  val OeErElasticity: Double = _p.openEcon.erElasticity
  val OeUlcExportBoost: Double = _p.openEcon.ulcExportBoost
  val OeNfaReturnRate: Double = _p.openEcon.nfaReturnRate.toDouble
  val OeEuTransfers: Double = _p.openEcon.euTransfers.toDouble
  val OeFdiBase: Double = _p.openEcon.fdiBase.toDouble
  val OePortfolioSensitivity: Double = _p.openEcon.portfolioSensitivity
  val OeRiskPremiumSensitivity: Double = _p.openEcon.riskPremiumSensitivity

  // ═══ GPW ═══
  val GpwEnabled: Boolean = _p.flags.gpw
  val GpwInitIndex: Double = _p.equity.initIndex
  val GpwInitMcap: Double = _p.equity.initMcap.toDouble
  val GpwPeMean: Double = _p.equity.peMean
  val GpwDivYield: Double = _p.equity.divYield.toDouble
  val GpwForeignShare: Double = _p.equity.foreignShare.toDouble
  val GpwEquityIssuance: Boolean = _p.flags.gpwEquityIssuance
  val GpwIssuanceFrac: Double = _p.equity.issuanceFrac.toDouble
  val GpwIssuanceMinSize: Int = _p.equity.issuanceMinSize
  val GpwHhEquity: Boolean = _p.flags.gpwHhEquity
  val GpwHhEquityFrac: Double = _p.equity.hhEquityFrac.toDouble
  val GpwWealthEffectMpc: Double = _p.equity.wealthEffectMpc.toDouble
  val GpwDividends: Boolean = _p.flags.gpwDividends
  val GpwDivTax: Double = _p.equity.divTax.toDouble

  // ═══ CORPORATE BONDS ═══
  val CorpBondSpread: Double = _p.corpBond.spread.toDouble
  val CorpBondInitStock: Double = _p.corpBond.initStock.toDouble
  val CorpBondMinSize: Int = _p.corpBond.minSize
  val CorpBondIssuanceFrac: Double = _p.corpBond.issuanceFrac.toDouble
  val CorpBondBankShare: Double = _p.corpBond.bankShare.toDouble
  val CorpBondPpkShare: Double = _p.corpBond.ppkShare.toDouble
  val CorpBondRecovery: Double = _p.corpBond.recovery.toDouble
  val CorpBondMaturity: Double = _p.corpBond.maturity

  // ═══ INSURANCE ═══
  val InsEnabled: Boolean = _p.flags.insurance
  val InsLifeReserves: Double = _p.ins.lifeReserves.toDouble
  val InsNonLifeReserves: Double = _p.ins.nonLifeReserves.toDouble
  val InsGovBondShare: Double = _p.ins.govBondShare.toDouble
  val InsCorpBondShare: Double = _p.ins.corpBondShare.toDouble
  val InsEquityShare: Double = _p.ins.equityShare.toDouble
  val InsLifePremiumRate: Double = _p.ins.lifePremiumRate.toDouble
  val InsNonLifePremiumRate: Double = _p.ins.nonLifePremiumRate.toDouble
  val InsLifeLossRatio: Double = _p.ins.lifeLossRatio.toDouble
  val InsNonLifeLossRatio: Double = _p.ins.nonLifeLossRatio.toDouble
  val InsNonLifeUnempSens: Double = _p.ins.nonLifeUnempSens
  val InsRebalanceSpeed: Double = _p.ins.rebalanceSpeed.toDouble

  // ═══ NBFI ═══
  val NbfiEnabled: Boolean = _p.flags.nbfi
  val NbfiTfiInitAum: Double = _p.nbfi.tfiInitAum.toDouble
  val NbfiTfiGovBondShare: Double = _p.nbfi.tfiGovBondShare.toDouble
  val NbfiTfiCorpBondShare: Double = _p.nbfi.tfiCorpBondShare.toDouble
  val NbfiTfiEquityShare: Double = _p.nbfi.tfiEquityShare.toDouble
  val NbfiTfiInflowRate: Double = _p.nbfi.tfiInflowRate.toDouble
  val NbfiTfiRebalanceSpeed: Double = _p.nbfi.tfiRebalanceSpeed.toDouble
  val NbfiCreditInitStock: Double = _p.nbfi.creditInitStock.toDouble
  val NbfiCreditBaseRate: Double = _p.nbfi.creditBaseRate.toDouble
  val NbfiCreditRate: Double = _p.nbfi.creditRate.toDouble
  val NbfiCountercyclical: Double = _p.nbfi.countercyclical
  val NbfiCreditMaturity: Double = _p.nbfi.creditMaturity
  val NbfiDefaultBase: Double = _p.nbfi.defaultBase.toDouble
  val NbfiDefaultUnempSens: Double = _p.nbfi.defaultUnempSens

  // ═══ FDI ═══
  val FdiEnabled: Boolean = _p.flags.fdi
  val FdiForeignShares: Vector[Double] = _p.fdi.foreignShares.map(_.toDouble)
  val FdiProfitShiftRate: Double = _p.fdi.profitShiftRate.toDouble
  val FdiRepatriationRate: Double = _p.fdi.repatriationRate.toDouble
  val FdiMaProb: Double = _p.fdi.maProb.toDouble
  val FdiMaSizeMin: Int = _p.fdi.maSizeMin

  // ═══ HOUSING ═══
  val ReEnabled: Boolean = _p.flags.re
  val ReMortgage: Boolean = _p.flags.reMortgage
  val ReHhHousing: Boolean = _p.flags.reHhHousing
  val ReInitHpi: Double = _p.housing.initHpi
  val ReInitValue: Double = _p.housing.initValue.toDouble
  val ReInitMortgage: Double = _p.housing.initMortgage.toDouble
  val RePriceIncomeElast: Double = _p.housing.priceIncomeElast
  val RePriceRateElast: Double = _p.housing.priceRateElast
  val RePriceReversion: Double = _p.housing.priceReversion.toDouble
  val ReMortgageSpread: Double = _p.housing.mortgageSpread.toDouble
  val ReMortgageMaturity: Int = _p.housing.mortgageMaturity
  val ReLtvMax: Double = _p.housing.ltvMax.toDouble
  val ReOriginationRate: Double = _p.housing.originationRate.toDouble
  val ReDefaultBase: Double = _p.housing.defaultBase.toDouble
  val ReDefaultUnempSens: Double = _p.housing.defaultUnempSens
  val ReMortgageRecovery: Double = _p.housing.mortgageRecovery.toDouble
  val ReWealthMpc: Double = _p.housing.wealthMpc.toDouble
  val ReRentalYield: Double = _p.housing.rentalYield.toDouble
  val ReRegional: Boolean = _p.flags.reRegional
  val ReRegionalHpi: Vector[Double] = _p.housing.regionalHpi
  val ReRegionalValueShares: Vector[Double] = _p.housing.regionalValueShares.map(_.toDouble)
  val ReRegionalMortgageShares: Vector[Double] = _p.housing.regionalMortgageShares.map(_.toDouble)
  val ReRegionalGammas: Vector[Double] = _p.housing.regionalGammas.map(_.toDouble)
  val ReRegionalIncomeMult: Vector[Double] = _p.housing.regionalIncomeMult

  // ═══ GVC ═══
  val GvcEnabled: Boolean = _p.flags.gvc
  val GvcEuTradeShare: Double = _p.gvc.euTradeShare.toDouble
  val GvcExportShares: Vector[Double] = _p.gvc.exportShares.map(_.toDouble)
  val GvcDepth: Vector[Double] = _p.gvc.depth.map(_.toDouble)
  val GvcForeignInflation: Double = _p.gvc.foreignInflation.toDouble
  val GvcForeignGdpGrowth: Double = _p.gvc.foreignGdpGrowth.toDouble
  val GvcErPassthrough: Double = _p.gvc.erPassthrough.toDouble
  val GvcEuErPassthrough: Double = _p.gvc.euErPassthrough.toDouble
  val GvcDemandShockMonth: Int = _p.gvc.demandShockMonth
  val GvcDemandShockSize: Double = _p.gvc.demandShockSize.toDouble
  val GvcDemandShockSectors: Set[Int] = _p.gvc.demandShockSectors
  val GvcDisruptionRecovery: Double = _p.gvc.disruptionRecovery.toDouble

  // ═══ SECTORAL MOBILITY ═══
  val LmSectoralMobility: Boolean = _p.flags.sectoralMobility
  val LmFrictionMatrix: Vector[Vector[Double]] = _p.labor.frictionMatrix
  val LmFrictionDurationMult: Double = _p.labor.frictionDurationMult
  val LmFrictionCostMult: Double = _p.labor.frictionCostMult.toDouble
  val LmVoluntarySearchProb: Double = _p.labor.voluntarySearchProb.toDouble
  val LmVoluntaryWageThreshold: Double = _p.labor.voluntaryWageThreshold.toDouble
  val LmVacancyWeight: Double = _p.labor.vacancyWeight
  val LmAdjacentFrictionMax: Double = _p.labor.adjacentFrictionMax.toDouble

  // ═══ UNIONS ═══
  val UnionEnabled: Boolean = _p.flags.unions
  val UnionDensity: Vector[Double] = _p.labor.unionDensity.map(_.toDouble)
  val UnionWagePremium: Double = _p.labor.unionWagePremium.toDouble
  val UnionRigidity: Double = _p.labor.unionRigidity.toDouble

  // ═══ EXPECTATIONS ═══
  val ExpEnabled: Boolean = _p.flags.expectations
  val ExpLambda: Double = _p.labor.expLambda.toDouble
  val ExpCredibilityInit: Double = _p.labor.expCredibilityInit.toDouble
  val ExpCredibilitySpeed: Double = _p.labor.expCredibilitySpeed.toDouble
  val ExpCredibilityThreshold: Double = _p.labor.expCredibilityThreshold.toDouble
  val ExpWagePassthrough: Double = _p.labor.expWagePassthrough.toDouble
  val ExpBondSensitivity: Double = _p.labor.expBondSensitivity.toDouble

  // ═══ IMMIGRATION ═══
  val ImmigEnabled: Boolean = _p.flags.immigration
  val ImmigMonthlyRate: Double = _p.immigration.monthlyRate.toDouble
  val ImmigEndogenous: Boolean = _p.flags.immigEndogenous
  val ImmigWageElasticity: Double = _p.immigration.wageElasticity
  val ImmigForeignWage: Double = _p.immigration.foreignWage.toDouble
  val ImmigRemittanceRate: Double = _p.immigration.remitRate.toDouble
  val ImmigReturnRate: Double = _p.immigration.returnRate.toDouble
  val ImmigSectorShares: Vector[Double] = _p.immigration.sectorShares.map(_.toDouble)
  val ImmigSkillMean: Double = _p.immigration.skillMean.toDouble
  val ImmigWageDiscount: Double = _p.immigration.wageDiscount.toDouble
  val ImmigInitStock: Int = _p.immigration.initStock

  // ═══ PIT ═══
  val PitEnabled: Boolean = _p.flags.pit
  val PitRate1: Double = _p.fiscal.pitRate1.toDouble
  val PitRate2: Double = _p.fiscal.pitRate2.toDouble
  val PitBracket1Annual: Double = _p.fiscal.pitBracket1Annual.toDouble
  val PitTaxCreditAnnual: Double = _p.fiscal.pitTaxCreditAnnual.toDouble
  val PitEffectiveRate: Double = _p.fiscal.pitEffectiveRate.toDouble

  // ═══ SOCIAL 800+ ═══
  val Social800Enabled: Boolean = _p.flags.social800
  val Social800Rate: Double = _p.fiscal.social800Rate.toDouble
  val Social800ChildrenPerHh: Double = _p.fiscal.social800ChildrenPerHh
  val Social800ImmigrantEligible: Boolean = _p.flags.social800ImmigEligible

  // ═══ EDUCATION ═══
  val EduShares: Vector[Double] = _p.social.eduShares.map(_.toDouble)
  val EduSectorShares: Option[Vector[Vector[Double]]] = _p.social.eduSectorShares
  val EduWagePreemia: Vector[Double] = _p.social.eduWagePreemia
  val EduRetrainMult: Vector[Double] = _p.social.eduRetrainMult
  val EduSkillFloors: Vector[Double] = _p.social.eduSkillFloors.map(_.toDouble)
  val EduSkillCeilings: Vector[Double] = _p.social.eduSkillCeilings.map(_.toDouble)
  val EduImmigShares: Vector[Double] = _p.social.eduImmigShares.map(_.toDouble)

  def drawEducation(sectorIdx: Int, rng: scala.util.Random): Int =
    _p.social.drawEducation(sectorIdx, rng)

  def drawImmigrantEducation(rng: scala.util.Random): Int =
    _p.social.drawImmigrantEducation(rng)

  def eduWagePremium(education: Int): Double =
    _p.social.eduWagePremium(education)

  def eduRetrainMultiplier(education: Int): Double =
    _p.social.eduRetrainMultiplier(education)

  def eduSkillRange(education: Int): (Double, Double) =
    _p.social.eduSkillRange(education)

  // ═══ DIGITALIZATION ═══
  val DigiDrift: Double = _p.firm.digiDrift.toDouble
  val DigiInvestCost: Double = _p.firm.digiInvestCost.toDouble
  val DigiInvestBoost: Double = _p.firm.digiInvestBoost.toDouble
  val DigiCapexDiscount: Double = _p.firm.digiCapexDiscount.toDouble
  val DigiInvestBaseProb: Double = _p.firm.digiInvestBaseProb.toDouble

  // ═══ PHYSICAL CAPITAL ═══
  val PhysCapEnabled: Boolean = _p.flags.physCap
  val PhysCapKLRatios: Vector[Double] = _p.capital.klRatios.map(_.toDouble)
  val PhysCapDepRates: Vector[Double] = _p.capital.depRates.map(_.toDouble)
  val PhysCapImportShare: Double = _p.capital.importShare.toDouble
  val PhysCapAdjustSpeed: Double = _p.capital.adjustSpeed.toDouble
  val PhysCapProdElast: Double = _p.capital.prodElast.toDouble
  val PhysCapCostReplace: Double = _p.capital.costReplace.toDouble

  // ═══ INVENTORIES ═══
  val InventoryEnabled: Boolean = _p.flags.inventory
  val InventoryTargetRatios: Vector[Double] = _p.capital.inventoryTargetRatios.map(_.toDouble)
  val InventoryAdjustSpeed: Double = _p.capital.inventoryAdjustSpeed.toDouble
  val InventoryCarryingCost: Double = _p.capital.inventoryCarryingCost.toDouble
  val InventorySpoilageRates: Vector[Double] = _p.capital.inventorySpoilageRates.map(_.toDouble)
  val InventoryCostFraction: Double = _p.capital.inventoryCostFraction.toDouble
  val InventoryLiquidationDisc: Double = _p.capital.inventoryLiquidationDisc.toDouble
  val InventoryInitRatio: Double = _p.capital.inventoryInitRatio.toDouble
  val InventoryCostReplace: Double = _p.capital.inventoryCostReplace.toDouble

  // ═══ INFORMAL ECONOMY ═══
  val InformalEnabled: Boolean = _p.flags.informal
  val InformalSectorShares: Vector[Double] = _p.informal.sectorShares.map(_.toDouble)
  val InformalCitEvasion: Double = _p.informal.citEvasion.toDouble
  val InformalVatEvasion: Double = _p.informal.vatEvasion.toDouble
  val InformalPitEvasion: Double = _p.informal.pitEvasion.toDouble
  val InformalExciseEvasion: Double = _p.informal.exciseEvasion.toDouble
  val InformalUnempThreshold: Double = _p.informal.unempThreshold.toDouble
  val InformalCyclicalSens: Double = _p.informal.cyclicalSens.toDouble
  val InformalSmoothing: Double = _p.informal.smoothing.toDouble

  // ═══ ENERGY / CLIMATE ═══
  val EnergyEnabled: Boolean = _p.flags.energy
  val EnergyCostShares: Vector[Double] = _p.climate.energyCostShares.map(_.toDouble)
  val EnergyCarbonIntensity: Vector[Double] = _p.climate.carbonIntensity
  val EtsBasePrice: Double = _p.climate.etsBasePrice
  val EtsPriceDrift: Double = _p.climate.etsPriceDrift.toDouble
  val GreenKLRatios: Vector[Double] = _p.climate.greenKLRatios.map(_.toDouble)
  val GreenDepRate: Double = _p.climate.greenDepRate.toDouble
  val GreenAdjustSpeed: Double = _p.climate.greenAdjustSpeed.toDouble
  val GreenMaxDiscount: Double = _p.climate.greenMaxDiscount.toDouble
  val GreenImportShare: Double = _p.climate.greenImportShare.toDouble
  val GreenInitRatio: Double = _p.climate.greenInitRatio.toDouble
  val GreenBudgetShare: Double = _p.climate.greenBudgetShare.toDouble
  val EnergyCostReplace: Double = _p.climate.energyCostReplace.toDouble
  val EnergyDomesticShare: Double = _p.climate.energyDomesticShare.toDouble

  // ═══ REMITTANCES ═══
  val RemittanceEnabled: Boolean = _p.flags.remittance
  val RemittancePerCapita: Double = _p.remittance.perCapita.toDouble
  val RemittanceErElasticity: Double = _p.remittance.erElasticity
  val RemittanceGrowthRate: Double = _p.remittance.growthRate.toDouble
  val RemittanceCyclicalSens: Double = _p.remittance.cyclicalSens.toDouble

  // ═══ TOURISM ═══
  val TourismEnabled: Boolean = _p.flags.tourism
  val TourismInboundShare: Double = _p.tourism.inboundShare.toDouble
  val TourismOutboundShare: Double = _p.tourism.outboundShare.toDouble
  val TourismErElasticity: Double = _p.tourism.erElasticity
  val TourismSeasonality: Double = _p.tourism.seasonality.toDouble
  val TourismPeakMonth: Int = _p.tourism.peakMonth
  val TourismGrowthRate: Double = _p.tourism.growthRate.toDouble
  val TourismShockMonth: Int = _p.tourism.shockMonth
  val TourismShockSize: Double = _p.tourism.shockSize.toDouble
  val TourismShockRecovery: Double = _p.tourism.shockRecovery.toDouble

  // ═══ HOUSEHOLDS (individual-mode) ═══
  val HhCount: Int = _p.household.count
  val HhSavingsMu: Double = _p.household.savingsMu
  val HhSavingsSigma: Double = _p.household.savingsSigma
  val HhDebtFraction: Double = _p.household.debtFraction.toDouble
  val HhDebtMu: Double = _p.household.debtMu
  val HhDebtSigma: Double = _p.household.debtSigma
  val HhRentMean: Double = _p.household.rentMean.toDouble
  val HhRentStd: Double = _p.household.rentStd.toDouble
  val HhRentFloor: Double = _p.household.rentFloor.toDouble
  val HhMpcAlpha: Double = _p.household.mpcAlpha
  val HhMpcBeta: Double = _p.household.mpcBeta
  val HhSkillDecayRate: Double = _p.household.skillDecayRate.toDouble
  val HhScarringRate: Double = _p.household.scarringRate.toDouble
  val HhScarringCap: Double = _p.household.scarringCap.toDouble
  val HhScarringOnset: Int = _p.household.scarringOnset
  val HhRetrainingCost: Double = _p.household.retrainingCost.toDouble
  val HhRetrainingDuration: Int = _p.household.retrainingDuration
  val HhRetrainingBaseSuccess: Double = _p.household.retrainingBaseSuccess.toDouble
  val HhRetrainingProb: Double = _p.household.retrainingProb.toDouble
  val HhRetrainingEnabled: Boolean = _p.household.retrainingEnabled
  val HhBankruptcyThreshold: Double = _p.household.bankruptcyThreshold
  val HhSocialK: Int = _p.household.socialK
  val HhSocialP: Double = _p.household.socialP.toDouble
  val HhDebtServiceRate: Double = _p.household.debtServiceRate.toDouble
  val HhBaseAmortRate: Double = _p.household.baseAmortRate.toDouble
  val HhDepositSpread: Double = _p.household.depositSpread.toDouble
