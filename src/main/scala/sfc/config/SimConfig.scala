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
  val FirmSizeMicroShare: Double = _p.pop.firmSizeMicroShare
  val FirmSizeSmallShare: Double = _p.pop.firmSizeSmallShare
  val FirmSizeMediumShare: Double = _p.pop.firmSizeMediumShare
  val FirmSizeLargeShare: Double = _p.pop.firmSizeLargeShare
  val FirmSizeLargeMax: Int = _p.pop.firmSizeLargeMax
  val RealGdp: Double = _p.pop.realGdp

  // Endogenous firm entry
  val FirmEntryEnabled: Boolean = _p.flags.firmEntry
  val FirmEntryRate: Double = _p.firm.entryRate
  val FirmEntryProfitSens: Double = _p.firm.entryProfitSens
  val FirmEntrySectorBarriers: Vector[Double] = _p.firm.entrySectorBarriers
  val FirmEntryAiThreshold: Double = _p.firm.entryAiThreshold
  val FirmEntryAiProb: Double = _p.firm.entryAiProb
  val FirmEntryStartupCash: Double = _p.firm.entryStartupCash

  // ═══ TIMELINE ═══
  val Duration: Int = _p.timeline.duration

  // ═══ FIRM PRODUCTION & COSTS ═══
  val BaseRevenue: Double = _p.firm.baseRevenue
  val OtherCosts: Double = _p.firm.otherCosts
  val AiCapex: Double = _p.firm.aiCapex
  val HybridCapex: Double = _p.firm.hybridCapex
  val AiOpex: Double = _p.firm.aiOpex
  val HybridOpex: Double = _p.firm.hybridOpex
  val AutoSkeletonCrew: Int = _p.firm.autoSkeletonCrew
  val HybridReadinessMin: Double = _p.firm.hybridReadinessMin
  val FullAiReadinessMin: Double = _p.firm.fullAiReadinessMin
  val DemandPassthrough: Double = _p.firm.demandPassthrough

  // ═══ HOUSEHOLDS ═══
  val BaseWage: Double = _p.household.baseWage
  val BaseReservationWage: Double = _p.household.baseReservationWage
  val Mpc: Double = _p.household.mpc
  val LaborSupplySteepness: Double = _p.household.laborSupplySteepness
  val WageAdjSpeed: Double = _p.household.wageAdjSpeed

  // ═══ FISCAL POLICY ═══
  val CitRate: Double = _p.fiscal.citRate
  val VatRates: Vector[Double] = _p.fiscal.vatRates
  val ExciseRates: Vector[Double] = _p.fiscal.exciseRates
  val CustomsDutyRate: Double = _p.fiscal.customsDutyRate
  val CustomsNonEuShare: Double = _p.fiscal.customsNonEuShare
  val GovBaseSpending: Double = _p.fiscal.govBaseSpending
  val GovFiscalRecyclingRate: Double = _p.fiscal.govFiscalRecyclingRate
  val GovAutoStabMult: Double = _p.fiscal.govAutoStabMult
  val GovInvestEnabled: Boolean = _p.flags.govInvest
  val GovInvestShare: Double = _p.fiscal.govInvestShare
  val GovCapitalMultiplier: Double = _p.fiscal.govCapitalMultiplier
  val GovCurrentMultiplier: Double = _p.fiscal.govCurrentMultiplier
  val GovDepreciationRate: Double = _p.fiscal.govDepreciationRate
  val GovInitCapital: Double = _p.fiscal.govInitCapital
  val EuFundsEnabled: Boolean = _p.flags.euFunds
  val EuFundsTotalEur: Double = _p.fiscal.euFundsTotalEur
  val EuFundsPeriodMonths: Int = _p.fiscal.euFundsPeriodMonths
  val EuFundsStartMonth: Int = _p.fiscal.euFundsStartMonth
  val EuFundsAlpha: Double = _p.fiscal.euFundsAlpha
  val EuFundsBeta: Double = _p.fiscal.euFundsBeta
  val EuCofinanceRate: Double = _p.fiscal.euCofinanceRate
  val EuCapitalShare: Double = _p.fiscal.euCapitalShare
  val MinWageEnabled: Boolean = _p.flags.minWage
  val MinWageAdjustMonths: Int = _p.fiscal.minWageAdjustMonths
  val MinWageInflationIndex: Boolean = _p.fiscal.minWageInflationIndex
  val MinWageTargetRatio: Double = _p.fiscal.minWageTargetRatio
  val MinWageConvergenceSpeed: Double = _p.fiscal.minWageConvergenceSpeed
  val FofConsWeights: Vector[Double] = _p.fiscal.fofConsWeights
  val FofGovWeights: Vector[Double] = _p.fiscal.fofGovWeights
  val FofExportShares: Vector[Double] = _p.fiscal.fofExportShares
  val FofInvestWeights: Vector[Double] = _p.fiscal.fofInvestWeights

  // ═══ MONETARY POLICY ═══
  val NbpInitialRate: Double = _p.monetary.initialRate
  val NbpTargetInfl: Double = _p.monetary.targetInfl
  val NbpNeutralRate: Double = _p.monetary.neutralRate
  val TaylorAlpha: Double = _p.monetary.taylorAlpha
  val TaylorBeta: Double = _p.monetary.taylorBeta
  val TaylorInertia: Double = _p.monetary.taylorInertia
  val RateFloor: Double = _p.monetary.rateFloor
  val RateCeiling: Double = _p.monetary.rateCeiling
  val NbpMaxRateChange: Double = _p.monetary.maxRateChange
  val NbpSymmetric: Boolean = _p.flags.nbpSymmetric
  val NbpNairu: Double = _p.monetary.nairu
  val TaylorDelta: Double = _p.monetary.taylorDelta
  val NbpReserveRateMult: Double = _p.monetary.reserveRateMult
  val NbpStandingFacilities: Boolean = _p.flags.nbpStandingFacilities
  val NbpDepositFacilitySpread: Double = _p.monetary.depositFacilitySpread
  val NbpLombardSpread: Double = _p.monetary.lombardSpread
  val NbpForwardGuidance: Boolean = _p.flags.nbpForwardGuidance

  // Unemployment benefits
  val GovUnempBenefitEnabled: Boolean = _p.flags.govUnempBenefit
  val GovBenefitM1to3: Double = _p.fiscal.govBenefitM1to3
  val GovBenefitM4to6: Double = _p.fiscal.govBenefitM4to6
  val GovBenefitDuration: Int = _p.fiscal.govBenefitDuration
  val GovBenefitCoverage: Double = _p.fiscal.govBenefitCoverage

  // Government bond market
  val GovBondMarket: Boolean = _p.flags.govBondMarket
  val GovFiscalRiskBeta: Double = _p.fiscal.govFiscalRiskBeta
  val GovTermPremium: Double = _p.fiscal.govTermPremium

  // QE
  val NbpQe: Boolean = _p.flags.nbpQe
  val NbpQePace: Double = _p.monetary.qePace
  val NbpQeMaxGdpShare: Double = _p.monetary.qeMaxGdpShare

  // FX intervention
  val NbpFxIntervention: Boolean = _p.flags.nbpFxIntervention
  val NbpFxBand: Double = _p.monetary.fxBand
  val NbpFxReserves: Double = _p.monetary.fxReserves
  val NbpFxMaxMonthly: Double = _p.monetary.fxMaxMonthly
  val NbpFxStrength: Double = _p.monetary.fxStrength

  // ═══ BANKING ═══
  val InitBankCapital: Double = _p.banking.initCapital
  val InitBankDeposits: Double = _p.banking.initDeposits
  val InitBankLoans: Double = _p.banking.initLoans
  val InitBankGovBonds: Double = _p.banking.initGovBonds
  val InitNbpGovBonds: Double = _p.banking.initNbpGovBonds
  val InitGovDebt: Double = _p.fiscal.initGovDebt
  val InitConsumerLoans: Double = _p.banking.initConsumerLoans
  val BaseSpread: Double = _p.banking.baseSpread
  val NplSpreadFactor: Double = _p.banking.nplSpreadFactor
  val MinCar: Double = _p.banking.minCar
  val LoanRecovery: Double = _p.banking.loanRecovery
  val BankProfitRetention: Double = _p.banking.profitRetention
  val BankFailureEnabled: Boolean = _p.flags.bankFailure
  val BankReserveReq: Double = _p.banking.reserveReq
  val BankStressThreshold: Double = _p.banking.stressThreshold
  val CreditDiagnostics: Boolean = _p.flags.creditDiagnostics

  // Consumer credit
  val CcSpread: Double = _p.household.ccSpread
  val CcMaxDti: Double = _p.household.ccMaxDti
  val CcMaxLoan: Double = _p.household.ccMaxLoan
  val CcAmortRate: Double = _p.household.ccAmortRate
  val CcNplRecovery: Double = _p.household.ccNplRecovery
  val CcEligRate: Double = _p.household.ccEligRate

  // JST
  val JstEnabled: Boolean = _p.flags.jst
  val JstPitShare: Double = _p.fiscal.jstPitShare
  val JstCitShare: Double = _p.fiscal.jstCitShare
  val JstPropertyTax: Double = _p.fiscal.jstPropertyTax
  val JstSubventionShare: Double = _p.fiscal.jstSubventionShare
  val JstDotacjeShare: Double = _p.fiscal.jstDotacjeShare
  val JstSpendingMult: Double = _p.fiscal.jstSpendingMult

  // LCR/NSFR
  val BankLcrEnabled: Boolean = _p.flags.bankLcr
  val BankLcrMin: Double = _p.banking.lcrMin
  val BankNsfrMin: Double = _p.banking.nsfrMin
  val BankDemandDepositRunoff: Double = _p.banking.demandDepositRunoff
  val BankTermDepositFrac: Double = _p.banking.termDepositFrac
  val InterbankTermStructure: Boolean = _p.flags.interbankTermStructure

  // ═══ SOCIAL SECURITY ═══
  val ZusEnabled: Boolean = _p.flags.zus
  val ZusContribRate: Double = _p.social.zusContribRate
  val ZusBasePension: Double = _p.social.zusBasePension
  val ZusScale: Double = _p.social.zusScale
  val PpkEnabled: Boolean = _p.flags.ppk
  val PpkEmployeeRate: Double = _p.social.ppkEmployeeRate
  val PpkEmployerRate: Double = _p.social.ppkEmployerRate
  val PpkBondAlloc: Double = _p.social.ppkBondAlloc
  val DemEnabled: Boolean = _p.flags.demographics
  val DemRetirementRate: Double = _p.social.demRetirementRate
  val DemWorkingAgeDecline: Double = _p.social.demWorkingAgeDecline
  val DemInitialRetirees: Int = _p.social.demInitialRetirees

  // ═══ MACROPRUDENTIAL ═══
  val MacropruEnabled: Boolean = _p.flags.macropru
  val CcybMax: Double = _p.banking.ccybMax
  val CcybActivationGap: Double = _p.banking.ccybActivationGap
  val CcybReleaseGap: Double = _p.banking.ccybReleaseGap
  val OsiiPkoBp: Double = _p.banking.osiiPkoBp
  val OsiiPekao: Double = _p.banking.osiiPekao
  val ConcentrationLimit: Double = _p.banking.concentrationLimit

  // KNF/BFG
  val P2rAddons: Vector[Double] = _p.banking.p2rAddons
  val BfgLevyRate: Double = _p.banking.bfgLevyRate
  val BailInEnabled: Boolean = _p.flags.bailIn
  val BailInDepositHaircut: Double = _p.banking.bailInDepositHaircut
  val BfgDepositGuarantee: Double = _p.banking.bfgDepositGuarantee

  // ═══ FOREIGN SECTOR ═══
  val BaseExRate: Double = _p.forex.baseExRate
  val ForeignRate: Double = _p.forex.foreignRate
  val ImportPropensity: Double = _p.forex.importPropensity
  val ExportBase: Double = _p.forex.exportBase
  val TechImportShare: Double = _p.forex.techImportShare
  val IrpSensitivity: Double = _p.forex.irpSensitivity
  val ExRateAdjSpeed: Double = _p.forex.exRateAdjSpeed
  val ExportAutoBoost: Double = _p.forex.exportAutoBoost

  // ═══ NETWORK ═══
  val NetworkK: Int = _p.firm.networkK
  val NetworkRewireP: Double = _p.firm.networkRewireP
  val DemoEffectThresh: Double = _p.firm.demoEffectThresh
  val DemoEffectBoost: Double = _p.firm.demoEffectBoost
  val SigmaLambda: Double = _p.firm.sigmaLambda
  val SigmaCapMult: Double = _p.firm.sigmaCapMult
  val RewireRho: Double = _p.firm.rewireRho

  // ═══ I-O COUPLING ═══
  val IoEnabled: Boolean = _p.flags.io
  val IoMatrix: Vector[Vector[Double]] = _p.io.matrix
  val IoColumnSums: Vector[Double] = _p.io.columnSums
  val IoScale: Double = _p.io.scale

  // ═══ OPEN ECONOMY ═══
  val OeEnabled: Boolean = _p.flags.openEcon
  val OeImportContent: Vector[Double] = _p.openEcon.importContent
  val OeErFloor: Double = _p.openEcon.erFloor
  val OeErCeiling: Double = _p.openEcon.erCeiling
  val OeExportBase: Double = _p.openEcon.exportBase
  val OeImportPushCap: Double = _p.openEcon.importPushCap
  val OeForeignGdpGrowth: Double = _p.openEcon.foreignGdpGrowth
  val OeExportPriceElasticity: Double = _p.openEcon.exportPriceElasticity
  val OeImportPriceElasticity: Double = _p.openEcon.importPriceElasticity
  val OeErElasticity: Double = _p.openEcon.erElasticity
  val OeUlcExportBoost: Double = _p.openEcon.ulcExportBoost
  val OeNfaReturnRate: Double = _p.openEcon.nfaReturnRate
  val OeEuTransfers: Double = _p.openEcon.euTransfers
  val OeFdiBase: Double = _p.openEcon.fdiBase
  val OePortfolioSensitivity: Double = _p.openEcon.portfolioSensitivity
  val OeRiskPremiumSensitivity: Double = _p.openEcon.riskPremiumSensitivity

  // ═══ GPW ═══
  val GpwEnabled: Boolean = _p.flags.gpw
  val GpwInitIndex: Double = _p.equity.initIndex
  val GpwInitMcap: Double = _p.equity.initMcap
  val GpwPeMean: Double = _p.equity.peMean
  val GpwDivYield: Double = _p.equity.divYield
  val GpwForeignShare: Double = _p.equity.foreignShare
  val GpwEquityIssuance: Boolean = _p.flags.gpwEquityIssuance
  val GpwIssuanceFrac: Double = _p.equity.issuanceFrac
  val GpwIssuanceMinSize: Int = _p.equity.issuanceMinSize
  val GpwHhEquity: Boolean = _p.flags.gpwHhEquity
  val GpwHhEquityFrac: Double = _p.equity.hhEquityFrac
  val GpwWealthEffectMpc: Double = _p.equity.wealthEffectMpc
  val GpwDividends: Boolean = _p.flags.gpwDividends
  val GpwDivTax: Double = _p.equity.divTax

  // ═══ CORPORATE BONDS ═══
  val CorpBondSpread: Double = _p.corpBond.spread
  val CorpBondInitStock: Double = _p.corpBond.initStock
  val CorpBondMinSize: Int = _p.corpBond.minSize
  val CorpBondIssuanceFrac: Double = _p.corpBond.issuanceFrac
  val CorpBondBankShare: Double = _p.corpBond.bankShare
  val CorpBondPpkShare: Double = _p.corpBond.ppkShare
  val CorpBondRecovery: Double = _p.corpBond.recovery
  val CorpBondMaturity: Double = _p.corpBond.maturity

  // ═══ INSURANCE ═══
  val InsEnabled: Boolean = _p.flags.insurance
  val InsLifeReserves: Double = _p.ins.lifeReserves
  val InsNonLifeReserves: Double = _p.ins.nonLifeReserves
  val InsGovBondShare: Double = _p.ins.govBondShare
  val InsCorpBondShare: Double = _p.ins.corpBondShare
  val InsEquityShare: Double = _p.ins.equityShare
  val InsLifePremiumRate: Double = _p.ins.lifePremiumRate
  val InsNonLifePremiumRate: Double = _p.ins.nonLifePremiumRate
  val InsLifeLossRatio: Double = _p.ins.lifeLossRatio
  val InsNonLifeLossRatio: Double = _p.ins.nonLifeLossRatio
  val InsNonLifeUnempSens: Double = _p.ins.nonLifeUnempSens
  val InsRebalanceSpeed: Double = _p.ins.rebalanceSpeed

  // ═══ NBFI ═══
  val NbfiEnabled: Boolean = _p.flags.nbfi
  val NbfiTfiInitAum: Double = _p.nbfi.tfiInitAum
  val NbfiTfiGovBondShare: Double = _p.nbfi.tfiGovBondShare
  val NbfiTfiCorpBondShare: Double = _p.nbfi.tfiCorpBondShare
  val NbfiTfiEquityShare: Double = _p.nbfi.tfiEquityShare
  val NbfiTfiInflowRate: Double = _p.nbfi.tfiInflowRate
  val NbfiTfiRebalanceSpeed: Double = _p.nbfi.tfiRebalanceSpeed
  val NbfiCreditInitStock: Double = _p.nbfi.creditInitStock
  val NbfiCreditBaseRate: Double = _p.nbfi.creditBaseRate
  val NbfiCreditRate: Double = _p.nbfi.creditRate
  val NbfiCountercyclical: Double = _p.nbfi.countercyclical
  val NbfiCreditMaturity: Double = _p.nbfi.creditMaturity
  val NbfiDefaultBase: Double = _p.nbfi.defaultBase
  val NbfiDefaultUnempSens: Double = _p.nbfi.defaultUnempSens

  // ═══ FDI ═══
  val FdiEnabled: Boolean = _p.flags.fdi
  val FdiForeignShares: Vector[Double] = _p.fdi.foreignShares
  val FdiProfitShiftRate: Double = _p.fdi.profitShiftRate
  val FdiRepatriationRate: Double = _p.fdi.repatriationRate
  val FdiMaProb: Double = _p.fdi.maProb
  val FdiMaSizeMin: Int = _p.fdi.maSizeMin

  // ═══ HOUSING ═══
  val ReEnabled: Boolean = _p.flags.re
  val ReMortgage: Boolean = _p.flags.reMortgage
  val ReHhHousing: Boolean = _p.flags.reHhHousing
  val ReInitHpi: Double = _p.housing.initHpi
  val ReInitValue: Double = _p.housing.initValue
  val ReInitMortgage: Double = _p.housing.initMortgage
  val RePriceIncomeElast: Double = _p.housing.priceIncomeElast
  val RePriceRateElast: Double = _p.housing.priceRateElast
  val RePriceReversion: Double = _p.housing.priceReversion
  val ReMortgageSpread: Double = _p.housing.mortgageSpread
  val ReMortgageMaturity: Int = _p.housing.mortgageMaturity
  val ReLtvMax: Double = _p.housing.ltvMax
  val ReOriginationRate: Double = _p.housing.originationRate
  val ReDefaultBase: Double = _p.housing.defaultBase
  val ReDefaultUnempSens: Double = _p.housing.defaultUnempSens
  val ReMortgageRecovery: Double = _p.housing.mortgageRecovery
  val ReWealthMpc: Double = _p.housing.wealthMpc
  val ReRentalYield: Double = _p.housing.rentalYield
  val ReRegional: Boolean = _p.flags.reRegional
  val ReRegionalHpi: Vector[Double] = _p.housing.regionalHpi
  val ReRegionalValueShares: Vector[Double] = _p.housing.regionalValueShares
  val ReRegionalMortgageShares: Vector[Double] = _p.housing.regionalMortgageShares
  val ReRegionalGammas: Vector[Double] = _p.housing.regionalGammas
  val ReRegionalIncomeMult: Vector[Double] = _p.housing.regionalIncomeMult

  // ═══ GVC ═══
  val GvcEnabled: Boolean = _p.flags.gvc
  val GvcEuTradeShare: Double = _p.gvc.euTradeShare
  val GvcExportShares: Vector[Double] = _p.gvc.exportShares
  val GvcDepth: Vector[Double] = _p.gvc.depth
  val GvcForeignInflation: Double = _p.gvc.foreignInflation
  val GvcForeignGdpGrowth: Double = _p.gvc.foreignGdpGrowth
  val GvcErPassthrough: Double = _p.gvc.erPassthrough
  val GvcEuErPassthrough: Double = _p.gvc.euErPassthrough
  val GvcDemandShockMonth: Int = _p.gvc.demandShockMonth
  val GvcDemandShockSize: Double = _p.gvc.demandShockSize
  val GvcDemandShockSectors: Set[Int] = _p.gvc.demandShockSectors
  val GvcDisruptionRecovery: Double = _p.gvc.disruptionRecovery

  // ═══ SECTORAL MOBILITY ═══
  val LmSectoralMobility: Boolean = _p.flags.sectoralMobility
  val LmFrictionMatrix: Vector[Vector[Double]] = _p.labor.frictionMatrix
  val LmFrictionDurationMult: Double = _p.labor.frictionDurationMult
  val LmFrictionCostMult: Double = _p.labor.frictionCostMult
  val LmVoluntarySearchProb: Double = _p.labor.voluntarySearchProb
  val LmVoluntaryWageThreshold: Double = _p.labor.voluntaryWageThreshold
  val LmVacancyWeight: Double = _p.labor.vacancyWeight
  val LmAdjacentFrictionMax: Double = _p.labor.adjacentFrictionMax

  // ═══ UNIONS ═══
  val UnionEnabled: Boolean = _p.flags.unions
  val UnionDensity: Vector[Double] = _p.labor.unionDensity
  val UnionWagePremium: Double = _p.labor.unionWagePremium
  val UnionRigidity: Double = _p.labor.unionRigidity

  // ═══ EXPECTATIONS ═══
  val ExpEnabled: Boolean = _p.flags.expectations
  val ExpLambda: Double = _p.labor.expLambda
  val ExpCredibilityInit: Double = _p.labor.expCredibilityInit
  val ExpCredibilitySpeed: Double = _p.labor.expCredibilitySpeed
  val ExpCredibilityThreshold: Double = _p.labor.expCredibilityThreshold
  val ExpWagePassthrough: Double = _p.labor.expWagePassthrough
  val ExpBondSensitivity: Double = _p.labor.expBondSensitivity

  // ═══ IMMIGRATION ═══
  val ImmigEnabled: Boolean = _p.flags.immigration
  val ImmigMonthlyRate: Double = _p.immigration.monthlyRate
  val ImmigEndogenous: Boolean = _p.flags.immigEndogenous
  val ImmigWageElasticity: Double = _p.immigration.wageElasticity
  val ImmigForeignWage: Double = _p.immigration.foreignWage
  val ImmigRemittanceRate: Double = _p.immigration.remitRate
  val ImmigReturnRate: Double = _p.immigration.returnRate
  val ImmigSectorShares: Vector[Double] = _p.immigration.sectorShares
  val ImmigSkillMean: Double = _p.immigration.skillMean
  val ImmigWageDiscount: Double = _p.immigration.wageDiscount
  val ImmigInitStock: Int = _p.immigration.initStock

  // ═══ PIT ═══
  val PitEnabled: Boolean = _p.flags.pit
  val PitRate1: Double = _p.fiscal.pitRate1
  val PitRate2: Double = _p.fiscal.pitRate2
  val PitBracket1Annual: Double = _p.fiscal.pitBracket1Annual
  val PitTaxCreditAnnual: Double = _p.fiscal.pitTaxCreditAnnual
  val PitEffectiveRate: Double = _p.fiscal.pitEffectiveRate

  // ═══ SOCIAL 800+ ═══
  val Social800Enabled: Boolean = _p.flags.social800
  val Social800Rate: Double = _p.fiscal.social800Rate
  val Social800ChildrenPerHh: Double = _p.fiscal.social800ChildrenPerHh
  val Social800ImmigrantEligible: Boolean = _p.flags.social800ImmigEligible

  // ═══ EDUCATION ═══
  val EduShares: Vector[Double] = _p.social.eduShares
  val EduSectorShares: Option[Vector[Vector[Double]]] = _p.social.eduSectorShares
  val EduWagePreemia: Vector[Double] = _p.social.eduWagePreemia
  val EduRetrainMult: Vector[Double] = _p.social.eduRetrainMult
  val EduSkillFloors: Vector[Double] = _p.social.eduSkillFloors
  val EduSkillCeilings: Vector[Double] = _p.social.eduSkillCeilings
  val EduImmigShares: Vector[Double] = _p.social.eduImmigShares

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
  val DigiDrift: Double = _p.firm.digiDrift
  val DigiInvestCost: Double = _p.firm.digiInvestCost
  val DigiInvestBoost: Double = _p.firm.digiInvestBoost
  val DigiCapexDiscount: Double = _p.firm.digiCapexDiscount
  val DigiInvestBaseProb: Double = _p.firm.digiInvestBaseProb

  // ═══ PHYSICAL CAPITAL ═══
  val PhysCapEnabled: Boolean = _p.flags.physCap
  val PhysCapKLRatios: Vector[Double] = _p.capital.klRatios
  val PhysCapDepRates: Vector[Double] = _p.capital.depRates
  val PhysCapImportShare: Double = _p.capital.importShare
  val PhysCapAdjustSpeed: Double = _p.capital.adjustSpeed
  val PhysCapProdElast: Double = _p.capital.prodElast
  val PhysCapCostReplace: Double = _p.capital.costReplace

  // ═══ INVENTORIES ═══
  val InventoryEnabled: Boolean = _p.flags.inventory
  val InventoryTargetRatios: Vector[Double] = _p.capital.inventoryTargetRatios
  val InventoryAdjustSpeed: Double = _p.capital.inventoryAdjustSpeed
  val InventoryCarryingCost: Double = _p.capital.inventoryCarryingCost
  val InventorySpoilageRates: Vector[Double] = _p.capital.inventorySpoilageRates
  val InventoryCostFraction: Double = _p.capital.inventoryCostFraction
  val InventoryLiquidationDisc: Double = _p.capital.inventoryLiquidationDisc
  val InventoryInitRatio: Double = _p.capital.inventoryInitRatio
  val InventoryCostReplace: Double = _p.capital.inventoryCostReplace

  // ═══ INFORMAL ECONOMY ═══
  val InformalEnabled: Boolean = _p.flags.informal
  val InformalSectorShares: Vector[Double] = _p.informal.sectorShares
  val InformalCitEvasion: Double = _p.informal.citEvasion
  val InformalVatEvasion: Double = _p.informal.vatEvasion
  val InformalPitEvasion: Double = _p.informal.pitEvasion
  val InformalExciseEvasion: Double = _p.informal.exciseEvasion
  val InformalUnempThreshold: Double = _p.informal.unempThreshold
  val InformalCyclicalSens: Double = _p.informal.cyclicalSens
  val InformalSmoothing: Double = _p.informal.smoothing

  // ═══ ENERGY / CLIMATE ═══
  val EnergyEnabled: Boolean = _p.flags.energy
  val EnergyCostShares: Vector[Double] = _p.climate.energyCostShares
  val EnergyCarbonIntensity: Vector[Double] = _p.climate.carbonIntensity
  val EtsBasePrice: Double = _p.climate.etsBasePrice
  val EtsPriceDrift: Double = _p.climate.etsPriceDrift
  val GreenKLRatios: Vector[Double] = _p.climate.greenKLRatios
  val GreenDepRate: Double = _p.climate.greenDepRate
  val GreenAdjustSpeed: Double = _p.climate.greenAdjustSpeed
  val GreenMaxDiscount: Double = _p.climate.greenMaxDiscount
  val GreenImportShare: Double = _p.climate.greenImportShare
  val GreenInitRatio: Double = _p.climate.greenInitRatio
  val GreenBudgetShare: Double = _p.climate.greenBudgetShare
  val EnergyCostReplace: Double = _p.climate.energyCostReplace
  val EnergyDomesticShare: Double = _p.climate.energyDomesticShare

  // ═══ REMITTANCES ═══
  val RemittanceEnabled: Boolean = _p.flags.remittance
  val RemittancePerCapita: Double = _p.remittance.perCapita
  val RemittanceErElasticity: Double = _p.remittance.erElasticity
  val RemittanceGrowthRate: Double = _p.remittance.growthRate
  val RemittanceCyclicalSens: Double = _p.remittance.cyclicalSens

  // ═══ TOURISM ═══
  val TourismEnabled: Boolean = _p.flags.tourism
  val TourismInboundShare: Double = _p.tourism.inboundShare
  val TourismOutboundShare: Double = _p.tourism.outboundShare
  val TourismErElasticity: Double = _p.tourism.erElasticity
  val TourismSeasonality: Double = _p.tourism.seasonality
  val TourismPeakMonth: Int = _p.tourism.peakMonth
  val TourismGrowthRate: Double = _p.tourism.growthRate
  val TourismShockMonth: Int = _p.tourism.shockMonth
  val TourismShockSize: Double = _p.tourism.shockSize
  val TourismShockRecovery: Double = _p.tourism.shockRecovery

  // ═══ HOUSEHOLDS (individual-mode) ═══
  val HhCount: Int = _p.household.count
  val HhSavingsMu: Double = _p.household.savingsMu
  val HhSavingsSigma: Double = _p.household.savingsSigma
  val HhDebtFraction: Double = _p.household.debtFraction
  val HhDebtMu: Double = _p.household.debtMu
  val HhDebtSigma: Double = _p.household.debtSigma
  val HhRentMean: Double = _p.household.rentMean
  val HhRentStd: Double = _p.household.rentStd
  val HhRentFloor: Double = _p.household.rentFloor
  val HhMpcAlpha: Double = _p.household.mpcAlpha
  val HhMpcBeta: Double = _p.household.mpcBeta
  val HhSkillDecayRate: Double = _p.household.skillDecayRate
  val HhScarringRate: Double = _p.household.scarringRate
  val HhScarringCap: Double = _p.household.scarringCap
  val HhScarringOnset: Int = _p.household.scarringOnset
  val HhRetrainingCost: Double = _p.household.retrainingCost
  val HhRetrainingDuration: Int = _p.household.retrainingDuration
  val HhRetrainingBaseSuccess: Double = _p.household.retrainingBaseSuccess
  val HhRetrainingProb: Double = _p.household.retrainingProb
  val HhRetrainingEnabled: Boolean = _p.household.retrainingEnabled
  val HhBankruptcyThreshold: Double = _p.household.bankruptcyThreshold
  val HhSocialK: Int = _p.household.socialK
  val HhSocialP: Double = _p.household.socialP
  val HhDebtServiceRate: Double = _p.household.debtServiceRate
  val HhBaseAmortRate: Double = _p.household.baseAmortRate
  val HhDepositSpread: Double = _p.household.depositSpread
