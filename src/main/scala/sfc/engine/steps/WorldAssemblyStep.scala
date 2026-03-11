package sfc.engine.steps

import sfc.accounting.*
import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.*
import sfc.engine.mechanisms.{FirmEntry, SectoralMobility}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object WorldAssemblyStep:

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
      s9: BankUpdateStep.Output,
  )

  case class Output(
      newWorld: World,
      finalFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      sfcResult: Sfc.SfcResult,
  )

  def run(in: Input, rng: Random)(using p: SimParams): Output =
    // GPW: finalize equity state with HH equity wealth
    val totalHhEquityWealth = in.s9.reassignedHouseholds.kahanSumBy(_.equityWealth.toDouble)

    val equityAfterStep = in.s7.equityAfterIssuance.copy(
      hhEquityWealth = PLN(totalHhEquityWealth),
      lastWealthEffect = PLN.Zero,
      lastDomesticDividends = PLN(in.s7.netDomesticDividends),
      lastForeignDividends = PLN(in.s7.foreignDividendOutflow),
      lastDividendTax = PLN(in.s7.dividendTax),
    )

    // Flow-of-funds residual
    val fofResidual = {
      val totalFirmRev   = (0 until p.sectorDefs.length).map { s =>
        in.s2.living
          .filter(_.sector.toInt == s)
          .kahanSumBy(f => (Firm.computeCapacity(f) * (in.s4.sectorMults(s) * in.w.priceLevel)).toDouble)
      }.kahanSum
      val adjustedDemand = in.s4.sectorMults.indices.map { s =>
        in.s4.sectorCap(s) * in.s4.sectorMults(s) * in.w.priceLevel
      }.kahanSum
      totalFirmRev - adjustedDemand
    }

    // Informal economy: aggregate metrics and next-month cyclical adjustment (#45)
    val taxEvasionLoss         =
      if p.flags.informal then
        in.s5.sumCitEvasion + (in.s9.vat - in.s9.vatAfterEvasion) + (in.s3.pitRevenue - in.s9.pitAfterEvasion) + (in.s9.exciseRevenue - in.s9.exciseAfterEvasion)
      else 0.0
    val informalEmployed       = if p.flags.informal then in.s2.employed.toDouble * in.s9.effectiveShadowShare else 0.0
    val newInformalCyclicalAdj = if p.flags.informal then
      val unemp  = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
      val target = Math.max(0.0, unemp - p.informal.unempThreshold.toDouble) * p.informal.cyclicalSens.toDouble
      in.w.mechanisms.informalCyclicalAdj * p.informal.smoothing.toDouble + target * (1.0 - p.informal.smoothing.toDouble)
    else 0.0

    // Pre-compute values surfaced on World for SimOutput
    val aliveBanksForObs      = in.s9.finalBankingSector.banks.filterNot(_.failed)
    val depositFacilityUsage  = aliveBanksForObs
      .filter(_.reservesAtNbp > PLN.Zero)
      .kahanSumBy(_.reservesAtNbp.toDouble)
    val etsPrice              =
      if p.flags.energy then p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, in.s1.m.toDouble)
      else 0.0
    val monthInYear           = ((in.s1.m - 1) % 12) + 1
    val tourismSeasonalFactor =
      1.0 + p.tourism.seasonality.toDouble * Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)

    val newEffectiveShadowShare =
      if p.flags.informal then
        p.fiscal.fofConsWeights
          .map(_.toDouble)
          .zip(p.informal.sectorShares.map(_.toDouble))
          .map((cw, ss) => cw * Math.min(1.0, ss + newInformalCyclicalAdj))
          .sum: Double
      else 0.0

    val newW = World(
      month = in.s1.m,
      inflation = Rate(in.s7.newInfl),
      priceLevel = in.s7.newPrice,
      gdpProxy = in.s7.gdp,
      currentSigmas = in.s7.newSigmas,
      totalPopulation = in.w.totalPopulation + in.s5.netMigration,
      gov = in.s9.newGovWithYield.copy(
        minWageLevel = PLN(in.s1.baseMinWage),
        minWagePriceLevel = in.s1.updatedMinWagePriceLevel,
      ),
      nbp = in.s8.postFxNbp,
      bank = in.s9.resolvedBank,
      bankingSector = in.s9.finalBankingSector,
      forex = in.s8.newForex,
      bop = in.s8.newBop,
      hhAgg = in.s9.finalHhAgg,
      households = in.s9.reassignedHouseholds,
      monetaryAgg = in.s9.monAgg,
      social = SocialState(
        jst = in.s9.newJst,
        zus = in.s2.newZus,
        ppk = in.s9.finalPpk,
        demographics = in.s2.newDemographics,
      ),
      financial = FinancialMarketsState(
        equity = equityAfterStep,
        corporateBonds = in.s8.newCorpBonds,
        insurance = in.s9.finalInsurance,
        nbfi = in.s9.finalNbfi,
      ),
      external = ExternalState(
        gvc = in.s8.newGvc,
        immigration = in.s2.newImmig,
        tourismSeasonalFactor = tourismSeasonalFactor,
      ),
      real = RealState(
        housing = in.s9.housingAfterFlows,
        sectoralMobility = SectoralMobility.State(
          crossSectorHires = in.s5.postFirmCrossSectorHires + in.s3.hhAgg.crossSectorHires,
          voluntaryQuits = in.s3.hhAgg.voluntaryQuits,
          sectorMobilityRate = in.s9.finalHhAgg.sectorMobilityRate.toDouble,
        ),
        grossInvestment = PLN(in.s5.sumGrossInvestment),
        aggGreenInvestment = PLN(in.s5.sumGreenInvestment),
        aggGreenCapital = PLN(in.s7.aggGreenCapital),
        etsPrice = etsPrice,
        automationRatio = Ratio(in.s7.autoR),
        hybridRatio = Ratio(in.s7.hybR),
      ),
      mechanisms = MechanismsState(
        macropru = in.s7.newMacropru,
        expectations = in.s8.newExp,
        bfgFundBalance = PLN(in.w.mechanisms.bfgFundBalance.toDouble + in.s9.bfgLevy),
        informalCyclicalAdj = newInformalCyclicalAdj,
        effectiveShadowShare = newEffectiveShadowShare,
      ),
      plumbing = MonetaryPlumbingState(
        reserveInterestTotal = PLN(in.s8.totalReserveInterest),
        standingFacilityNet = PLN(in.s8.totalStandingFacilityIncome),
        interbankInterestNet = PLN(in.s8.totalInterbankInterest),
        depositFacilityUsage = PLN(depositFacilityUsage),
        fofResidual = PLN(fofResidual),
      ),
      flows = FlowState(
        ioFlows = PLN(in.s5.totalIoPaid),
        fdiProfitShifting = PLN(in.s5.sumProfitShifting),
        fdiRepatriation = PLN(in.s5.sumFdiRepatriation),
        fdiCitLoss = PLN(in.s8.fdiCitLoss),
        diasporaRemittanceInflow = PLN(in.s6.diasporaInflow),
        tourismExport = PLN(in.s6.tourismExport),
        tourismImport = PLN(in.s6.tourismImport),
        aggInventoryStock = PLN(in.s7.aggInventoryStock),
        aggInventoryChange = PLN(in.s7.aggInventoryChange),
        aggEnergyCost = PLN(in.s5.sumEnergyCost),
        firmBirths = 0,
        firmDeaths = 0,
        taxEvasionLoss = PLN(taxEvasionLoss),
        informalEmployed = informalEmployed,
        bailInLoss = PLN(in.s9.bailInLoss),
        bfgLevyTotal = in.s9.bfgLevy,
        sectorDemandMult = in.s4.sectorMults,
      ),
    )

    // SFC accounting check
    val prevSnap  = Sfc.snapshot(in.w, in.firms, in.w.households)
    val currSnap  = Sfc.snapshot(newW, in.s9.reassignedFirms, in.s9.reassignedHouseholds)
    val sfcFlows  = Sfc.MonthlyFlows(
      govSpending = PLN(
        in.s9.newGovWithYield.unempBenefitSpend.toDouble
          + in.s9.newGovWithYield.socialTransferSpend.toDouble
          + in.s4.govPurchases + in.s8.monthlyDebtService + in.s2.newZus.govSubvention.toDouble
          + in.s7.euCofin,
      ),
      govRevenue = PLN(
        in.s5.sumTax + in.s7.dividendTax + in.s9.pitAfterEvasion + in.s9.vatAfterEvasion + in.s8.nbpRemittance + in.s9.exciseAfterEvasion + in.s9.customsDutyRevenue,
      ),
      nplLoss = PLN(in.s5.nplLoss),
      interestIncome = PLN(in.s5.intIncome),
      hhDebtService = PLN(in.s6.hhDebtService),
      totalIncome = PLN(in.s3.totalIncome),
      totalConsumption = PLN(in.s3.consumption),
      newLoans = PLN(in.s5.sumNewLoans),
      nplRecovery = PLN(in.s5.nplNew * p.banking.loanRecovery.toDouble),
      currentAccount = in.s8.newBop.currentAccount,
      valuationEffect = PLN(in.s8.oeValuationEffect),
      bankBondIncome = PLN(in.s8.bankBondIncome),
      qePurchase = PLN(in.s8.qePurchaseAmount),
      newBondIssuance = PLN(if p.flags.govBondMarket then in.s9.actualBondChange else 0.0),
      depositInterestPaid = PLN(in.s6.depositInterestPaid),
      reserveInterest = PLN(in.s8.totalReserveInterest),
      standingFacilityIncome = PLN(in.s8.totalStandingFacilityIncome),
      interbankInterest = PLN(in.s8.totalInterbankInterest),
      jstDepositChange = PLN(in.s9.jstDepositChange),
      jstSpending = in.s9.newJst.spending,
      jstRevenue = in.s9.newJst.revenue,
      zusContributions = in.s2.newZus.contributions,
      zusPensionPayments = in.s2.newZus.pensionPayments,
      zusGovSubvention = in.s2.newZus.govSubvention,
      dividendIncome = PLN(in.s7.netDomesticDividends),
      foreignDividendOutflow = PLN(in.s7.foreignDividendOutflow),
      dividendTax = PLN(in.s7.dividendTax),
      mortgageInterestIncome = PLN(in.s9.mortgageInterestIncome),
      mortgageNplLoss = PLN(in.s9.mortgageDefaultLoss),
      mortgageOrigination = in.s9.housingAfterFlows.lastOrigination,
      mortgagePrincipalRepaid = PLN(in.s9.mortgagePrincipal),
      mortgageDefaultAmount = PLN(in.s9.mortgageDefaultAmount),
      remittanceOutflow = PLN(in.s6.remittanceOutflow),
      fofResidual = PLN(fofResidual),
      consumerDebtService = PLN(in.s6.consumerDebtService),
      consumerNplLoss = PLN(in.s6.consumerNplLoss),
      consumerOrigination = PLN(in.s6.consumerOrigination),
      consumerPrincipalRepaid = PLN(in.s6.consumerPrincipal),
      consumerDefaultAmount = PLN(in.s6.consumerDefaultAmt),
      corpBondCouponIncome = PLN(in.s8.corpBondBankCoupon),
      corpBondDefaultLoss = PLN(in.s8.corpBondBankDefaultLoss),
      corpBondIssuance = PLN(in.s5.actualBondIssuance),
      corpBondAmortization = PLN(in.s8.corpBondAmort),
      corpBondDefaultAmount = PLN(in.s5.totalBondDefault),
      insNetDepositChange = PLN(in.s8.insNetDepositChange),
      nbfiDepositDrain = PLN(in.s8.nbfiDepositDrain),
      nbfiOrigination = in.s9.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = in.s9.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = in.s9.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = PLN(in.s5.sumProfitShifting),
      fdiRepatriation = PLN(in.s5.sumFdiRepatriation),
      diasporaInflow = PLN(in.s6.diasporaInflow),
      tourismExport = PLN(in.s6.tourismExport),
      tourismImport = PLN(in.s6.tourismImport),
      bfgLevy = PLN(in.s9.bfgLevy),
      bailInLoss = PLN(in.s9.bailInLoss),
      bankCapitalDestruction = PLN(in.s9.multiCapDestruction),
      investNetDepositFlow = PLN(in.s9.investNetDepositFlow),
    )
    val sfcResult = Sfc.validate(prevSnap, currSnap, sfcFlows)

    // FDI M&A: monthly domestic → foreign conversion (#33)
    val postFdiFirms = FirmEntry.applyFdiMa(in.s9.reassignedFirms, rng)

    // Endogenous Firm Entry (#35): recycle bankrupt slots
    val entryResult              = FirmEntry.process(
      FirmEntry.Input(
        firms = postFdiFirms,
        automationRatio = newW.real.automationRatio.toDouble,
        hybridRatio = newW.real.hybridRatio.toDouble,
        rng = rng,
      ),
    )
    val (finalFirms, firmBirths) = (entryResult.firms, entryResult.births)

    val finalW = newW.updateFlows(_.copy(firmBirths = firmBirths, firmDeaths = in.s5.firmDeaths))
    Output(finalW, finalFirms, in.s9.reassignedHouseholds, sfcResult)
