package sfc.engine.steps

import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.*
import sfc.engine.markets.{FiscalBudget, HousingMarket}
import sfc.engine.mechanisms.{TaxRevenue, YieldCurve}
import sfc.types.*
import sfc.util.KahanSum.*

object BankUpdateStep:

  case class Input(
      w: World,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
  )

  case class Output(
      resolvedBank: Banking.Aggregate,
      finalBankingSector: Banking.State,
      reassignedFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      finalPpk: SocialSecurity.PpkState,
      finalInsurance: Insurance.State,
      finalNbfi: Nbfi.State,
      newGovWithYield: FiscalBudget.GovState,
      newJst: Jst.State,
      housingAfterFlows: HousingMarket.State,
      bfgLevy: PLN,
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      monAgg: Option[Banking.MonetaryAggregates],
      finalHhAgg: Household.Aggregates,
      // Tax intermediates (needed by SFC check)
      vat: PLN,
      vatAfterEvasion: PLN,
      pitAfterEvasion: PLN,
      exciseRevenue: PLN,
      exciseAfterEvasion: PLN,
      customsDutyRevenue: PLN,
      effectiveShadowShare: Ratio,
      // Housing flows (needed by SFC check)
      mortgageInterestIncome: PLN,
      mortgagePrincipal: PLN,
      mortgageDefaultLoss: PLN,
      mortgageDefaultAmount: PLN,
      // Other intermediates (needed by SFC/World assembly)
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      actualBondChange: PLN,
  )

  // --- Intermediate result types for sub-methods ---

  private case class GovJstResult(
      newGovWithYield: FiscalBudget.GovState,
      newJst: Jst.State,
      jstDepositChange: PLN,
      tax: TaxRevenue.Output,
  )

  private case class HousingResult(
      housingAfterFlows: HousingMarket.State,
      mortgageFlows: HousingMarket.MortgageFlows,
  )

  private case class BondAllocations(
      finalPpk: SocialSecurity.PpkState,
      finalInsurance: Insurance.State,
      finalNbfi: Nbfi.State,
      ppkBondPurchase: PLN,
      insBondPurchase: PLN,
      tfiBondPurchase: PLN,
      actualBondChange: PLN,
  )

  private case class PerBankHhFlows(
      incomeShare: PLN,
      consShare: PLN,
      hhDebtService: PLN,
      depInterest: PLN,
      ccDebtService: PLN,
      ccOrigination: PLN,
      ccDefault: PLN,
  )

  private case class MultiBankResult(
      finalBankingSector: Banking.State,
      reassignedFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      resolvedBank: Banking.Aggregate,
  )

  def run(in: Input)(using p: SimParams): Output =
    val govJst               = computeGovAndJst(in)
    val housing              = computeHousingFlows(in)
    val bfgLevy              =
      if p.flags.bankFailure then Banking.computeBfgLevy(in.w.bankingSector.banks).total
      else PLN.Zero
    val investNetDepositFlow = computeInvestNetDepositFlow(in)
    val newBank              =
      computeAggregateBank(in, housing.mortgageFlows, bfgLevy, investNetDepositFlow, govJst.jstDepositChange)
    val finalHhAgg           = computeHhAgg(in)
    val bonds                = computeBondAllocations(in, newBank, govJst.newGovWithYield)
    val multi                = processMultiBankPath(
      in,
      govJst.jstDepositChange,
      investNetDepositFlow,
      housing.mortgageFlows,
      bonds,
    )
    val monAgg               = computeMonetaryAggregates(multi.finalBankingSector, multi.resolvedBank)

    Output(
      resolvedBank = multi.resolvedBank,
      finalBankingSector = multi.finalBankingSector,
      reassignedFirms = multi.reassignedFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalPpk = bonds.finalPpk,
      finalInsurance = bonds.finalInsurance,
      finalNbfi = bonds.finalNbfi,
      newGovWithYield = govJst.newGovWithYield,
      newJst = govJst.newJst,
      housingAfterFlows = housing.housingAfterFlows,
      bfgLevy = bfgLevy,
      bailInLoss = multi.bailInLoss,
      multiCapDestruction = multi.multiCapDestruction,
      monAgg = monAgg,
      finalHhAgg = finalHhAgg,
      vat = PLN(govJst.tax.vat),
      vatAfterEvasion = PLN(govJst.tax.vatAfterEvasion),
      pitAfterEvasion = PLN(govJst.tax.pitAfterEvasion),
      exciseRevenue = PLN(govJst.tax.exciseRevenue),
      exciseAfterEvasion = PLN(govJst.tax.exciseAfterEvasion),
      customsDutyRevenue = PLN(govJst.tax.customsDutyRevenue),
      effectiveShadowShare = Ratio(govJst.tax.effectiveShadowShare),
      mortgageInterestIncome = housing.mortgageFlows.interest,
      mortgagePrincipal = housing.mortgageFlows.principal,
      mortgageDefaultLoss = housing.mortgageFlows.defaultLoss,
      mortgageDefaultAmount = housing.mortgageFlows.defaultAmount,
      jstDepositChange = govJst.jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      actualBondChange = bonds.actualBondChange,
    )

  /** Government budget update (deficit, debt, bonds) and JST local government
    * step.
    */
  private def computeGovAndJst(in: Input)(using p: SimParams): GovJstResult =
    val tax = TaxRevenue.compute(
      TaxRevenue.Input(
        consumption = in.s3.consumption,
        pitRevenue = in.s3.pitRevenue,
        totalImports = in.s8.external.newBop.totalImports.toDouble,
        informalCyclicalAdj = in.w.mechanisms.informalCyclicalAdj,
      ),
    )

    val unempBenefitSpend   = in.s3.hhAgg.totalUnempBenefits.toDouble
    val socialTransferSpend =
      if p.flags.social800 then in.s3.hhAgg.totalSocialTransfers.toDouble
      else 0.0

    val newGov          = FiscalBudget.update(
      FiscalBudget.Input(
        prev = in.w.gov,
        priceLevel = in.s7.newPrice,
        citPaid = PLN(in.s5.sumTax + in.s7.dividendTax + tax.pitAfterEvasion),
        vat = PLN(tax.vatAfterEvasion),
        nbpRemittance = in.s8.banking.nbpRemittance,
        exciseRevenue = PLN(tax.exciseAfterEvasion),
        customsDutyRevenue = PLN(tax.customsDutyRevenue),
        unempBenefitSpend = PLN(unempBenefitSpend),
        debtService = in.s8.banking.monthlyDebtService,
        zusGovSubvention = PLN(in.s2.newZus.govSubvention.toDouble),
        socialTransferSpend = PLN(socialTransferSpend),
        euCofinancing = PLN(in.s7.euCofin),
        euProjectCapital = PLN(in.s7.euProjectCapital),
        govPurchasesActual = PLN(in.s4.govPurchases),
      ),
    )
    val newGovWithYield = newGov.copy(bondYield = in.s8.monetary.newBondYield)

    val nLivingFirms = in.s5.ioFirms.count(Firm.isAlive)
    val jstResult    =
      Jst.step(
        in.w.social.jst,
        newGovWithYield.taxRevenue,
        PLN(in.s3.totalIncome),
        PLN(in.s7.gdp),
        nLivingFirms,
        PLN(tax.pitAfterEvasion),
      )

    GovJstResult(
      newGovWithYield = newGovWithYield,
      newJst = jstResult.state,
      jstDepositChange = jstResult.depositChange,
      tax = tax,
    )

  /** Housing market: price step, origination, mortgage flows. */
  private def computeHousingFlows(in: Input)(using p: SimParams): HousingResult =
    val unempRate                = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val prevMortgageRate         = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Double =
      if p.flags.interbankTermStructure then YieldCurve.compute(in.w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else in.w.nbp.referenceRate.toDouble
    val mortgageRate             = mortgageBaseRate + p.housing.mortgageSpread.toDouble

    val mortgageRateTyped = Rate(mortgageRate)
    val housingAfterPrice = HousingMarket.step(
      HousingMarket.StepInput(
        prev = in.w.real.housing,
        mortgageRate = mortgageRateTyped,
        inflation = Rate(in.s7.newInfl),
        incomeGrowth = Rate(in.s2.wageGrowth),
        employed = in.s2.employed,
        prevMortgageRate = prevMortgageRate,
      ),
    )
    val housingAfterOrig  =
      HousingMarket.processOrigination(housingAfterPrice, PLN(in.s3.totalIncome), mortgageRateTyped, true)
    val mortgageFlows     = HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRateTyped, Ratio(unempRate))
    val housingAfterFlows = HousingMarket.applyFlows(housingAfterOrig, mortgageFlows)

    HousingResult(housingAfterFlows = housingAfterFlows, mortgageFlows = mortgageFlows)

  /** Investment net deposit flow: lagged demand minus current domestic
    * investment.
    */
  private def computeInvestNetDepositFlow(in: Input)(using p: SimParams): PLN =
    val currentInvestDomestic = in.s5.sumGrossInvestment * (1.0 - p.capital.importShare.toDouble) +
      in.s5.sumGreenInvestment * (1.0 - p.climate.greenImportShare.toDouble)
    PLN(in.s4.laggedInvestDemand - currentInvestDomestic)

  /** Single-bank aggregate path: capital PnL, loans, deposits, consumer credit.
    */
  private def computeAggregateBank(
      in: Input,
      mortgageFlows: HousingMarket.MortgageFlows,
      bfgLevy: PLN,
      investNetDepositFlow: PLN,
      jstDepositChange: PLN,
  )(using p: SimParams): Banking.Aggregate =
    val aggCapitalPnl   = Banking.computeCapitalDelta(
      Banking.CapitalPnlInput(
        prevCapital = in.w.bank.capital,
        nplLoss = PLN(in.s5.nplLoss),
        mortgageNplLoss = mortgageFlows.defaultLoss,
        consumerNplLoss = PLN(in.s6.consumerNplLoss),
        corpBondDefaultLoss = in.s8.corpBonds.corpBondBankDefaultLoss,
        bfgLevy = bfgLevy,
        intIncome = PLN(in.s5.intIncome),
        hhDebtService = PLN(in.s6.hhDebtService),
        bondIncome = in.s8.banking.bankBondIncome,
        depositInterest = PLN(in.s6.depositInterestPaid),
        reserveInterest = in.s8.banking.totalReserveInterest,
        standingFacilityIncome = in.s8.banking.totalStandingFacilityIncome,
        interbankInterest = in.s8.banking.totalInterbankInterest,
        mortgageInterestIncome = mortgageFlows.interest,
        consumerDebtService = PLN(in.s6.consumerDebtService),
        corpBondCoupon = in.s8.corpBonds.corpBondBankCoupon,
      ),
    )
    val nplWriteOffRate = 0.05 // monthly NPL write-off rate
    in.w.bank.copy(
      totalLoans = PLN(
        Math.max(0, in.w.bank.totalLoans.toDouble + in.s5.sumNewLoans - in.s5.nplNew * p.banking.loanRecovery.toDouble),
      ),
      nplAmount = PLN(Math.max(0, in.w.bank.nplAmount.toDouble + in.s5.nplNew - in.w.bank.nplAmount.toDouble * nplWriteOffRate)),
      capital = aggCapitalPnl.newCapital,
      deposits = PLN(
        in.w.bank.deposits.toDouble + (in.s3.totalIncome - in.s3.consumption) + investNetDepositFlow.toDouble
          + jstDepositChange.toDouble
          + in.s7.netDomesticDividends - in.s7.foreignDividendOutflow - in.s6.remittanceOutflow + in.s6.diasporaInflow
          + in.s6.tourismExport - in.s6.tourismImport
          + in.s6.consumerOrigination + in.s8.nonBank.insNetDepositChange.toDouble + in.s8.nonBank.nbfiDepositDrain.toDouble,
      ),
      consumerLoans = PLN(
        Math.max(
          0.0,
          in.w.bank.consumerLoans.toDouble + in.s6.consumerOrigination - in.s6.consumerPrincipal - in.s6.consumerDefaultAmt,
        ),
      ),
      consumerNpl = PLN(
        Math.max(0.0, in.w.bank.consumerNpl.toDouble + in.s6.consumerDefaultAmt - in.w.bank.consumerNpl.toDouble * nplWriteOffRate),
      ),
      corpBondHoldings = in.s8.corpBonds.newCorpBonds.bankHoldings,
    )

  /** Recompute household aggregates from final households. */
  private def computeHhAgg(in: Input)(using SimParams): Household.Aggregates =
    Household.computeAggregates(
      in.s5.households,
      PLN(in.s2.newWage),
      PLN(in.s1.resWage),
      in.s3.importAdj,
      in.s3.hhAgg.retrainingAttempts,
      in.s3.hhAgg.retrainingSuccesses,
    )

  /** PPK, insurance, and TFI gov bond purchases, each capped at available
    * supply.
    */
  private def computeBondAllocations(
      in: Input,
      newBank: Banking.Aggregate,
      newGovWithYield: FiscalBudget.GovState,
  )(using p: SimParams): BondAllocations =
    val actualBondChange = (newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding).toDouble
    val bondSupplyBase   = newBank.govBondHoldings.toDouble +
      (if p.flags.govBondMarket then actualBondChange else 0.0) - in.s8.monetary.qePurchaseAmount.toDouble

    // PPK bond purchases (capped at available bonds after QE)
    val ppkBondPurchase = Math.min(in.s2.rawPpkBondPurchase, Math.max(0.0, bondSupplyBase))
    val finalPpk        = in.s2.newPpk.copy(bondHoldings = PLN(in.w.social.ppk.bondHoldings.toDouble + ppkBondPurchase))

    // Insurance gov bond purchases (capped at available bonds after QE + PPK)
    val insGovBondDelta      = (in.s8.nonBank.newInsurance.govBondHoldings - in.w.financial.insurance.govBondHoldings).toDouble
    val availableBondsForIns = bondSupplyBase - ppkBondPurchase
    val insBondPurchase      = Math.max(0.0, Math.min(Math.max(0.0, insGovBondDelta), Math.max(0.0, availableBondsForIns)))
    val finalInsurance       =
      in.s8.nonBank.newInsurance.copy(govBondHoldings = PLN(in.w.financial.insurance.govBondHoldings.toDouble + insBondPurchase))

    // TFI gov bond purchases (#42)
    val tfiGovBondDelta      = (in.s8.nonBank.newNbfi.tfiGovBondHoldings - in.w.financial.nbfi.tfiGovBondHoldings).toDouble
    val availableBondsForTfi = bondSupplyBase - ppkBondPurchase - insBondPurchase
    val tfiBondPurchase      = Math.max(0.0, Math.min(Math.max(0.0, tfiGovBondDelta), Math.max(0.0, availableBondsForTfi)))
    val finalNbfi            =
      in.s8.nonBank.newNbfi.copy(tfiGovBondHoldings = PLN(in.w.financial.nbfi.tfiGovBondHoldings.toDouble + tfiBondPurchase))

    BondAllocations(
      finalPpk,
      finalInsurance,
      finalNbfi,
      ppkBondPurchase = PLN(ppkBondPurchase),
      insBondPurchase = PLN(insBondPurchase),
      tfiBondPurchase = PLN(tfiBondPurchase),
      actualBondChange = PLN(actualBondChange),
    )

  /** Resolve per-bank household flows from tracked data or worker-share
    * fallback.
    */
  private def resolvePerBankHhFlows(
      bId: Int,
      perBankHhFlowsOpt: Option[Vector[PerBankFlow]],
      totalWorkers: Double,
      perBankWorkers: Vector[Int],
      in: Input,
  ): PerBankHhFlows =
    perBankHhFlowsOpt match
      case Some(pbf) =>
        val f = pbf(bId)
        PerBankHhFlows(
          incomeShare = f.income,
          consShare = f.consumption,
          hhDebtService = f.debtService,
          depInterest = f.depositInterest,
          ccDebtService = f.consumerDebtService,
          ccOrigination = f.consumerOrigination,
          ccDefault = f.consumerDefault,
        )
      case None      =>
        val ws = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
        PerBankHhFlows(
          incomeShare = PLN(in.s3.totalIncome * ws),
          consShare = PLN(in.s3.consumption * ws),
          hhDebtService = PLN(in.s6.hhDebtService * ws),
          depInterest = PLN.Zero,
          ccDebtService = PLN(in.s6.consumerDebtService * ws),
          ccOrigination = PLN(in.s6.consumerOrigination * ws),
          ccDefault = PLN(in.s6.consumerDefaultAmt * ws),
        )

  /** Compute updated state for a single bank in the multi-bank path. */
  private def updateSingleBank(
      b: Banking.BankState,
      hhFlows: PerBankHhFlows,
      workerShare: Ratio,
      mortgageFlows: HousingMarket.MortgageFlows,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      in: Input,
  )(using p: SimParams): Banking.BankState =
    val bId           = b.id.toInt
    val ws            = workerShare.toDouble
    val bankNplNew    = in.s5.perBankNplDebt(bId)
    val bankNplLoss   = bankNplNew * (1.0 - p.banking.loanRecovery.toDouble)
    val bankIntIncome = in.s5.perBankIntIncome(bId)
    val bankBondInc   = b.govBondHoldings.toDouble * in.s8.monetary.newBondYield.toDouble / 12.0
    val bankResInt    = perBankReserveInt.perBank(bId).toDouble
    val bankSfInc     = perBankStandingFac.perBank(bId).toDouble
    val bankIbInt     = perBankInterbankInt.perBank(bId).toDouble
    val newLoansTotal =
      Math.max(0.0, b.loans.toDouble + in.s5.perBankNewLoans(bId) - bankNplNew * p.banking.loanRecovery.toDouble)

    val newDep = b.deposits.toDouble + (hhFlows.incomeShare.toDouble - hhFlows.consShare.toDouble) +
      investNetDepositFlow.toDouble * ws + jstDepositChange.toDouble * ws +
      in.s7.netDomesticDividends * ws - in.s7.foreignDividendOutflow * ws -
      in.s6.remittanceOutflow * ws + in.s6.diasporaInflow * ws +
      in.s6.tourismExport * ws - in.s6.tourismImport * ws +
      hhFlows.ccOrigination.toDouble +
      in.s8.nonBank.insNetDepositChange.toDouble * ws + in.s8.nonBank.nbfiDepositDrain.toDouble * ws

    val bankMortgageIntIncome   = mortgageFlows.interest.toDouble * ws
    val bankMortgageNplLoss     = mortgageFlows.defaultLoss.toDouble * ws
    val bankCcNplLoss           = hhFlows.ccDefault.toDouble * (1.0 - p.household.ccNplRecovery.toDouble)
    val bankCcPrincipal         = in.s3.perBankHhFlowsOpt match
      case Some(pbf) => pbf(bId).consumerPrincipal.toDouble
      case _         =>
        val ccAmort       = p.household.ccAmortRate.toDouble
        val ccMonthlyRate = (in.s1.lendingBaseRate + p.household.ccSpread.toDouble) / 12.0
        if ccAmort + ccMonthlyRate > 0 then hhFlows.ccDebtService.toDouble * (ccAmort / (ccAmort + ccMonthlyRate))
        else 0.0
    val bankCorpBondCoupon      = in.s8.corpBonds.corpBondBankCoupon.toDouble * ws
    val bankCorpBondDefaultLoss = in.s8.corpBonds.corpBondBankDefaultLoss.toDouble * ws
    val bankBfgLevy             =
      if p.flags.bankFailure && !b.failed then b.deposits.toDouble * p.banking.bfgLevyRate.toDouble / 12.0
      else 0.0

    val capitalPnl = Banking.computeCapitalDelta(
      Banking.CapitalPnlInput(
        prevCapital = b.capital,
        nplLoss = PLN(bankNplLoss),
        mortgageNplLoss = PLN(bankMortgageNplLoss),
        consumerNplLoss = PLN(bankCcNplLoss),
        corpBondDefaultLoss = PLN(bankCorpBondDefaultLoss),
        bfgLevy = PLN(bankBfgLevy),
        intIncome = PLN(bankIntIncome),
        hhDebtService = PLN(hhFlows.hhDebtService.toDouble),
        bondIncome = PLN(bankBondInc),
        depositInterest = PLN(hhFlows.depInterest.toDouble),
        reserveInterest = PLN(bankResInt),
        standingFacilityIncome = PLN(bankSfInc),
        interbankInterest = PLN(bankIbInt),
        mortgageInterestIncome = PLN(bankMortgageIntIncome),
        consumerDebtService = PLN(hhFlows.ccDebtService.toDouble),
        corpBondCoupon = PLN(bankCorpBondCoupon),
      ),
    )

    val nplWriteOffRate = 0.05 // monthly NPL write-off rate
    val shortLoanFrac   = 0.20
    val mediumLoanFrac  = 0.30
    val longLoanFrac    = 0.50
    b.copy(
      loans = PLN(newLoansTotal),
      nplAmount = PLN(Math.max(0.0, b.nplAmount.toDouble + bankNplNew - b.nplAmount.toDouble * nplWriteOffRate)),
      capital = capitalPnl.newCapital,
      deposits = PLN(newDep),
      demandDeposits = PLN(newDep * (1.0 - p.banking.termDepositFrac.toDouble)),
      termDeposits = PLN(newDep * p.banking.termDepositFrac.toDouble),
      loansShort = PLN(newLoansTotal * shortLoanFrac),
      loansMedium = PLN(newLoansTotal * mediumLoanFrac),
      loansLong = PLN(newLoansTotal * longLoanFrac),
      consumerLoans = PLN(Math.max(0.0, b.consumerLoans.toDouble + hhFlows.ccOrigination.toDouble - bankCcPrincipal - hhFlows.ccDefault.toDouble)),
      consumerNpl = PLN(
        Math.max(0.0, b.consumerNpl.toDouble + hhFlows.ccDefault.toDouble - b.consumerNpl.toDouble * nplWriteOffRate),
      ),
      corpBondHoldings = in.s8.corpBonds.newCorpBonds.bankHoldings * ws,
    )

  /** Multi-bank update: per-bank loop, interbank clearing, bond allocation,
    * failure resolution.
    */
  private def processMultiBankPath(
      in: Input,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      bonds: BondAllocations,
  )(using p: SimParams): MultiBankResult =
    val bs                  = in.w.bankingSector
    val perBankReserveInt   = Banking.computeReserveInterest(bs.banks, in.w.nbp.referenceRate)
    val perBankStandingFac  = Banking.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate)
    val perBankInterbankInt = Banking.interbankInterestFlows(bs.banks, bs.interbankRate)
    val totalWorkers        = in.s5.perBankWorkers.kahanSumBy(_.toDouble)

    val updatedBanks = bs.banks.map { b =>
      val bId         = b.id.toInt
      val workerShare = Ratio(if totalWorkers > 0 then in.s5.perBankWorkers(bId) / totalWorkers else 0.0)
      val hhFlows     = resolvePerBankHhFlows(bId, in.s3.perBankHhFlowsOpt, totalWorkers, in.s5.perBankWorkers, in)
      updateSingleBank(
        b,
        hhFlows,
        workerShare,
        mortgageFlows,
        perBankReserveInt,
        perBankStandingFac,
        perBankInterbankInt,
        jstDepositChange,
        investNetDepositFlow,
        in,
      )
    }

    processInterbankAndFailures(in, updatedBanks, bs, bonds)

  /** Interbank clearing, bond allocation, QE, failure check, bail-in,
    * resolution, reassignment.
    */
  private def processInterbankAndFailures(
      in: Input,
      updatedBanks: Vector[Banking.BankState],
      bs: Banking.State,
      bonds: BondAllocations,
  )(using p: SimParams): MultiBankResult =
    val ibRate               = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate)
    val afterInterbank       = Banking.clearInterbank(updatedBanks, bs.configs)
    val afterBonds           =
      if p.flags.govBondMarket then Banking.allocateBonds(afterInterbank, bonds.actualBondChange)
      else afterInterbank
    val afterQe              = Banking.allocateQePurchases(afterBonds, in.s8.monetary.qePurchaseAmount)
    val afterPpk             = Banking.allocateQePurchases(afterQe, bonds.ppkBondPurchase)
    val afterIns             = Banking.allocateQePurchases(afterPpk, bonds.insBondPurchase)
    val afterTfi             = Banking.allocateQePurchases(afterIns, bonds.tfiBondPurchase)
    val failResult           =
      Banking.checkFailures(afterTfi, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb)
    val afterFailCheck       = failResult.banks
    val anyFailed            = failResult.anyFailed
    val bailInResult         =
      if anyFailed then Banking.applyBailIn(afterFailCheck) else Banking.BailInResult(afterFailCheck, PLN.Zero)
    val resolveResult        =
      if anyFailed then Banking.resolveFailures(bailInResult.banks)
      else Banking.ResolutionResult(bailInResult.banks, BankId.NoBank)
    val afterResolve         = resolveResult.banks
    val rawAbsorberId        = resolveResult.absorberId
    val absorberId           =
      if rawAbsorberId.toInt >= 0 then rawAbsorberId
      else Banking.healthiestBankId(afterResolve)
    val multiCapDest         =
      if anyFailed then
        afterTfi
          .zip(afterFailCheck)
          .map { case (pre, post) =>
            if !pre.failed && post.failed then pre.capital.toDouble else 0.0
          }
          .kahanSum
      else 0.0
    val curve                = if p.flags.interbankTermStructure then Some(YieldCurve.compute(ibRate.toDouble)) else None
    val finalBankingSector   = bs.copy(banks = afterResolve, interbankRate = ibRate, interbankCurve = curve)
    val reassignedFirms      =
      if anyFailed then
        in.s7.rewiredFirms.map: f =>
          if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
          else f
      else in.s7.rewiredFirms
    val reassignedHouseholds =
      if anyFailed then
        in.s5.households.map: h =>
          if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
          else h
      else in.s5.households

    MultiBankResult(
      finalBankingSector = finalBankingSector,
      reassignedFirms = reassignedFirms,
      reassignedHouseholds = reassignedHouseholds,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = PLN(multiCapDest),
      resolvedBank = finalBankingSector.aggregate,
    )

  /** Monetary aggregates (M1/M2/M3) when credit diagnostics enabled. */
  private def computeMonetaryAggregates(
      finalBankingSector: Banking.State,
      resolvedBank: Banking.Aggregate,
  )(using p: SimParams): Option[Banking.MonetaryAggregates] =
    if p.flags.creditDiagnostics then
      val totalReserves = finalBankingSector.banks.kahanSumBy(_.reservesAtNbp.toDouble)
      Some(Banking.MonetaryAggregates.compute(resolvedBank.deposits, PLN(totalReserves)))
    else None
