package sfc.accounting

import sfc.agents.{Firm, Household}
import sfc.engine.World
import sfc.types.*
import sfc.util.KahanSum.*

object SfcCheck:

  /** Snapshot of all monetary stocks in the economy at one point in time. */
  case class Snapshot(
    hhSavings: PLN,
    hhDebt: PLN,
    firmCash: PLN,
    firmDebt: PLN,
    bankCapital: PLN,
    bankDeposits: PLN,
    bankLoans: PLN,
    govDebt: PLN,
    nfa: PLN = PLN.Zero,
    bankBondHoldings: PLN = PLN.Zero,
    nbpBondHoldings: PLN = PLN.Zero,
    bondsOutstanding: PLN = PLN.Zero,
    interbankNetSum: PLN = PLN.Zero,
    jstDeposits: PLN = PLN.Zero,
    jstDebt: PLN = PLN.Zero,
    fusBalance: PLN = PLN.Zero, // ZUS/FUS raw surplus/deficit
    ppkBondHoldings: PLN = PLN.Zero, // PPK government bond holdings
    mortgageStock: PLN = PLN.Zero, // Outstanding mortgage debt
    consumerLoans: PLN = PLN.Zero, // Outstanding consumer credit stock
    corpBondsOutstanding: PLN = PLN.Zero, // corporate bond stock
    insuranceGovBondHoldings: PLN = PLN.Zero, // insurance gov bond holdings
    tfiGovBondHoldings: PLN = PLN.Zero, // TFI gov bond holdings
    nbfiLoanStock: PLN = PLN.Zero, // NBFI credit stock
  )

  /** Flows observed during a single month (computed in Simulation.step). These must match the EXACT values used in
    * balance sheet updates.
    */
  case class MonthlyFlows(
    govSpending: PLN,
    govRevenue: PLN,
    nplLoss: PLN,
    interestIncome: PLN,
    hhDebtService: PLN,
    totalIncome: PLN,
    totalConsumption: PLN,
    newLoans: PLN,
    nplRecovery: PLN,
    currentAccount: PLN = PLN.Zero,
    valuationEffect: PLN = PLN.Zero,
    bankBondIncome: PLN = PLN.Zero,
    qePurchase: PLN = PLN.Zero,
    newBondIssuance: PLN = PLN.Zero,
    depositInterestPaid: PLN = PLN.Zero,
    reserveInterest: PLN = PLN.Zero, // NBP pays on required reserves
    standingFacilityIncome: PLN = PLN.Zero, // Deposit/lombard facility net
    interbankInterest: PLN = PLN.Zero, // Interbank interest (net ≈ 0)
    jstDepositChange: PLN = PLN.Zero, // JST deposit flow
    jstSpending: PLN = PLN.Zero, // JST spending
    jstRevenue: PLN = PLN.Zero, // JST revenue
    zusContributions: PLN = PLN.Zero, // ZUS contributions
    zusPensionPayments: PLN = PLN.Zero, // ZUS pension payments
    zusGovSubvention: PLN = PLN.Zero, // ZUS gov subvention
    dividendIncome: PLN = PLN.Zero, // net domestic dividends → HH deposits
    foreignDividendOutflow: PLN = PLN.Zero, // foreign dividends → CA outflow
    dividendTax: PLN = PLN.Zero, // Belka tax → gov revenue
    mortgageInterestIncome: PLN = PLN.Zero, // mortgage interest → bank capital
    mortgageNplLoss: PLN = PLN.Zero, // mortgage NPL loss → bank capital
    mortgageOrigination: PLN = PLN.Zero, // new mortgages issued
    mortgagePrincipalRepaid: PLN = PLN.Zero, // monthly principal repaid
    mortgageDefaultAmount: PLN = PLN.Zero, // gross mortgage defaults (before recovery)
    remittanceOutflow: PLN = PLN.Zero, // immigrant remittances → deposit outflow
    fofResidual: PLN = PLN.Zero, // flow-of-funds residual (Σ firmRevenue - Σ sectorDemand)
    consumerDebtService: PLN = PLN.Zero, // consumer credit: monthly debt service (principal + interest)
    consumerNplLoss: PLN = PLN.Zero, // consumer credit: NPL loss (after recovery)
    consumerOrigination: PLN = PLN.Zero, // consumer credit: new loan origination
    consumerPrincipalRepaid: PLN = PLN.Zero, // consumer credit: principal portion of debt service
    consumerDefaultAmount: PLN = PLN.Zero, // consumer credit: gross default amount (before recovery)
    corpBondCouponIncome: PLN = PLN.Zero, // bank coupon income from corp bonds
    corpBondDefaultLoss: PLN = PLN.Zero, // bank loss from corp bond defaults
    corpBondIssuance: PLN = PLN.Zero, // new corp bonds issued this month
    corpBondAmortization: PLN = PLN.Zero, // corp bond principal repaid
    corpBondDefaultAmount: PLN = PLN.Zero, // gross corp bond defaults
    insNetDepositChange: PLN = PLN.Zero, // insurance net HH deposit change
    nbfiDepositDrain: PLN = PLN.Zero, // TFI deposit drain
    nbfiOrigination: PLN = PLN.Zero, // NBFI monthly origination
    nbfiRepayment: PLN = PLN.Zero, // NBFI monthly repayment
    nbfiDefaultAmount: PLN = PLN.Zero, // NBFI gross monthly defaults
    fdiProfitShifting: PLN = PLN.Zero, // FDI profit shifting (service import)
    fdiRepatriation: PLN = PLN.Zero, // FDI dividend repatriation (primary income debit)
    diasporaInflow: PLN = PLN.Zero, // diaspora remittance inflow → deposit inflow
    tourismExport: PLN = PLN.Zero, // inbound tourism → deposit inflow + export
    tourismImport: PLN = PLN.Zero, // outbound tourism → deposit outflow + import
    bfgLevy: PLN = PLN.Zero, // BFG levy (bank capital expense)
    bailInLoss: PLN = PLN.Zero, // bail-in deposit destruction
    bankCapitalDestruction: PLN = PLN.Zero, // Capital wiped when bank fails (shareholders wiped)
    investNetDepositFlow: PLN = PLN.Zero, // Investment demand net flow: lagged revenue - current spending
  )

  enum SfcIdentity:
    case BankCapital, BankDeposits, GovDebt, Nfa, BondClearing,
      InterbankNetting, JstDebt, FusBalance, MortgageStock,
      FlowOfFunds, ConsumerCredit, CorpBondStock, NbfiCredit

  case class IdentityError(
    identity: SfcIdentity,
    msg: String,
    expected: Double,
    actual: Double,
  )

  def snapshot(w: World, firms: Array[Firm.State], households: Option[Vector[Household.State]]): Snapshot =
    val hhS = PLN(households.map(_.kahanSumBy(_.savings.toDouble)).getOrElse(0.0))
    val hhD = PLN(households.map(_.kahanSumBy(_.debt.toDouble)).getOrElse(0.0))
    val ibNet = PLN(w.bankingSector.map(_.banks.kahanSumBy(_.interbankNet.toDouble)).getOrElse(0.0))
    Snapshot(
      hhSavings = hhS,
      hhDebt = hhD,
      firmCash = PLN(firms.kahanSumBy(_.cash.toDouble)),
      firmDebt = PLN(firms.kahanSumBy(_.debt.toDouble)),
      bankCapital = w.bank.capital,
      bankDeposits = w.bank.deposits,
      bankLoans = w.bank.totalLoans,
      govDebt = w.gov.cumulativeDebt,
      nfa = w.bop.nfa,
      bankBondHoldings = w.bank.govBondHoldings,
      nbpBondHoldings = w.nbp.govBondHoldings,
      bondsOutstanding = w.gov.bondsOutstanding,
      interbankNetSum = ibNet,
      jstDeposits = w.jst.deposits,
      jstDebt = w.jst.debt,
      fusBalance = w.zus.fusBalance,
      ppkBondHoldings = w.ppk.bondHoldings,
      mortgageStock = w.housing.mortgageStock,
      consumerLoans = w.bank.consumerLoans,
      corpBondsOutstanding = w.corporateBonds.outstanding,
      insuranceGovBondHoldings = w.insurance.govBondHoldings,
      tfiGovBondHoldings = w.nbfi.tfiGovBondHoldings,
      nbfiLoanStock = w.nbfi.nbfiLoanStock,
    )

  /** Validate 13 exact balance-sheet identities. Returns `Right(())` if all pass,
    * or `Left(errors)` with every violated identity and its expected/actual values.
    *
    * Together these 13 identities ensure that every financial asset has a matching liability,
    * which implies the Godley sectoral balances rule (S−I)+(G−T)+(X−M)=0 by construction.
    *
    * The monetary circuit closes via sector-level flow-of-funds (Identity 10).
    *
    *   1. Bank capital: Δ = -nplLoss - mortgageNplLoss - consumerNplLoss - corpBondDefaultLoss - bfgLevy -
    *      bankCapitalDestruction + (interestIncome + hhDebtService + bankBondIncome + mortgageInterestIncome +
    *      consumerDebtService + corpBondCouponIncome - depositInterestPaid + reserveInterest +
    *      standingFacilityIncome + interbankInterest) × 0.3
    *   2. Bank deposits: Δ = totalIncome - totalConsumption + investNetDepositFlow + jstDepositChange + dividendIncome -
    *      foreignDividendOutflow - remittanceOutflow + diasporaInflow + tourismExport - tourismImport - bailInLoss +
    *      consumerOrigination + insNetDepositChange + nbfiDepositDrain
    *   3. Gov debt: Δ = govSpending - govRevenue (govRevenue includes dividendTax + zusGovSubvention)
    *   4. NFA: Δ = currentAccount + valuationEffect (currentAccount includes -foreignDividendOutflow,
    *      -fdiProfitShifting, -fdiRepatriation, +diasporaInflow)
    *   5. Bond clearing: bankBondHoldings + nbpBondHoldings + ppkBondHoldings + insuranceGovBondHoldings +
    *      tfiGovBondHoldings = bondsOutstanding
    *   6. Interbank netting: Σ interbankNet_i = 0 (trivially 0 in single-bank mode)
    *   7. JST debt: Δ = jstSpending - jstRevenue (trivially 0 when JST disabled)
    *   8. FUS balance: Δ = zusContributions - zusPensionPayments (trivially 0 when ZUS disabled)
    *   9. Mortgage stock: Δ = origination - principalRepaid - defaultAmount (trivially 0 when RE disabled)
    *   10. Flow-of-funds: Σ firmRevenue = domesticCons + govPurchases + investDemand + exports (closes by construction)
    *   11. Consumer credit: Δ consumerLoans = origination - principalRepaid - defaultAmount
    *   12. Corp bond stock: Δ corpBondsOutstanding = issuance - amortization - defaultAmount
    *   13. NBFI credit stock: Δ nbfiLoanStock = origination - repayment - defaultAmount
    *
    * These catch: mis-routed flows (e.g. rent subtracted from HH but not added to bank/consumption), refactoring errors
    * in balance sheet updates, and any new flow that modifies a stock without updating the counterpart.
    */
  def validate(
    month: Int,
    prev: Snapshot,
    curr: Snapshot,
    flows: MonthlyFlows,
    tolerance: Double = 0.01,
    nfaTolerance: Double = 1.0,
  ): Either[Vector[IdentityError], Unit] =
    import SfcIdentity.*
    val errors = Vector.newBuilder[IdentityError]

    def check(id: SfcIdentity, msg: String, expected: Double, actual: Double, tol: Double = tolerance): Unit =
      if Math.abs(actual - expected) >= tol then
        errors += IdentityError(id, msg, expected, actual)

    // Identity 1: Bank capital (profit retention + losses)
    val expectedBankCapChange = (-flows.nplLoss - flows.mortgageNplLoss - flows.consumerNplLoss
      - flows.corpBondDefaultLoss - flows.bfgLevy - flows.bankCapitalDestruction +
      (flows.interestIncome + flows.hhDebtService + flows.bankBondIncome
        + flows.mortgageInterestIncome + flows.consumerDebtService + flows.corpBondCouponIncome
        - flows.depositInterestPaid
        + flows.reserveInterest + flows.standingFacilityIncome + flows.interbankInterest) * 0.3).toDouble
    val actualBankCapChange = (curr.bankCapital - prev.bankCapital).toDouble
    check(BankCapital, "bank capital change (profit retention + losses)", expectedBankCapChange, actualBankCapChange)

    // Identity 2: Bank deposits
    val expectedDepChange = (flows.totalIncome - flows.totalConsumption + flows.investNetDepositFlow +
      flows.jstDepositChange +
      flows.dividendIncome - flows.foreignDividendOutflow - flows.remittanceOutflow + flows.diasporaInflow +
      flows.tourismExport - flows.tourismImport - flows.bailInLoss +
      flows.consumerOrigination + flows.insNetDepositChange + flows.nbfiDepositDrain).toDouble
    val actualDepChange = (curr.bankDeposits - prev.bankDeposits).toDouble
    check(BankDeposits, "bank deposits change", expectedDepChange, actualDepChange)

    // Identity 3: Government debt (deficit = spending - revenue)
    val expectedGovDebtChange = (flows.govSpending - flows.govRevenue).toDouble
    val actualGovDebtChange = (curr.govDebt - prev.govDebt).toDouble
    check(GovDebt, "government debt change", expectedGovDebtChange, actualGovDebtChange)

    // Identity 4: NFA (Net Foreign Assets) — wider tolerance for FP cancellation
    val expectedNfaChange = (flows.currentAccount + flows.valuationEffect).toDouble
    val actualNfaChange = (curr.nfa - prev.nfa).toDouble
    check(Nfa, "NFA change (current account + valuation)", expectedNfaChange, actualNfaChange, nfaTolerance)

    // Identity 5: Bond clearing (holders = outstanding)
    val bondHolders = (curr.bankBondHoldings + curr.nbpBondHoldings + curr.ppkBondHoldings +
      curr.insuranceGovBondHoldings + curr.tfiGovBondHoldings).toDouble
    val bondsOut = curr.bondsOutstanding.toDouble
    check(BondClearing, "bond clearing (holders vs outstanding)", bondsOut, bondHolders)

    // Identity 6: Interbank netting (should be zero)
    check(InterbankNetting, "interbank netting (should be zero)", 0.0, curr.interbankNetSum.toDouble)

    // Identity 7: JST debt
    val expectedJstDebtChange = (flows.jstSpending - flows.jstRevenue).toDouble
    val actualJstDebtChange = (curr.jstDebt - prev.jstDebt).toDouble
    check(JstDebt, "JST debt change", expectedJstDebtChange, actualJstDebtChange)

    // Identity 8: FUS balance
    val expectedFusChange = (flows.zusContributions - flows.zusPensionPayments).toDouble
    val actualFusChange = (curr.fusBalance - prev.fusBalance).toDouble
    check(FusBalance, "FUS balance change (contributions - pensions)", expectedFusChange, actualFusChange)

    // Identity 9: Mortgage stock
    val expectedMortgageChange =
      (flows.mortgageOrigination - flows.mortgagePrincipalRepaid - flows.mortgageDefaultAmount).toDouble
    val actualMortgageChange = (curr.mortgageStock - prev.mortgageStock).toDouble
    check(MortgageStock, "mortgage stock change", expectedMortgageChange, actualMortgageChange)

    // Identity 10: Flow-of-funds residual (closes by construction)
    check(FlowOfFunds, "flow-of-funds residual", 0.0, flows.fofResidual.toDouble)

    // Identity 11: Consumer credit stock
    val expectedCcChange =
      (flows.consumerOrigination - flows.consumerPrincipalRepaid - flows.consumerDefaultAmount).toDouble
    val actualCcChange = (curr.consumerLoans - prev.consumerLoans).toDouble
    check(ConsumerCredit, "consumer credit stock change", expectedCcChange, actualCcChange)

    // Identity 12: Corporate bond stock
    val expectedCorpBondChange =
      (flows.corpBondIssuance - flows.corpBondAmortization - flows.corpBondDefaultAmount).toDouble
    val actualCorpBondChange = (curr.corpBondsOutstanding - prev.corpBondsOutstanding).toDouble
    check(CorpBondStock, "corporate bond stock change", expectedCorpBondChange, actualCorpBondChange)

    // Identity 13: NBFI credit stock
    val expectedNbfiChange = (flows.nbfiOrigination - flows.nbfiRepayment - flows.nbfiDefaultAmount).toDouble
    val actualNbfiChange = (curr.nbfiLoanStock - prev.nbfiLoanStock).toDouble
    check(NbfiCredit, "NBFI credit stock change", expectedNbfiChange, actualNbfiChange)

    val result = errors.result()
    if result.isEmpty then Right(()) else Left(result)
