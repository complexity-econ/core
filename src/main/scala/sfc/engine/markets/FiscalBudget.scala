package sfc.engine.markets

import sfc.accounting.*
import sfc.config.*
import sfc.types.*

/** Government budget reconciliation: monthly revenue, spending, deficit, debt.
  *
  * Revenue: CIT + dividend tax (PIT Belka 19%), VAT, excise, customs, NBP
  * remittance. Spending: current government purchases, capital investment,
  * unemployment benefits, social transfers (800+), ZUS subvention, EU
  * co-financing, debt service.
  *
  * When GOV_INVEST is enabled, base spending splits into current (1 − share)
  * and capital (share) components; public capital stock depreciates monthly.
  * Bond-financed deficit accumulates when GOV_BOND_MARKET is enabled.
  *
  * Calibration: MF budgetary law (ustawa budżetowa) structure. All flows in
  * nominal PLN, price-adjusted via priceLevel.
  */
object FiscalBudget:

  /** Monthly fiscal inputs — all monetary fields in PLN. */
  case class Input(
      prev: GovState,
      priceLevel: Double,
      // Revenue
      citPaid: PLN = PLN.Zero,
      vat: PLN = PLN.Zero,
      nbpRemittance: PLN = PLN.Zero,
      exciseRevenue: PLN = PLN.Zero,
      customsDutyRevenue: PLN = PLN.Zero,
      // Spending
      unempBenefitSpend: PLN = PLN.Zero,
      debtService: PLN = PLN.Zero,
      zusGovSubvention: PLN = PLN.Zero,
      socialTransferSpend: PLN = PLN.Zero,
      euCofinancing: PLN = PLN.Zero,
      euProjectCapital: PLN = PLN.Zero,
      govPurchasesActual: PLN = PLN.Zero,
  )

  /** Monthly budget update → new GovState. */
  def update(in: Input)(using p: SimParams): GovState =
    val govBaseRaw =
      if in.govPurchasesActual > PLN.Zero then in.govPurchasesActual.toDouble
      else p.fiscal.govBaseSpending.toDouble * in.priceLevel

    val (govCurrent, govCapital) =
      if p.flags.govInvest then
        val capShare = p.fiscal.govInvestShare.toDouble
        (govBaseRaw * (1.0 - capShare), govBaseRaw * capShare)
      else (govBaseRaw, 0.0)

    val totalSpend = in.unempBenefitSpend.toDouble + in.socialTransferSpend.toDouble +
      govCurrent + govCapital + in.debtService.toDouble + in.zusGovSubvention.toDouble + in.euCofinancing.toDouble
    val totalRev   = in.citPaid.toDouble + in.vat.toDouble + in.nbpRemittance.toDouble +
      in.exciseRevenue.toDouble + in.customsDutyRevenue.toDouble
    val deficit    = totalSpend - totalRev

    val newBondsOutstanding =
      if p.flags.govBondMarket then Math.max(0.0, in.prev.bondsOutstanding.toDouble + deficit)
      else in.prev.bondsOutstanding.toDouble

    val newCapitalStock =
      if p.flags.govInvest then
        val monthlyDepreciation = p.fiscal.govDepreciationRate.toDouble / 12.0
        in.prev.publicCapitalStock.toDouble * (1.0 - monthlyDepreciation) + govCapital + in.euProjectCapital.toDouble
      else 0.0

    GovState(
      taxRevenue = PLN(totalRev),
      deficit = PLN(deficit),
      cumulativeDebt = PLN(in.prev.cumulativeDebt.toDouble + deficit),
      unempBenefitSpend = in.unempBenefitSpend,
      bondsOutstanding = PLN(newBondsOutstanding),
      bondYield = in.prev.bondYield,
      debtServiceSpend = in.debtService,
      socialTransferSpend = in.socialTransferSpend,
      publicCapitalStock = PLN(newCapitalStock),
      govCurrentSpend = PLN(govCurrent),
      govCapitalSpend = PLN(govCapital + in.euProjectCapital.toDouble),
      euCofinancing = in.euCofinancing,
      exciseRevenue = in.exciseRevenue,
      customsDutyRevenue = in.customsDutyRevenue,
    )
