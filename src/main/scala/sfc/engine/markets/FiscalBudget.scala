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

  /** Monthly budget update → new GovState. All monetary arguments in PLN. */
  def update(
      prev: GovState,
      citPaid: Double,
      vat: Double,
      priceLevel: Double,
      unempBenefitSpend: Double,
      debtService: Double = 0.0,
      nbpRemittance: Double = 0.0,
      zusGovSubvention: Double = 0.0,
      socialTransferSpend: Double = 0.0,
      euCofinancing: Double = 0.0,
      euProjectCapital: Double = 0.0,
      exciseRevenue: Double = 0.0,
      customsDutyRevenue: Double = 0.0,
      govPurchasesActual: Double = 0.0,
  )(using p: SimParams): GovState =
    val govBaseRaw = if govPurchasesActual > 0 then govPurchasesActual
                     else p.fiscal.govBaseSpending.toDouble * priceLevel

    val (govCurrent, govCapital) =
      if p.flags.govInvest then
        val capShare = p.fiscal.govInvestShare.toDouble
        (govBaseRaw * (1.0 - capShare), govBaseRaw * capShare)
      else (govBaseRaw, 0.0)

    val totalSpend = unempBenefitSpend + socialTransferSpend + govCurrent + govCapital +
      debtService + zusGovSubvention + euCofinancing
    val totalRev   = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit    = totalSpend - totalRev

    val newBondsOutstanding =
      if p.flags.govBondMarket then Math.max(0.0, prev.bondsOutstanding.toDouble + deficit)
      else prev.bondsOutstanding.toDouble

    val newCapitalStock =
      if p.flags.govInvest then
        val monthlyDepreciation = p.fiscal.govDepreciationRate.toDouble / 12.0
        prev.publicCapitalStock.toDouble * (1.0 - monthlyDepreciation) + govCapital + euProjectCapital
      else 0.0

    GovState(
      taxRevenue = PLN(totalRev),
      deficit = PLN(deficit),
      cumulativeDebt = PLN(prev.cumulativeDebt.toDouble + deficit),
      unempBenefitSpend = PLN(unempBenefitSpend),
      bondsOutstanding = PLN(newBondsOutstanding),
      bondYield = prev.bondYield,
      debtServiceSpend = PLN(debtService),
      socialTransferSpend = PLN(socialTransferSpend),
      publicCapitalStock = PLN(newCapitalStock),
      govCurrentSpend = PLN(govCurrent),
      govCapitalSpend = PLN(govCapital + euProjectCapital),
      euCofinancing = PLN(euCofinancing),
      exciseRevenue = PLN(exciseRevenue),
      customsDutyRevenue = PLN(customsDutyRevenue),
    )
