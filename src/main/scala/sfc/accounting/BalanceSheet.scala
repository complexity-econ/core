package sfc.accounting

import sfc.types.*

case class GovState(
    taxRevenue: PLN,
    deficit: PLN,
    cumulativeDebt: PLN,
    unempBenefitSpend: PLN,
    bondsOutstanding: PLN = PLN.Zero,
    bondYield: Rate = Rate.Zero,
    debtServiceSpend: PLN = PLN.Zero,
    socialTransferSpend: PLN = PLN.Zero,
    publicCapitalStock: PLN = PLN.Zero,
    govCurrentSpend: PLN = PLN.Zero,
    govCapitalSpend: PLN = PLN.Zero,
    euCofinancing: PLN = PLN.Zero,
    exciseRevenue: PLN = PLN.Zero,
    customsDutyRevenue: PLN = PLN.Zero,
    minWageLevel: PLN = PLN(4666.0),
    minWagePriceLevel: Double = 1.0,
)
