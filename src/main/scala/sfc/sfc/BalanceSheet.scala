package sfc.sfc

import sfc.config.Config

case class GovState(
  bdpActive: Boolean,
  taxRevenue: Double,
  bdpSpending: Double,
  deficit: Double,
  cumulativeDebt: Double
)

case class BankState(
  totalLoans: Double,
  nplAmount: Double,
  capital: Double,
  deposits: Double
):
  def nplRatio: Double = if totalLoans > 1.0 then nplAmount / totalLoans else 0.0
  def car: Double = if totalLoans > 1.0 then capital / totalLoans else 10.0
  def lendingRate(refRate: Double): Double =
    refRate + Config.BaseSpread + Math.min(0.15, nplRatio * Config.NplSpreadFactor)
  def canLend(amount: Double): Boolean =
    val projected = capital / (totalLoans + amount)
    projected >= Config.MinCar

case class ForexState(
  exchangeRate: Double,
  imports: Double,
  exports: Double,
  tradeBalance: Double,
  techImports: Double
)
