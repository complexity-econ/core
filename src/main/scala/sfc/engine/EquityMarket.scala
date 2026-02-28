package sfc.engine

import sfc.config.Config

/** GPW equity market state: aggregate index, market cap, yields, foreign ownership. */
case class EquityMarketState(
  index: Double,            // WIG-like composite index level
  marketCap: Double,        // total market capitalization
  earningsYield: Double,    // E/P = inverse of P/E
  dividendYield: Double,    // dividend / price
  foreignOwnership: Double, // fraction held by foreign investors
  lastIssuance: Double = 0.0,          // equity issuance this month
  lastDomesticDividends: Double = 0.0, // net domestic dividends this month
  lastForeignDividends: Double = 0.0,  // foreign dividend outflow this month
  lastDividendTax: Double = 0.0,       // dividend tax this month
  hhEquityWealth: Double = 0.0,        // total HH equity holdings value
  lastWealthEffect: Double = 0.0,      // wealth effect consumption boost this month
  monthlyReturn: Double = 0.0          // index return this month (for HH revaluation next month)
)

object EquityMarket:
  def zero: EquityMarketState = EquityMarketState(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  def initial: EquityMarketState = EquityMarketState(
    index = Config.GpwInitIndex,
    marketCap = Config.GpwInitMcap,
    earningsYield = 1.0 / Config.GpwPeMean,
    dividendYield = Config.GpwDivYield,
    foreignOwnership = Config.GpwForeignShare
  )

  /** Monthly equity market step using Gordon growth model for equilibrium price.
    * P = D / (r - g), where:
    *   D = dividend yield × current index
    *   r = discount rate = refRate + equity risk premium
    *   g = expected earnings growth (proxy: gdpGrowth)
    *
    * @param prev previous equity market state
    * @param refRate central bank reference rate (annual)
    * @param inflation current annualized inflation
    * @param gdpGrowth GDP growth rate (monthly, relative to last period)
    * @param firmProfits total firm profits this month
    * @return updated equity market state
    */
  def step(prev: EquityMarketState, refRate: Double, inflation: Double,
           gdpGrowth: Double, firmProfits: Double): EquityMarketState =
    if !Config.GpwEnabled then return zero

    // Equity risk premium: mean-reverting around 5% (GPW historical)
    val equityRiskPremium = 0.05
    // Discount rate: risk-free + ERP (annual)
    val discountRate = Math.max(0.02, refRate + equityRiskPremium)
    // Expected growth: proxy from GDP growth (annualized) capped to avoid singularity
    val expectedGrowth = Math.max(-0.10, Math.min(discountRate - 0.01, gdpGrowth * 12.0))

    // Gordon growth fundamental value
    val dividend = prev.dividendYield * prev.index
    val denominator = discountRate - expectedGrowth
    val gordonIndex = if denominator > 0.005 then dividend / denominator
                      else prev.index  // fallback: hold steady if r ≈ g

    // Partial adjustment: index converges to fundamental (monthly smoothing)
    val adjustmentSpeed = 0.15
    val newIndex = Math.max(100.0, prev.index + adjustmentSpeed * (gordonIndex - prev.index))

    // Market cap scales with index
    val indexReturn = if prev.index > 0 then newIndex / prev.index else 1.0
    val newMarketCap = Math.max(0.0, prev.marketCap * indexReturn)

    // Earnings yield from firm profits and market cap
    val annualProfits = firmProfits * 12.0
    val newEarningsYield = if newMarketCap > 0 then
      Math.max(0.01, Math.min(0.50, annualProfits / newMarketCap))
    else prev.earningsYield

    // Dividend yield: payout ratio × earnings yield (mean-reverting to calibrated)
    val payoutRatio = 0.57  // GPW average
    val impliedDivYield = newEarningsYield * payoutRatio
    val newDivYield = prev.dividendYield * 0.9 + impliedDivYield * 0.1

    // Foreign ownership: slow-moving, mean-reverting to calibrated share
    val newForeignOwnership = prev.foreignOwnership * 0.99 + Config.GpwForeignShare * 0.01

    // Monthly return for HH revaluation (used next month)
    val mReturn = if prev.index > 0 then newIndex / prev.index - 1.0 else 0.0

    EquityMarketState(newIndex, newMarketCap, newEarningsYield, newDivYield, newForeignOwnership,
      monthlyReturn = mReturn)

  /** Process equity issuance: firm raises CAPEX via equity, increasing market cap.
    * Returns updated state with diluted index (supply effect). */
  def processIssuance(amount: Double, prev: EquityMarketState): EquityMarketState =
    if amount <= 0 then return prev.copy(lastIssuance = 0.0)
    // New shares increase market cap; index diluted by supply effect
    val dilutionFactor = prev.marketCap / (prev.marketCap + amount)
    prev.copy(
      marketCap = prev.marketCap + amount,
      index = prev.index * dilutionFactor,
      lastIssuance = amount
    )

  /** Compute dividends from firm profits.
    * @return (domesticDividends, foreignDividends, dividendTax) */
  /** Compute dividends from market cap and yields.
    * @return (netDomesticDividends, foreignDividends, dividendTax) */
  def computeDividends(firmProfits: Double, divYield: Double, marketCap: Double,
                       foreignShare: Double): (Double, Double, Double) =
    if marketCap <= 0 then return (0.0, 0.0, 0.0)
    // Total monthly dividends: divYield is annual, so /12
    val totalDividends = divYield * marketCap / 12.0
    val foreignDividends = totalDividends * foreignShare
    val domesticDividends = totalDividends - foreignDividends
    // Belka tax on domestic dividends (19%)
    val dividendTax = domesticDividends * Config.GpwDivTax
    val netDomesticDividends = domesticDividends - dividendTax
    (netDomesticDividends, foreignDividends, dividendTax)
