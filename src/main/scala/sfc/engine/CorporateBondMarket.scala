package sfc.engine

import sfc.config.Config

/** Corporate bond market state: Catalyst + non-public issuance (#40). */
case class CorporateBondMarketState(
  outstanding: Double,          // total corp bonds outstanding
  bankHoldings: Double,         // banks' share of outstanding
  ppkHoldings: Double,          // PPK's share of outstanding
  otherHoldings: Double,        // other investors (off-model sink)
  corpBondYield: Double,        // current yield = govBondYield + spread
  lastIssuance: Double = 0.0,
  lastAmortization: Double = 0.0,
  lastCouponIncome: Double = 0.0,   // bank + ppk coupon income
  lastDefaultLoss: Double = 0.0,    // net loss after recovery
  lastDefaultAmount: Double = 0.0,  // gross default amount
  creditSpread: Double = 0.025      // current credit spread
)

object CorporateBondMarket:
  def zero: CorporateBondMarketState = CorporateBondMarketState(0, 0, 0, 0, 0)

  def initial: CorporateBondMarketState =
    val stock = Config.CorpBondInitStock
    CorporateBondMarketState(
      outstanding = stock,
      bankHoldings = stock * Config.CorpBondBankShare,
      ppkHoldings = stock * Config.CorpBondPpkShare,
      otherHoldings = stock * (1.0 - Config.CorpBondBankShare - Config.CorpBondPpkShare),
      corpBondYield = 0.06 + Config.CorpBondSpread,
      creditSpread = Config.CorpBondSpread
    )

  /** Compute current corporate bond yield = gov bond yield + credit spread.
    * Spread widens with system NPL (credit risk channel). */
  def computeYield(govBondYield: Double, nplRatio: Double): Double =
    val cyclicalSpread = Config.CorpBondSpread * (1.0 + nplRatio * 5.0)
    val spread = Math.min(0.10, cyclicalSpread)
    Math.max(0.01, govBondYield + spread)

  /** Monthly coupon income from corporate bond holdings.
    * Returns (totalCoupon, bankCoupon, ppkCoupon). */
  def computeCoupon(state: CorporateBondMarketState): (Double, Double, Double) =
    val totalCoupon = state.outstanding * state.corpBondYield / 12.0
    val bankCoupon = state.bankHoldings * state.corpBondYield / 12.0
    val ppkCoupon = state.ppkHoldings * state.corpBondYield / 12.0
    (totalCoupon, bankCoupon, ppkCoupon)

  /** Process defaults from bankrupt firms' bond debt.
    * Returns (grossDefault, lossAfterRecovery, bankLoss, ppkLoss). */
  def processDefaults(state: CorporateBondMarketState,
                      totalBondDefault: Double): (Double, Double, Double, Double) =
    if totalBondDefault <= 0 || state.outstanding <= 0 then return (0.0, 0.0, 0.0, 0.0)
    val defaultFrac = Math.min(1.0, totalBondDefault / state.outstanding)
    val bankDefault = state.bankHoldings * defaultFrac
    val ppkDefault = state.ppkHoldings * defaultFrac
    val lossAfterRecovery = totalBondDefault * (1.0 - Config.CorpBondRecovery)
    val bankLoss = bankDefault * (1.0 - Config.CorpBondRecovery)
    val ppkLoss = ppkDefault * (1.0 - Config.CorpBondRecovery)
    (totalBondDefault, lossAfterRecovery, bankLoss, ppkLoss)

  /** Monthly amortization: outstanding / maturity. */
  def amortization(state: CorporateBondMarketState): Double =
    state.outstanding / Math.max(1.0, Config.CorpBondMaturity)

  /** Process new issuance: allocate to holders proportionally. */
  def processIssuance(state: CorporateBondMarketState,
                      issuance: Double): CorporateBondMarketState =
    if issuance <= 0 then return state.copy(lastIssuance = 0.0)
    state.copy(
      outstanding = state.outstanding + issuance,
      bankHoldings = state.bankHoldings + issuance * Config.CorpBondBankShare,
      ppkHoldings = state.ppkHoldings + issuance * Config.CorpBondPpkShare,
      otherHoldings = state.otherHoldings + issuance *
        (1.0 - Config.CorpBondBankShare - Config.CorpBondPpkShare),
      lastIssuance = issuance
    )

  /** Full monthly step: yield -> coupon -> amortization -> default -> issuance. */
  def step(prev: CorporateBondMarketState, govBondYield: Double,
           nplRatio: Double, totalBondDefault: Double,
           totalBondIssuance: Double): CorporateBondMarketState =
    val newYield = computeYield(govBondYield, nplRatio)
    val (_, bankCoupon, ppkCoupon) = computeCoupon(prev)
    val amort = amortization(prev)
    val (grossDefault, _, _, _) = processDefaults(prev, totalBondDefault)
    // Subtract amortization + defaults from outstanding and holdings (proportional)
    val reduction = amort + grossDefault
    val reductionFrac = if prev.outstanding > 0 then
      Math.min(1.0, reduction / prev.outstanding) else 0.0
    val afterReduction = prev.copy(
      outstanding = Math.max(0.0, prev.outstanding - reduction),
      bankHoldings = Math.max(0.0, prev.bankHoldings - prev.bankHoldings * reductionFrac),
      ppkHoldings = Math.max(0.0, prev.ppkHoldings - prev.ppkHoldings * reductionFrac),
      otherHoldings = Math.max(0.0, prev.otherHoldings - prev.otherHoldings * reductionFrac),
      corpBondYield = newYield,
      creditSpread = Math.max(0.0, newYield - govBondYield),
      lastAmortization = amort,
      lastDefaultAmount = grossDefault,
      lastDefaultLoss = grossDefault * (1.0 - Config.CorpBondRecovery),
      lastCouponIncome = bankCoupon + ppkCoupon
    )
    // Add new issuance
    processIssuance(afterReduction, totalBondIssuance)
