package sfc.config

/** Household agent parameters: wages, consumption, savings, debt, and consumer credit.
  *
  * Each household is an individual agent with heterogeneous MPC (Beta-distributed), log-normal savings and debt, skill
  * level subject to decay and scarring, and access to consumer credit. Rent is drawn from a truncated normal.
  *
  * @param baseWage
  *   mean monthly gross wage (PLN, GUS 2024: ~8,266 PLN)
  * @param baseReservationWage
  *   minimum acceptable wage — also the 2025 minimum wage level (Dz.U. 2024)
  * @param mpc
  *   mean marginal propensity to consume (aggregate target)
  * @param laborSupplySteepness
  *   slope of labor supply response to wage gap
  * @param wageAdjSpeed
  *   monthly wage Phillips-curve adjustment speed
  * @param count
  *   number of household agents (set to totalPopulation in SimParams.defaults)
  * @param savingsMu
  *   log-normal mean of initial savings distribution (ln PLN)
  * @param savingsSigma
  *   log-normal std dev of initial savings distribution
  * @param debtFraction
  *   fraction of households initialized with positive debt (BIK 2024: ~40%)
  * @param debtMu
  *   log-normal mean of initial debt distribution (ln PLN)
  * @param debtSigma
  *   log-normal std dev of initial debt distribution
  * @param rentMean
  *   mean monthly rent (PLN, Otodom/NBP 2024)
  * @param rentStd
  *   std dev of rent (PLN)
  * @param rentFloor
  *   minimum rent (PLN)
  * @param mpcAlpha
  *   Beta distribution alpha parameter for heterogeneous MPC
  * @param mpcBeta
  *   Beta distribution beta parameter for heterogeneous MPC
  * @param skillDecayRate
  *   monthly skill depreciation rate while unemployed
  * @param scarringRate
  *   additional monthly skill loss after `scarringOnset` months of unemployment
  * @param scarringCap
  *   maximum cumulative scarring penalty
  * @param scarringOnset
  *   months of unemployment before scarring begins
  * @param retrainingCost
  *   cost of retraining program (PLN)
  * @param retrainingDuration
  *   duration of retraining in months
  * @param retrainingBaseSuccess
  *   base probability of successful retraining (education-adjusted)
  * @param retrainingProb
  *   monthly probability of enrolling in retraining while unemployed
  * @param retrainingEnabled
  *   whether retraining mechanism is active
  * @param bankruptcyThreshold
  *   savings threshold (in multiples of monthly wage) below which household defaults
  * @param socialK
  *   Watts-Strogatz degree for household social network
  * @param socialP
  *   Watts-Strogatz rewiring probability for household network
  * @param debtServiceRate
  *   monthly debt service as fraction of outstanding debt
  * @param baseAmortRate
  *   monthly amortization rate on household debt
  * @param depositSpread
  *   spread below policy rate for household deposit remuneration
  * @param ccSpread
  *   consumer credit spread over policy rate (NBP MIR 2024)
  * @param ccMaxDti
  *   maximum debt-to-income ratio for consumer credit eligibility (KNF Recommendation T)
  * @param ccMaxLoan
  *   maximum consumer loan size (PLN)
  * @param ccAmortRate
  *   monthly amortization rate on consumer loans
  * @param ccNplRecovery
  *   recovery rate on defaulted consumer loans (BIK 2024)
  * @param ccEligRate
  *   fraction of employed households eligible for consumer credit each month
  */
case class HouseholdConfig(
  baseWage: Double = 8266.0,
  baseReservationWage: Double = 4666.0,
  mpc: Double = 0.82,
  laborSupplySteepness: Double = 8.0,
  wageAdjSpeed: Double = 0.12,
  // Household count (defaults to totalPopulation — set in SimParams.defaults)
  count: Int = 100000,
  // Savings distribution
  savingsMu: Double = 9.6,
  savingsSigma: Double = 1.2,
  // Debt
  debtFraction: Double = 0.40,
  debtMu: Double = 10.5,
  debtSigma: Double = 1.5,
  // Rent
  rentMean: Double = 1800.0,
  rentStd: Double = 400.0,
  rentFloor: Double = 800.0,
  // MPC distribution
  mpcAlpha: Double = 8.2,
  mpcBeta: Double = 1.8,
  // Skill decay & scarring
  skillDecayRate: Double = 0.02,
  scarringRate: Double = 0.02,
  scarringCap: Double = 0.50,
  scarringOnset: Int = 3,
  // Retraining
  retrainingCost: Double = 5000.0,
  retrainingDuration: Int = 6,
  retrainingBaseSuccess: Double = 0.60,
  retrainingProb: Double = 0.15,
  retrainingEnabled: Boolean = true,
  // Bankruptcy
  bankruptcyThreshold: Double = -3.0,
  // Social network
  socialK: Int = 10,
  socialP: Double = 0.15,
  // Debt service
  debtServiceRate: Double = 0.02,
  baseAmortRate: Double = 0.015,
  depositSpread: Double = 0.02,
  // Consumer credit
  ccSpread: Double = 0.04,
  ccMaxDti: Double = 0.40,
  ccMaxLoan: Double = 50000.0,
  ccAmortRate: Double = 0.025,
  ccNplRecovery: Double = 0.15,
  ccEligRate: Double = 0.30,
)
