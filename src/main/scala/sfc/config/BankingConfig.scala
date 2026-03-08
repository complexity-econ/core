package sfc.config

/** Commercial banking system: balance sheets, credit risk, LCR/NSFR, macroprudential, and KNF/BFG supervision.
  *
  * Models a multi-bank system (7 banks by default, calibrated to KNF 2024) with heterogeneous balance sheets, credit
  * spreads, NPL dynamics, capital adequacy (Basel III CRR), liquidity coverage (LCR/NSFR), macroprudential buffers
  * (CCyB, O-SII), KNF BION/SREP P2R add-ons, BFG resolution levy and bail-in, and interbank market.
  *
  * Stock values (`initCapital`, `initDeposits`, etc.) are in raw PLN — scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param initCapital
  *   initial aggregate bank equity (KNF 2024: ~270 mld PLN)
  * @param initDeposits
  *   initial aggregate deposits (NBP M3 2024: ~1,900 mld PLN)
  * @param initLoans
  *   initial aggregate corporate loans (NBP 2024: ~700 mld PLN)
  * @param initGovBonds
  *   initial commercial bank government bond holdings (NBP 2024: ~400 mld PLN)
  * @param initNbpGovBonds
  *   initial NBP government bond holdings (NBP 2024: ~300 mld PLN)
  * @param initConsumerLoans
  *   initial consumer loan stock (BIK 2024: ~200 mld PLN)
  * @param baseSpread
  *   base lending spread over policy rate
  * @param nplSpreadFactor
  *   spread increase per unit NPL ratio
  * @param minCar
  *   minimum capital adequacy ratio (Basel III CRR: 8%)
  * @param loanRecovery
  *   loss-given-default recovery rate on corporate loans
  * @param profitRetention
  *   fraction of bank profits retained as capital
  * @param reserveReq
  *   required reserve ratio (NBP 2024: 3.5%)
  * @param stressThreshold
  *   CAR threshold below which bank enters stress mode
  * @param lcrMin
  *   minimum Liquidity Coverage Ratio (Basel III: 100%)
  * @param nsfrMin
  *   minimum Net Stable Funding Ratio (Basel III: 100%)
  * @param demandDepositRunoff
  *   LCR assumption: fraction of demand deposits that may run off in 30 days
  * @param termDepositFrac
  *   fraction of deposits that are term (stable for NSFR purposes)
  * @param p2rAddons
  *   per-bank BION/SREP P2R capital add-ons (KNF 2024, 7 banks)
  * @param bfgLevyRate
  *   annual BFG resolution fund levy as fraction of deposits (BFG 2024)
  * @param bailInDepositHaircut
  *   fraction of uninsured deposits bailed-in during resolution
  * @param bfgDepositGuarantee
  *   BFG deposit guarantee limit per depositor (PLN, BFG: 400,000)
  * @param ccybMax
  *   maximum countercyclical capital buffer (KNF 2024: 2.5%)
  * @param ccybActivationGap
  *   credit/GDP gap threshold to activate CCyB
  * @param ccybReleaseGap
  *   credit/GDP gap threshold to release CCyB
  * @param osiiPkoBp
  *   O-SII buffer for PKO BP (KNF 2024: 1.0%)
  * @param osiiPekao
  *   O-SII buffer for Pekao (KNF 2024: 0.5%)
  * @param concentrationLimit
  *   single-name concentration limit as fraction of capital (Art. 395 CRR: 25%)
  */
case class BankingConfig(
  // Initial balance sheet (raw — scaled by gdpRatio in SimParams.defaults)
  initCapital: Double = 270e9,
  initDeposits: Double = 1900e9,
  initLoans: Double = 700e9,
  initGovBonds: Double = 400e9,
  initNbpGovBonds: Double = 300e9,
  initConsumerLoans: Double = 200e9,
  // Spreads & risk
  baseSpread: Double = 0.015,
  nplSpreadFactor: Double = 5.0,
  minCar: Double = 0.08,
  loanRecovery: Double = 0.30,
  profitRetention: Double = 0.30,
  reserveReq: Double = 0.035,
  stressThreshold: Double = 0.05,
  // LCR/NSFR (Basel III)
  lcrMin: Double = 1.0,
  nsfrMin: Double = 1.0,
  demandDepositRunoff: Double = 0.10,
  termDepositFrac: Double = 0.40,
  // KNF/BFG
  p2rAddons: Vector[Double] = Vector(0.015, 0.010, 0.030, 0.015, 0.020, 0.025, 0.020),
  bfgLevyRate: Double = 0.0024,
  bailInDepositHaircut: Double = 0.08,
  bfgDepositGuarantee: Double = 400000.0,
  // Macroprudential (KNF 2024)
  ccybMax: Double = 0.025,
  ccybActivationGap: Double = 0.02,
  ccybReleaseGap: Double = -0.02,
  osiiPkoBp: Double = 0.01,
  osiiPekao: Double = 0.005,
  concentrationLimit: Double = 0.25,
):
  require(minCar > 0 && minCar < 1.0, s"minCar must be in (0,1): $minCar")
  require(initCapital >= 0, s"initCapital must be non-negative: $initCapital")
  require(initDeposits >= 0, s"initDeposits must be non-negative: $initDeposits")
  require(lcrMin > 0, s"lcrMin must be positive: $lcrMin")
  require(nsfrMin > 0, s"nsfrMin must be positive: $nsfrMin")
