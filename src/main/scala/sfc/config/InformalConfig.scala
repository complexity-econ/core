package sfc.config

/** Shadow economy and tax evasion: counter-cyclical informal sector dynamics.
  *
  * Models the Polish informal economy (Schneider 2023: 20-25% of GDP) with 4-channel tax evasion: firm-level CIT
  * evasion, and aggregate VAT/PIT/excise evasion scaled by per-sector shadow shares. The informal share is
  * counter-cyclical — rises with unemployment (lagged, smoothed). Affects SFC Identity 3 (government budget).
  *
  * @param sectorShares
  *   per-sector shadow economy share of output (6 sectors, Schneider 2023: Agri 35%, Retail 30%, etc.)
  * @param citEvasion
  *   fraction of shadow output evading CIT (firm-level channel)
  * @param vatEvasion
  *   fraction of shadow output evading VAT (aggregate channel)
  * @param pitEvasion
  *   fraction of shadow employment evading PIT
  * @param exciseEvasion
  *   fraction of shadow output evading excise duties
  * @param unempThreshold
  *   unemployment rate threshold for counter-cyclical activation
  * @param cyclicalSens
  *   sensitivity of shadow share to excess unemployment above threshold
  * @param smoothing
  *   exponential smoothing parameter for lagged unemployment (higher = more inertia)
  */
case class InformalConfig(
  sectorShares: Vector[Double] = Vector(0.05, 0.15, 0.30, 0.20, 0.02, 0.35),
  citEvasion: Double = 0.80,
  vatEvasion: Double = 0.90,
  pitEvasion: Double = 0.85,
  exciseEvasion: Double = 0.70,
  unempThreshold: Double = 0.05,
  cyclicalSens: Double = 0.50,
  smoothing: Double = 0.92,
):
  require(sectorShares.length == 6, s"sectorShares must have 6 sectors: ${sectorShares.length}")
