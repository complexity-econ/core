package sfc.config

/** Foreign exchange market: PLN/EUR base rate, trade propensities, and interest rate parity.
  *
  * Models the bilateral PLN/EUR exchange rate with interest rate parity (IRP) adjustment, import propensity, technology
  * import channel, and export automation boost. Provides the exchange rate foundation used by OpenEconConfig,
  * GvcConfig, TourismConfig, and RemittanceConfig.
  *
  * `exportBase` is in raw PLN — scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param baseExRate
  *   PLN/EUR exchange rate at simulation start (NBP 2024: ~4.33)
  * @param foreignRate
  *   foreign (ECB) reference interest rate for IRP
  * @param importPropensity
  *   aggregate import-to-GDP ratio (GUS/NBP 2024: ~22%)
  * @param exportBase
  *   monthly export volume in raw PLN (scaled by gdpRatio)
  * @param techImportShare
  *   share of imports classified as technology/capital goods
  * @param irpSensitivity
  *   sensitivity of exchange rate to interest rate differential (IRP channel)
  * @param exRateAdjSpeed
  *   monthly exchange rate adjustment speed toward equilibrium
  * @param exportAutoBoost
  *   export productivity boost from firm automation
  */
case class ForexConfig(
  baseExRate: Double = 4.33,
  foreignRate: Double = 0.04,
  importPropensity: Double = 0.22,
  exportBase: Double = 55.4e9, // raw — scaled by gdpRatio
  techImportShare: Double = 0.40,
  irpSensitivity: Double = 0.15,
  exRateAdjSpeed: Double = 0.02,
  exportAutoBoost: Double = 0.15,
):
  require(baseExRate > 0, s"baseExRate must be positive: $baseExRate")
  require(importPropensity >= 0 && importPropensity <= 1.0, s"importPropensity must be in [0,1]: $importPropensity")
