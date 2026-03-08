package sfc.config

/** Global Value Chain integration: sectoral trade structure, ER pass-through, and demand shocks.
  *
  * Models Poland's deep integration into EU/global supply chains (WIOD/OECD ICIO), with per-sector export shares, GVC
  * depth (backward linkages), differentiated EUR/non-EUR exchange rate pass-through (Campa & Goldberg 2005), foreign
  * demand shocks by sector, and supply chain disruption recovery. EU trade share ~70% of total (GUS/NBP 2024).
  *
  * @param euTradeShare
  *   share of total trade with EU partners (GUS/NBP 2024: ~70%)
  * @param exportShares
  *   per-sector share of total exports (6 sectors, GUS 2024)
  * @param depth
  *   per-sector GVC depth / backward linkage ratio (WIOD/OECD ICIO)
  * @param foreignInflation
  *   annual foreign (trading partner) inflation rate (ECB/IMF)
  * @param foreignGdpGrowth
  *   annual foreign GDP growth rate (ECB/IMF projections)
  * @param erPassthrough
  *   exchange rate pass-through to non-EU import prices (Campa & Goldberg 2005)
  * @param euErPassthrough
  *   exchange rate pass-through to EU import prices (lower due to EUR invoicing)
  * @param demandShockMonth
  *   simulation month when external demand shock hits (0 = no shock)
  * @param demandShockSize
  *   magnitude of demand shock (fraction of export demand affected)
  * @param demandShockSectors
  *   set of sector indices affected by demand shock
  * @param disruptionRecovery
  *   monthly recovery rate from supply chain disruption
  */
case class GvcConfig(
  euTradeShare: Double = 0.70,
  exportShares: Vector[Double] = Vector(0.05, 0.55, 0.15, 0.03, 0.02, 0.20),
  depth: Vector[Double] = Vector(0.35, 0.75, 0.30, 0.40, 0.10, 0.45),
  foreignInflation: Double = 0.02,
  foreignGdpGrowth: Double = 0.015,
  erPassthrough: Double = 0.60,
  euErPassthrough: Double = 0.15,
  demandShockMonth: Int = 0,
  demandShockSize: Double = 0.0,
  demandShockSectors: Set[Int] = Set.empty,
  disruptionRecovery: Double = 0.05,
):
  require(exportShares.length == 6, s"exportShares must have 6 sectors: ${exportShares.length}")
  require(depth.length == 6, s"depth must have 6 sectors: ${depth.length}")
