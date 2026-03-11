package sfc.engine.mechanisms

import sfc.config.SimParams
import sfc.util.KahanSum.*

/** Tax revenue computation: VAT, excise, customs, and informal-economy evasion
  * adjustments.
  *
  * Extracted from BankUpdateStep to separate fiscal revenue logic from banking
  * balance-sheet updates. All flows are monthly, in PLN.
  */
object TaxRevenue:

  case class Input(
      consumption: Double,
      pitRevenue: Double,
      totalImports: Double,
      informalCyclicalAdj: Double,
  )

  case class Output(
      vat: Double,
      vatAfterEvasion: Double,
      pitAfterEvasion: Double,
      exciseRevenue: Double,
      exciseAfterEvasion: Double,
      customsDutyRevenue: Double,
      effectiveShadowShare: Double,
  )

  def compute(in: Input)(using p: SimParams): Output =
    val vat = in.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.vatRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum

    val exciseRevenue = in.consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.exciseRates.map(_.toDouble))
      .map((w, r) => w * r)
      .kahanSum

    val customsDutyRevenue =
      if p.flags.openEcon then in.totalImports * p.fiscal.customsNonEuShare.toDouble * p.fiscal.customsDutyRate.toDouble
      else 0.0

    // Informal economy: aggregate tax evasion
    val effectiveShadowShare =
      if p.flags.informal then
        p.fiscal.fofConsWeights
          .map(_.toDouble)
          .zip(p.informal.sectorShares.map(_.toDouble))
          .map((cw, ss) => cw * Math.min(1.0, ss + in.informalCyclicalAdj))
          .kahanSum
      else 0.0

    val vatAfterEvasion =
      if p.flags.informal then vat * (1.0 - effectiveShadowShare * p.informal.vatEvasion.toDouble) else vat

    val exciseAfterEvasion =
      if p.flags.informal then exciseRevenue * (1.0 - effectiveShadowShare * p.informal.exciseEvasion.toDouble)
      else exciseRevenue

    val pitAfterEvasion =
      if p.flags.informal then in.pitRevenue * (1.0 - effectiveShadowShare * p.informal.pitEvasion.toDouble)
      else in.pitRevenue

    Output(
      vat = vat,
      vatAfterEvasion = vatAfterEvasion,
      pitAfterEvasion = pitAfterEvasion,
      exciseRevenue = exciseRevenue,
      exciseAfterEvasion = exciseAfterEvasion,
      customsDutyRevenue = customsDutyRevenue,
      effectiveShadowShare = effectiveShadowShare,
    )
