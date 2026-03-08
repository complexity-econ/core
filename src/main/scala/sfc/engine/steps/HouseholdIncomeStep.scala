package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, RunConfig}
import sfc.engine.{LaborMarket, SectoralMobility, World}
import sfc.types.*

import scala.util.Random

object HouseholdIncomeStep:

  case class Input(
    employed: Int,
    newWage: Double,
    bdp: Double,
    bdpActive: Boolean,
    resWage: Double,
    w: World,
    households: Vector[Household.State],
    firms: Array[Firm.State],
    lendingBaseRate: Double,
    newZus: SocialSecurity.ZusState,
    rc: RunConfig,
  )

  case class Output(
    totalIncome: Double,
    consumption: Double,
    importCons: Double,
    domesticCons: Double,
    updatedHouseholds: Vector[Household.State],
    hhAgg: Household.Aggregates,
    perBankHhFlowsOpt: Option[PerBankHhFlows],
    pitRevenue: Double,
    importAdj: Double,
    aggUnempBenefit: Double,
  )

  def run(in: Input): Output =
    val importAdj = Config.ImportPropensity *
      Math.pow(Config.BaseExRate / in.w.forex.exchangeRate, 0.5)

    val afterSep = LaborMarket.separations(in.households, in.firms, in.firms)
    val afterWages = LaborMarket.updateWages(afterSep, in.newWage)
    val bsec = in.w.bankingSector
    val nBanksHh = bsec.banks.length
    val hhBankRates = Some(
      BankRates(
        lendingRates =
          bsec.banks.zip(bsec.configs).map((b, cfg) => Banking.lendingRate(b, cfg, in.lendingBaseRate)).toArray,
        depositRates = bsec.banks.map(_ => Banking.hhDepositRate(in.w.nbp.referenceRate.toDouble)).toArray,
      ),
    )
    val eqReturn = in.w.equity.monthlyReturn.toDouble
    val secWages = if Config.LmSectoralMobility then Some(SectoralMobility.sectorWages(afterWages)) else None
    val secVacancies =
      if Config.LmSectoralMobility then Some(SectoralMobility.sectorVacancies(afterWages, in.firms)) else None
    val (newHhs, agg, pbf) = Household.step(
      afterWages,
      in.w,
      in.bdp,
      in.newWage,
      in.resWage,
      importAdj,
      Random,
      nBanksHh,
      hhBankRates,
      eqReturn,
      secWages,
      secVacancies,
    )

    val pitRevenue =
      if Config.PitEnabled then agg.totalPit.toDouble
      else 0.0

    Output(
      agg.totalIncome.toDouble,
      agg.consumption.toDouble,
      agg.importConsumption.toDouble,
      agg.domesticConsumption.toDouble,
      newHhs,
      agg,
      pbf,
      pitRevenue,
      importAdj,
      aggUnempBenefit = 0.0,
    )
