package sfc.engine

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.types.*

object Simulation:

  case class StepResult(
    world: World,
    firms: Array[Firm.State],
    households: Vector[Household.State],
    sfcCheck: Either[Vector[Sfc.SfcIdentityError], Unit],
  )

  def step(
    w: World,
    firms: Array[Firm.State],
    rc: RunConfig,
    households: Vector[Household.State],
  ): StepResult =
    val s1 = steps.FiscalConstraintStep.run(
      steps.FiscalConstraintStep.Input(
        month = w.month,
        gdpProxy = w.gdpProxy,
        gov = w.gov,
        priceLevel = w.priceLevel,
        minWageLevel = w.hh.minWageLevel,
        minWagePriceLevel = w.hh.minWagePriceLevel,
        marketWage = w.hh.marketWage,
        bankingSector = w.bankingSector,
        nbpReferenceRate = w.nbp.referenceRate.toDouble,
        expectedRate = w.expectations.expectedRate.toDouble,
        bdpAmount = rc.bdpAmount,
        isEurozone = rc.isEurozone,
      ),
    )
    val s2 = steps.LaborDemographicsStep.run(
      steps.LaborDemographicsStep.Input(w, firms, households, rc, s1),
    )
    val s3 = steps.HouseholdIncomeStep.run(
      steps.HouseholdIncomeStep.Input(w, firms, households, rc, s1, s2),
    )
    val s4 = steps.DemandStep.run(
      steps.DemandStep.Input(w, s2, s3),
    )
    val s5 = steps.FirmProcessingStep.run(
      steps.FirmProcessingStep.Input(w, firms, households, rc, s1, s2, s3, s4),
    )
    val s6 = steps.HouseholdFinancialStep.run(
      steps.HouseholdFinancialStep.Input(w, s1, s2, s3),
    )
    val s7 = steps.PriceEquityStep.run(
      steps.PriceEquityStep.Input(w, rc, s1, s2, s3, s4, s5),
    )
    val s8 = steps.OpenEconomyStep.run(
      steps.OpenEconomyStep.Input(w, rc, s1, s2, s3, s4, s5, s6, s7),
    )
    val s9 = steps.BankUpdateStep.run(
      steps.BankUpdateStep.Input(w, rc, s1, s2, s3, s4, s5, s6, s7, s8),
    )
    val s10 = steps.WorldAssemblyStep.run(
      steps.WorldAssemblyStep.Input(w, firms, households, rc, s1, s2, s3, s4, s5, s6, s7, s8, s9),
    )
    StepResult(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds, s10.sfcResult)
