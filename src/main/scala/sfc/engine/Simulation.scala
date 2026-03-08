package sfc.engine

import sfc.accounting.*
import sfc.agents.*
import sfc.config.*
import sfc.types.*

// ---------------------------------------------------------------------------
// Simulation — monthly step function for the SFC-ABM engine
// ---------------------------------------------------------------------------
//
// This object is the top-level orchestrator of the simulation. Each call to
// `step` advances the economy by one month, executing a fixed 10-stage
// pipeline that mirrors the real-world sequence of economic decisions:
//
//   s1  FiscalConstraintStep    — fiscal rules, minimum wage, lending base rate
//   s2  LaborDemographicsStep   — labor market clearing, wages, demographics, ZUS/PPK
//   s3  HouseholdIncomeStep     — HH income, consumption, PIT, sectoral mobility
//   s4  DemandStep              — per-sector demand multipliers, aggregate demand
//   s5  FirmProcessingStep      — production, I-O, technology adoption, loans, NPL
//   s6  HouseholdFinancialStep  — mortgages, consumer credit, remittances, tourism
//   s7  PriceEquityStep         — inflation, price level, GPW equity market, sigma dynamics
//   s8  OpenEconomyStep         — BoP, forex, GVC trade, monetary policy, bonds, QE, NBFI
//   s9  BankUpdateStep          — bank P&L, provisioning, CAR, interbank, deposit rates
//   s10 WorldAssemblyStep       — assemble new World + SFC validation (13 identities)
//
// The pipeline is strictly sequential: each step's Input case class carries
// typed references to all prior step Outputs it needs (e.g. s7 receives
// s1–s5). This makes data dependencies explicit at the type level and
// eliminates the field-unpacking boilerplate that would otherwise dominate
// the orchestrator.
//
// The final step (s10) assembles the updated World, reassigns households to
// firms, and runs the SFC accounting check (see Sfc.validate). If any of
// the 13 balance-sheet identities is violated, the check returns Left with
// detailed error information — the simulation halts immediately in Main.
//
// No business logic lives here — every calculation is delegated to a Step
// object in the `steps` package. This file is pure wiring.
//
// When to modify this file:
//   - Adding a new pipeline stage: insert a new sN call and thread its
//     Output to downstream steps that need it.
//   - Adding a new field to an existing Step's Input: no change here —
//     the Step's Input already receives the full Output objects.
//   - Changing step ordering: reorder the sN calls (rare — ordering
//     reflects real-world causality).
//
// See also:
//   - Sfc.scala          — the 13 SFC identities and MonthlyFlows
//   - WorldAssemblyStep  — final state assembly + SFC check invocation
//   - Sectors.scala       — labor market, inflation, Taylor rule, forex, gov
//   - Main.scala          — simulation loop that calls step() each month
// ---------------------------------------------------------------------------
object Simulation:

  /** Result of a single monthly simulation step.
    *
    * Bundles the updated simulation state together with the outcome of the SFC accounting check. The caller (Main)
    * should inspect `sfcCheck` and halt on `Left` — a failed identity means a monetary flow was mis-routed or omitted.
    */
  case class StepResult(
    world: World, // updated World state (all macro aggregates, institutional sectors)
    firms: Array[Firm.State], // post-step firm array (same length; bankrupt slots may be recycled)
    households: Vector[Household.State], // post-step households (reassigned to living firms)
    sfcCheck: Either[Vector[Sfc.SfcIdentityError], Unit], // Right(()) if all 13 identities hold
  )

  /** Advance the economy by one month.
    *
    * Executes the 10-stage pipeline (s1–s10) in causal order. Each stage receives typed Output references from all
    * prior stages it depends on — no intermediate unpacking is needed. The dependency DAG is:
    *
    * {{{
    *   s1 ──┬──────────────────────────────────────────────────────────────→ s9, s10
    *        ├→ s2 ──┬──────────────────────────────────────────────────────→ s9, s10
    *        │       ├→ s3 ──┬──────────────────────────────────────────────→ s9, s10
    *        │       │       ├→ s4 ──┬──────────────────────────────────────→ s9, s10
    *        │       │       │       ├→ s5 ──┬──────────────────────────────→ s9, s10
    *        │       │       │       │       ├→ s7 ─→ s8 ─→ s9 ─→ s10
    *        │       │       ├→ s6 ──┼───────┼──────→ s8
    *        │       │       │       │       │
    *        └───────┴───────┴───────┴───────┴──────────────────────────────→ s10 (World assembly)
    * }}}
    *
    * @param w
    *   current World state (read-only within the step; a new World is assembled in s10)
    * @param firms
    *   current firm array (mutated only by FirmProcessingStep and PriceEquityStep's rewiring)
    * @param rc
    *   run configuration (currency regime, BDP amount, time horizon)
    * @param households
    *   current household vector (updated by HouseholdIncomeStep and HouseholdFinancialStep)
    * @return
    *   StepResult with updated state and SFC check outcome
    */
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
