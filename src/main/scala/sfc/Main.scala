package sfc

import sfc.accounting.Sfc
import sfc.config.*
import sfc.engine.*
import sfc.init.WorldInit
import sfc.types.*

/** Result of a single simulation run. */
case class RunResult(
  timeSeries: TimeSeries,
  terminalState: Simulation.SimState,
)

/** Run one simulation with given seed. Throws [[Sfc.SfcViolationException]] on any SFC identity violation. */
def runSingle(seed: Int, rc: RunConfig): RunResult =
  val init = WorldInit.initialize(seed, rc)
  var state = Simulation.SimState(init.world, init.firms, init.households)

  val results = Array.ofDim[Double](Config.Duration, Observables.nCols)

  for t <- 0 until Config.Duration do
    val stepResult = Simulation.step(state, rc)
    stepResult.sfcCheck match
      case Left(errors) => throw Sfc.SfcViolationException(t + 1, errors)
      case Right(())    => // OK
    state = stepResult.state
    results(t) = Observables.compute(t, state.world, state.firms, state.households)

  RunResult(TimeSeries.wrap(results), state)

// ---- Monte Carlo main entry point ----

@main def sfcMonteCarlo(
  bdpAmountStr: String,
  nSeedsStr: String,
  outputPrefix: String,
  regimeStr: String = "pln",
): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds = nSeedsStr.toInt
  val regime = regimeStr.toLowerCase match
    case "eur" | "euro" | "ecb" => MonetaryRegime.Eur
    case _                      => MonetaryRegime.Pln
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix, regime)
  McRunner.run(rc)
