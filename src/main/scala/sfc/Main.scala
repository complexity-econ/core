package sfc

import sfc.config.*

// $COVERAGE-OFF$ entry point only
@main def sfcMonteCarlo(
    nSeedsStr: String,
    outputPrefix: String,
): Unit =
  given SimParams = SimParams.defaults
  val nSeeds      = nSeedsStr.toInt
  val rc          = RunConfig(nSeeds, outputPrefix)
  McRunner.run(rc)
// $COVERAGE-ON$
