package sfc.config

import sfc.types.*

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise
  * data.
  */
object FirmSizeDistribution:
  import scala.util.Random

  def draw(rng: Random)(using p: SimParams): Int = p.pop.firmSizeDist match
    case FirmSizeDist.Gus     =>
      val r = rng.nextDouble()
      if r < p.pop.firmSizeMicroShare.toDouble then rng.between(1, 10)
      else if r < p.pop.firmSizeMicroShare.toDouble + p.pop.firmSizeSmallShare.toDouble then rng.between(10, 50)
      else if r < 1.0 - p.pop.firmSizeLargeShare.toDouble then rng.between(50, 250)
      else rng.between(250, p.pop.firmSizeLargeMax + 1)
    case FirmSizeDist.Uniform => p.pop.workersPerFirm
