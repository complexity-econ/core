package sfc.config

import sfc.engine.markets.SectoralMobility

/** Labor market: sectoral mobility, trade unions, and forward-looking expectations.
  *
  * Three interacting mechanisms: (1) sectoral labor mobility with friction matrix (GUS LFS 2024, Shimer 2005), (2)
  * trade unions with per-sector density, wage premia, and downward rigidity (GUS 2024), and (3) forward-looking
  * inflation/wage expectations with adaptive learning and central bank credibility (Carroll 2003, Bewley 1999, NBP
  * target band).
  *
  * @param frictionMatrix
  *   sector-to-sector transition friction matrix (6x6, higher = harder to move)
  * @param frictionDurationMult
  *   multiplier on transition duration (scales friction matrix)
  * @param frictionCostMult
  *   wage penalty during sectoral transition (fraction of wage lost)
  * @param voluntarySearchProb
  *   monthly probability of employed worker searching for better sector
  * @param voluntaryWageThreshold
  *   minimum wage gap (fraction) to trigger voluntary search
  * @param vacancyWeight
  *   weight of vacancy rate in sectoral attractiveness
  * @param adjacentFrictionMax
  *   maximum friction for adjacent sectors (caps transition difficulty)
  * @param unionDensity
  *   per-sector union membership density (6 sectors, GUS 2024)
  * @param unionWagePremium
  *   wage premium for unionized workers (empirical: ~8%)
  * @param unionRigidity
  *   downward nominal wage rigidity imposed by unions (0-1 scale)
  * @param expLambda
  *   adaptive learning rate for inflation expectations (Carroll 2003)
  * @param expCredibilityInit
  *   initial central bank credibility (0-1 scale)
  * @param expCredibilitySpeed
  *   monthly credibility adjustment speed
  * @param expCredibilityThreshold
  *   inflation deviation threshold for credibility loss (pp)
  * @param expWagePassthrough
  *   pass-through of inflation expectations to wage demands
  * @param expBondSensitivity
  *   sensitivity of bond yields to inflation expectations
  */
case class LaborConfig(
  // Sectoral mobility (GUS LFS 2024, Shimer 2005)
  frictionMatrix: Vector[Vector[Double]] = SectoralMobility.DefaultFrictionMatrix,
  frictionDurationMult: Double = 1.0,
  frictionCostMult: Double = 0.5,
  voluntarySearchProb: Double = 0.02,
  voluntaryWageThreshold: Double = 0.20,
  vacancyWeight: Double = 2.0,
  adjacentFrictionMax: Double = 0.4,
  // Unions (GUS 2024)
  unionDensity: Vector[Double] = Vector(0.02, 0.15, 0.03, 0.12, 0.30, 0.04),
  unionWagePremium: Double = 0.08,
  unionRigidity: Double = 0.50,
  // Expectations (Carroll 2003, Bewley 1999)
  expLambda: Double = 0.70,
  expCredibilityInit: Double = 0.80,
  expCredibilitySpeed: Double = 0.05,
  expCredibilityThreshold: Double = 0.02,
  expWagePassthrough: Double = 0.50,
  expBondSensitivity: Double = 0.50,
):
  require(unionDensity.length == 6, s"unionDensity must have 6 sectors: ${unionDensity.length}")
