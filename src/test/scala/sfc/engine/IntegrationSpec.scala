package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, RunConfig}
import sfc.runSingle

class IntegrationSpec extends AnyFlatSpec with Matchers:

  // Use small firm count via env override if set, otherwise default
  private val rc = RunConfig(2000.0, 1, "test")

  "runSingle" should "complete without exception" in {
    noException should be thrownBy runSingle(42, rc)
  }

  it should "produce 120 rows x 19 columns" in {
    val results = runSingle(42, rc)
    results.length shouldBe Config.Duration
    for row <- results do
      row.length shouldBe 19
  }

  it should "have Month column = 1..120" in {
    val results = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      results(t)(0) shouldBe (t + 1).toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    val results = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      results(t)(3) should be >= 0.0
      results(t)(3) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    val results = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      results(t)(2) should be >= 0.0
      results(t)(2) should be <= 1.0
  }

  it should "be reproducible with the same seed" in {
    val r1 = runSingle(42, rc)
    val r2 = runSingle(42, rc)
    for t <- 0 until Config.Duration; c <- 0 until 19 do
      r1(t)(c) shouldBe r2(t)(c)
  }
