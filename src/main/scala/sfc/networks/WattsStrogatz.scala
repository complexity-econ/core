package sfc.networks

import sfc.agents.{Firm, TechState}

import scala.util.Random

object Network:
  /** Generate Watts-Strogatz small-world adjacency for N nodes */
  def wattsStrogatz(n: Int, k: Int, p: Double): Array[Array[Int]] =
    val adj = Array.fill(n)(scala.collection.mutable.Set.empty[Int])

    // Ring lattice: connect each node to k/2 nearest neighbors on each side
    val halfK = k / 2
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val right = (i + j) % n
      val left  = (i - j + n) % n
      adj(i) += right
      adj(right) += i
      adj(i) += left
      adj(left) += i

    // Rewire with probability p
    for
      i <- 0 until n
      j <- 1 to halfK
    do
      val target = (i + j) % n
      if Random.nextDouble() < p && adj(i).size < n - 1 then
        // Find a random node not already connected
        var newTarget = Random.nextInt(n)
        var attempts = 0
        while (newTarget == i || adj(i).contains(newTarget)) && attempts < 20 do
          newTarget = Random.nextInt(n)
          attempts += 1
        if newTarget != i && !adj(i).contains(newTarget) then
          adj(i) -= target
          adj(target) -= i
          adj(i) += newTarget
          adj(newTarget) += i

    adj.map(_.toArray)

  /** Compute local automation ratio among neighbors */
  def localAutoRatio(firm: Firm, firms: Array[Firm]): Double =
    val neighbors = firm.neighbors
    if neighbors.isEmpty then return 0.0
    val autoCount = neighbors.count { nid =>
      val nf = firms(nid)
      nf.tech.isInstanceOf[TechState.Automated] || nf.tech.isInstanceOf[TechState.Hybrid]
    }
    autoCount.toDouble / neighbors.length
