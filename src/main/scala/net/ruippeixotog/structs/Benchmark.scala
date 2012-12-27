package net.ruippeixotog.structs

import compat.Platform._
import scala.math._
import util.Random

import Complexity._

object Complexity {
  val const = { _: Int => 1.0 }
  val logN = { n: Int => log(n) }
  val linear = { n: Int => n }
  val nLogN = { n: Int => n * log(n) }

  def complexityTest(durations: Map[Int, Long], T: Int => Double): Seq[Double] =
    durations.toSeq.sortBy(_._1).map { case(n, dur) => dur / T(n) }
}

object Benchmark extends App {
  val factory = QueueBootstrap.Factory[SkewBinomialQueue]

  val outerSample = 100
  val innerSample = 100000

  def withBenchmark[T](text: => String)(f: => T): Long = {
    val start = currentTime
    f
    val duration = currentTime - start
    println(text + ": " + duration + " ms")
    duration
  }

  val results = (1 to outerSample).foldLeft(Map[Int, Map[Symbol, Seq[Long]]]()) { (acc, run) =>
    (1 to 7).foldLeft(acc) { (acc, order) =>

      val nElems = pow(10, order).toInt
      println("Starting run %d, %d elements".format(run, nElems))

      collectGarbage()

      val elems = (1 to nElems).map { _ => Random.nextInt() }
      val queue = factory.create[Int](elems: _*)

      val newElems = (1 to innerSample).map { _ => Random.nextInt() }
      val insertRes = withBenchmark("insert") {
        newElems.map { x => queue.insert(x) }
      }

      val minRes = withBenchmark("min") {
        (1 to innerSample).foreach { _ => queue.min }
      }

      val deleteRes = withBenchmark("deleteMin") {
        (1 to innerSample).foreach { _ => queue.withoutMin }
      }

      val bq2 = factory.create(elems: _*)
      val meldRes = withBenchmark("meld") {
        (1 to innerSample).foreach { _ => queue.meld(bq2) }
      }

      val orderRes = acc.getOrElse(nElems, Map())
      val newOrderRes = orderRes +
        ('insert -> (orderRes.getOrElse('insert, Seq[Long]()) :+ insertRes)) +
        ('min -> (orderRes.getOrElse('min, Seq[Long]()) :+ minRes)) +
        ('delete -> (orderRes.getOrElse('delete, Seq[Long]()) :+ deleteRes)) +
        ('meld -> (orderRes.getOrElse('meld, Seq[Long]()) :+ meldRes))

      acc + (nElems -> newOrderRes)
    }
  }

  val avgResults = results.mapValues { _.mapValues{ _.sum / outerSample }}

  println()
  println("----- Results -----")
  println("Note: the values shown here are for each %d operations".format(innerSample))
  for {
    (nElems, m) <- avgResults
    (op, res) <- m
  } println("Queue with %d elements, %s: %d ms".format(nElems, op, res))

  println()
  println("----- Complexity tests -----")
  val resultsByOp = (for {
    (nElems, m) <- avgResults
    (op, res) <- m
  } yield (op, nElems, res)).groupBy(_._1).mapValues(_.map(p => (p._2, p._3)).toMap)

  println("Ratios for insert: " + complexityTest(resultsByOp('insert), const).mkString(","))
  println("Ratios for min: " + complexityTest(resultsByOp('min), const).mkString(","))
  println("Ratios for delete: " + complexityTest(resultsByOp('delete), logN).mkString(","))
  println("Ratios for meld: " + complexityTest(resultsByOp('meld), const).mkString(","))
}
