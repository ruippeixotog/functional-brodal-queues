package net.ruippeixotog.structs

import util.Random
import org.specs2.mutable._

abstract class PriorityQueueSpec[Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]](
  implicit factory: QueueFactory[Q]) extends Specification {

  def queueName: String

  def extractMins[T](q: Q[T], nMins: Int): Seq[T] = {
    (1 to nMins).foldLeft((Seq[T](), q)) { case ((mins, qi), _) =>
      (mins :+ qi.min, qi.withoutMin)
    }._1
  }

  def randomSeq(n: Int) = (1 to n).map { _ => Random.nextInt() }

  ("a " + queueName) should {
    "be created empty" in {
      factory.empty[Int].isEmpty must beTrue
      factory.empty[Int].min must throwA[NoSuchElementException]
    }

    "return the correct minimum" in {
      factory.create(3, 4, 2, 6).isEmpty must beFalse
      factory.create(3, 4, 2, 6).min mustEqual 2

      val seq1 = randomSeq(1000)
      val q = factory.create(seq1: _*)
      q.min mustEqual seq1.min
    }

    "delete the minimum element" in {
      factory.create(3, 4, 2, 6).withoutMin.min mustEqual 3

      val seq1 = randomSeq(1000)
      val q = factory.create(seq1: _*)
      extractMins(q.withoutMin, 999) mustEqual seq1.sorted.tail
    }

    "insert correctly new elements" in {
      factory.create(2).insert(1).min mustEqual 1

      val seq1 = randomSeq(1000)
      val newElem = Random.nextInt()
      val q = factory.create(seq1: _*)
      extractMins(q.insert(newElem), 1001) mustEqual (seq1 :+ newElem).sorted
    }

    "be merged with another" in {
      val qm = factory.create(2, 5, 6) meld factory.create(3, 5, 7)
      extractMins(qm, 6) mustEqual Seq(2, 3, 5, 5, 6, 7)

      val seq1 = randomSeq(1000)
      val seq2 = randomSeq(1000)
      val q = factory.create(seq1: _*)
      val q2 = factory.create(seq2: _*)
      extractMins(q meld q2, 2000) mustEqual (seq1 ++ seq2).sorted
    }
  }
}
