import org.specs2.mutable._

import BinomialQueue._
import util.Random

class BinomialQueueSpec extends Specification {

  def extractMins[T <% Ordered[T]](q: BinomialQueue[T], nMins: Int): Seq[T] = {
    (1 to nMins).foldLeft((Seq[T](), q)) { case ((mins, qi), _) =>
        (mins :+ qi.findMin, qi.withoutMin)
    }._1
  }

  def randomSeq(n: Int) = (1 to n).map { _ => Random.nextInt() }

  "a binomial queue" should {
    "be created empty" in {
      BinomialQueue[Int]().findMin must throwA[NoSuchElementException]
    }

    "return the correct minimum" in {
      BinomialQueue(3, 4, 2, 6).findMin mustEqual 2

      val seq1 = randomSeq(1000)
      val q = BinomialQueue(seq1: _*)
      q.findMin mustEqual seq1.min
      extractMins(q, 1000) mustEqual seq1.sorted
    }

    "delete the minimum element" in {
      BinomialQueue(3, 4, 2, 6).withoutMin.findMin mustEqual 3

      val seq1 = randomSeq(1000)
      val q = BinomialQueue(seq1: _*)
      extractMins(q.withoutMin, 999) mustEqual seq1.sorted.tail
    }

    "insert correctly new elements" in {
      BinomialQueue(2).insert(1).findMin mustEqual 1

      val seq1 = randomSeq(1000)
      val newElem = Random.nextInt()
      val q = BinomialQueue(seq1: _*)
      extractMins(q.insert(newElem), 1001) mustEqual (seq1 :+ newElem).sorted
    }

    "be merged with another" in {
      val qm = BinomialQueue(2, 5, 6) meld BinomialQueue(3, 5, 7)
      extractMins(qm, 6) mustEqual Seq(2, 3, 5, 5, 6, 7)

      val seq1 = randomSeq(1000)
      val seq2 = randomSeq(1000)
      val q = BinomialQueue(seq1: _*)
      val q2 = BinomialQueue(seq2: _*)
      extractMins(q meld q2, 2000) mustEqual (seq1 ++ seq2).sorted
    }
  }
}
