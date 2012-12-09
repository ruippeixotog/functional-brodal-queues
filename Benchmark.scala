
import collection.mutable.PriorityQueue
import compat.Platform._
import java.util.NoSuchElementException
import scala.math._
import util.Random

import BinomialQueue._

object Benchmark extends App {
  (1 to 7).map { order =>
    val elems = (1 to pow(10, order).toInt).map {
      _ => Random.nextInt(Integer.MAX_VALUE)
    }

    // insert
    val pq = withBenchmark("PriorityQueue, insert, " + pow(10, order).toInt + " elements") {
      elems.foldLeft(PriorityQueue[Int]()) { (acc, i) => acc += i; acc }
    }

    val bq = withBenchmark("BinomialQueue, insert, " + pow(10, order).toInt + " elements") {
      elems.foldLeft(BinomialQueue[Int]()) { (acc, i) => acc.insert(i) }
    }

    // deleteMin
    val pqCopy = pq.clone()
    withBenchmark("PriorityQueue, deleteMin, " + pow(10, order).toInt + " elements") {
      try {
        while(true) pq.dequeue()
      } catch {
        case _: NoSuchElementException =>
      }
    }

    def extractMins[T <% Ordered[T]](q: BinomialQueue[T]): Nothing =
      extractMins(q.withoutMin)

    withBenchmark("BinomialQueue, deleteMin, " + pow(10, order).toInt + " elements") {
      try {
        extractMins(bq)
      } catch {
        case _: NoSuchElementException =>
      }
    }

    // meld
    val elems2 = (1 to pow(10, order).toInt).map {
      _ => Random.nextInt(Integer.MAX_VALUE)
    }

    val pq2 = PriorityQueue(elems2: _*)
    withBenchmark("PriorityQueue, meld, " + pow(10, order).toInt + " elements") {
      pqCopy ++ pq2
    }

    val bq2 = BinomialQueue(elems2: _*)
    withBenchmark("BinomialQueue, meld, " + pow(10, order).toInt + " elements") {
      bq.meld(bq2)
    }
  }

  def withBenchmark[T](text: String)(f: => T): T = {
    val start = currentTime
    val res = f
    println(text + ": " + (currentTime - start) + " ms")
    res
  }
}
