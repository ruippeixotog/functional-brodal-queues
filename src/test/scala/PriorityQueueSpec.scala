import util.Random
import org.specs2.mutable._

trait PriorityQueueSpec[Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]] extends Specification {

  def queueName: String
  def emptyQueue[T <% Ordered[T]]: Q[T]
  def createQueue[T <% Ordered[T]](s: T*) = s.foldLeft(emptyQueue)(_ insert _)

  def extractMins[T](q: Q[T], nMins: Int): Seq[T] = {
    (1 to nMins).foldLeft((Seq[T](), q)) { case ((mins, qi), _) =>
      (mins :+ qi.min, qi.withoutMin)
    }._1
  }

  def randomSeq(n: Int) = (1 to n).map { _ => Random.nextInt() }

  ("a " + queueName) should {
    "be created empty" in {
      emptyQueue[Int].isEmpty must beTrue
      emptyQueue[Int].min must throwA[NoSuchElementException]
    }

    "return the correct minimum" in {
      createQueue(3, 4, 2, 6).isEmpty must beFalse
      createQueue(3, 4, 2, 6).min mustEqual 2

      val seq1 = randomSeq(1000)
      val q = createQueue(seq1: _*)
      q.min mustEqual seq1.min
    }

    "delete the minimum element" in {
      createQueue(3, 4, 2, 6).withoutMin.min mustEqual 3

      val seq1 = randomSeq(1000)
      val q = createQueue(seq1: _*)
      extractMins(q.withoutMin, 999) mustEqual seq1.sorted.tail
    }

    "insert correctly new elements" in {
      createQueue(2).insert(1).min mustEqual 1

      val seq1 = randomSeq(1000)
      val newElem = Random.nextInt()
      val q = createQueue(seq1: _*)
      extractMins(q.insert(newElem), 1001) mustEqual (seq1 :+ newElem).sorted
    }

    "be merged with another" in {
      val qm = createQueue(2, 5, 6) meld createQueue(3, 5, 7)
      extractMins(qm, 6) mustEqual Seq(2, 3, 5, 5, 6, 7)

      val seq1 = randomSeq(1000)
      val seq2 = randomSeq(1000)
      val q = createQueue(seq1: _*)
      val q2 = createQueue(seq2: _*)
      extractMins(q meld q2, 2000) mustEqual (seq1 ++ seq2).sorted
    }
  }
}
