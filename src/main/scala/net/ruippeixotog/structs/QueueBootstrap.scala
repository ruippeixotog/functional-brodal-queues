package net.ruippeixotog.structs


object QueueBootstrap {
  def apply[T, Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]](
    implicit tOrdered: T => Ordered[T], factory: QueueFactory[Q]): QueueBootstrap[T, Q] = EmptyBootstrap[T, Q]

  implicit def Factory[Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]](implicit innerFact: QueueFactory[Q]) =
    new QueueFactory[({ type λ[T] = QueueBootstrap[T, Q] })#λ] {
      def empty[T <% Ordered[T]] = QueueBootstrap[T, Q]
    }
}

trait QueueBootstrap[T, Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]]
  extends StrictlyTypedPriorityQueue[T, QueueBootstrap[T, Q]]

case class EmptyBootstrap[T, Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]](
  implicit tOrdered: T => Ordered[T], factory: QueueFactory[Q])
  extends QueueBootstrap[T, Q] {

  def isEmpty: Boolean = true

  def insert(e: T): QueueBootstrap[T, Q] = NonEmptyBootstrap[T, Q](factory.empty, e)

  def min: T = throw new NoSuchElementException

  def meld(q2: QueueBootstrap[T, Q]): QueueBootstrap[T, Q] = q2

  def withoutMin: QueueBootstrap[T, Q] = throw new NoSuchElementException
}

case class NonEmptyBootstrap[T, Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]](queue: Q[NonEmptyBootstrap[T, Q]], minElem: T)(
  implicit tOrdered: T => Ordered[T], factory: QueueFactory[Q])
  extends QueueBootstrap[T, Q] with Ordered[NonEmptyBootstrap[T, Q]] {

  def isEmpty: Boolean = false

  def insert(e: T): QueueBootstrap[T, Q] =
    this meld NonEmptyBootstrap(factory.empty, e)

  def min: T = minElem

  def meld(q2: QueueBootstrap[T, Q]): QueueBootstrap[T, Q] = q2 match {
    case q2 @ NonEmptyBootstrap(q2q, q2min) =>
      if (q2min < minElem) NonEmptyBootstrap(q2q.insert(this), q2min)
      else NonEmptyBootstrap(queue.insert(q2), minElem)
    case _ => this
  }

  def withoutMin: QueueBootstrap[T, Q] = {
    if(queue.isEmpty) EmptyBootstrap()
    else {
      val minQueue = queue.min
      NonEmptyBootstrap(minQueue.queue meld queue.withoutMin, minQueue.min)
    }
  }

  def compare(that: NonEmptyBootstrap[T, Q]): Int = minElem compare that.minElem
}
