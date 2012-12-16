
object QueueBootstrap {
  def apply[T <% Ordered[T], Q <: StrictlyTypedPriorityQueue[T, Q]](q: Q): QueueBootstrap[T, Q] =
    if (q.isEmpty) EmptyBootstrap[T, Q](q)
    else NonEmptyBootstrap(q, q.min)
}

trait QueueBootstrap[T, Q <: StrictlyTypedPriorityQueue[T, Q]]
  extends StrictlyTypedPriorityQueue[T, QueueBootstrap[T, Q]] {
  val queue: Q
}

case class EmptyBootstrap[T <% Ordered[T], Q <: StrictlyTypedPriorityQueue[T, Q]](queue: Q)
  extends QueueBootstrap[T, Q] {

  def isEmpty: Boolean = true

  def insert(e: T): QueueBootstrap[T, Q] = NonEmptyBootstrap(queue.insert(e), e)

  def min: T = throw new NoSuchElementException

  def meld(q2: QueueBootstrap[T, Q]): QueueBootstrap[T, Q] =
    if(q2.isEmpty) EmptyBootstrap(queue meld q2.queue)
    else NonEmptyBootstrap(queue meld q2.queue, q2.min)

  def withoutMin: QueueBootstrap[T, Q] = throw new NoSuchElementException
}

case class NonEmptyBootstrap[T <% Ordered[T], Q <: StrictlyTypedPriorityQueue[T, Q]](queue: Q, minElem: T)
  extends QueueBootstrap[T, Q] {

  def isEmpty: Boolean = false

  def insert(e: T): QueueBootstrap[T, Q] =
    NonEmptyBootstrap(queue.insert(e), (if (e < minElem) e else minElem))

  def min: T = minElem

  def meld(q2: QueueBootstrap[T, Q]): QueueBootstrap[T, Q] =
    if(q2.isEmpty) NonEmptyBootstrap(queue meld q2.queue, minElem)
    else NonEmptyBootstrap(queue meld q2.queue, (if (q2.min < minElem) q2.min else minElem))

  def withoutMin: QueueBootstrap[T, Q] = {
    val newQueue = queue.withoutMin
    if (newQueue.isEmpty) EmptyBootstrap(newQueue)
    else NonEmptyBootstrap(newQueue, newQueue.min)
  }
}
