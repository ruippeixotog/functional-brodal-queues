package net.ruippeixotog.structs


trait QueueFactory[Q[U] <: StrictlyTypedPriorityQueue[U, Q[U]]] {
  def empty[T <% Ordered[T]]: Q[T]
  def create[T <% Ordered[T]](elems: T*) = elems.foldLeft(empty)(_ insert _)
}
