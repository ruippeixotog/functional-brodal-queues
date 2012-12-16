
trait QueueFactory[Q[_]] {
  def empty[T <% Ordered[T]]: Q[T]
}
