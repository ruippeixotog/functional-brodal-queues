
trait StrictlyTypedPriorityQueue[T, Q <: StrictlyTypedPriorityQueue[T, Q]] {
  def insert(e: T): Q
  def min: T
  def meld(q: Q): Q
  def withoutMin: Q
}
