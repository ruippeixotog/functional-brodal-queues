
class SkewBinomialQueueSpec extends PriorityQueueSpec[SkewBinomialQueue] {

  def queueName = "skew binomial queue"
  def emptyQueue[T <% Ordered[T]] = SkewBinomialQueue()
}
