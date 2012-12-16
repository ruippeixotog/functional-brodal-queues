
class SkewBinomialQueueSpec extends PriorityQueueSpec[SkewBinomialQueue] {

  def queueName = "skew binomial queue"
  def createQueue[T <% Ordered[T]](s: T*) = SkewBinomialQueue(s: _*)
}
