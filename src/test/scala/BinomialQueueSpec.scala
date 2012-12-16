
class BinomialQueueSpec extends PriorityQueueSpec[BinomialQueue] {

  def queueName = "binomial queue"
  def createQueue[T <% Ordered[T]](s: T*) = BinomialQueue(s: _*)
}