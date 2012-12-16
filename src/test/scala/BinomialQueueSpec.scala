
class BinomialQueueSpec extends PriorityQueueSpec[BinomialQueue] {

  def queueName = "binomial queue"
  def emptyQueue[T <% Ordered[T]] = BinomialQueue()
}