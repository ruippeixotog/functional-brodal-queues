
class BootstrappedSkewBinomialQueueSpec
  extends PriorityQueueSpec[({ type λ[T] = QueueBootstrap[T, SkewBinomialQueue[T]] })#λ] {

  def queueName = "bootstrapped skew binomial queue"
  def emptyQueue[T <% Ordered[T]] = QueueBootstrap(SkewBinomialQueue())
}
