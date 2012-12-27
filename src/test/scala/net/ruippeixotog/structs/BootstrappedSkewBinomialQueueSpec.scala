package net.ruippeixotog.structs

class BootstrappedSkewBinomialQueueSpec
  extends PriorityQueueSpec[({ type λ[T] = QueueBootstrap[T, SkewBinomialQueue] })#λ] {
  def queueName = "bootstrapped skew binomial queue"
}
