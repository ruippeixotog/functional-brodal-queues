package net.ruippeixotog.structs

import java.util.NoSuchElementException

import BinomialQueue._

object BinomialQueue {
  def apply[T <% Ordered[T]](seq: T*): BinomialQueue[T] =
    seq.foldLeft(BinomialQueue(List[Node[T]]()))(_.insert(_))

  implicit object Factory extends QueueFactory[BinomialQueue] {
    def empty[T <% Ordered[T]] = BinomialQueue[T]()
  }

  case class Node[T <% Ordered[T]] private(root: T, rank: Int, children: List[Node[T]]) {
    def link(that: Node[T]): Node[T] = {
      require(rank == that.rank)
      if (root < that.root) Node(root, rank + 1, that :: children)
      else Node(that.root, that.rank + 1, this :: that.children)
    }
  }

  object Node {
    def apply[T <% Ordered[T]](e: T): Node[T] = Node(e, 0, Nil)
  }
}

case class BinomialQueue[T <% Ordered[T]](nodes: List[Node[T]])
  extends StrictlyTypedPriorityQueue[T, BinomialQueue[T]] {

  implicit private def asNodeList(q: BinomialQueue[T]) = q.nodes
  implicit private def asBinomialQueue(nodes: List[Node[T]]) = new BinomialQueue(nodes)

  def isEmpty = nodes.isEmpty

  def insert(e: T): BinomialQueue[T] = insert(Node(e))

  private def insert(n: Node[T]): BinomialQueue[T] = nodes match {
    case Nil => List(n)
    case t :: ts =>
      require(n.rank <= t.rank)
      if (n.rank < t.rank) n :: t :: ts else ts.insert(n.link(t))
  }

  def min: T = {
    def findMinAux(q: BinomialQueue[T], currMin: T): T = q.nodes match {
      case Nil => currMin
      case t :: ts => findMinAux(ts, if (currMin < t.root) currMin else t.root)
    }
    nodes match {
      case Nil => throw new NoSuchElementException
      case t :: ts => findMinAux(ts, t.root)
    }
  }

  def meld(that: BinomialQueue[T]): BinomialQueue[T] = (nodes, that.nodes) match {
    case (Nil, q) => q
    case (q, Nil) => q
    case (t1 :: ts1, t2 :: ts2) =>
      if (t1.rank < t2.rank) t1 :: (ts1 meld t2 :: ts2)
      else if (t1.rank > t2.rank) t2 :: (t1 :: ts1 meld ts2)
      else ts1 meld ts2 insert t1.link(t2)
  }

  def withoutMin: BinomialQueue[T] = {
    def getMin(q: BinomialQueue[T]): (Node[T], BinomialQueue[T]) = q.nodes match {
      case Nil => throw new NoSuchElementException
      case List(n) => (n, Nil)
      case t :: ts =>
        val (t2, ts2) = getMin(ts)
        if (t.root < t2.root) (t, ts) else (t2, t :: ts2)
    }
    val (Node(_, _, c), ts2) = getMin(nodes)
    c.reverse meld ts2
  }
}
