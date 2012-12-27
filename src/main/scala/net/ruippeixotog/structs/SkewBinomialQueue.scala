package net.ruippeixotog.structs

import annotation.tailrec
import SkewBinomialQueue._

object SkewBinomialQueue {
  def apply[T <% Ordered[T]](seq: T*): SkewBinomialQueue[T] =
    seq.foldLeft(SkewBinomialQueue(List[Node[T]]()))(_.insert(_))

  implicit object Factory extends QueueFactory[SkewBinomialQueue] {
    def empty[T <% Ordered[T]] = SkewBinomialQueue[T]()
  }

  case class Node[T <% Ordered[T]] private(root: T, rank: Int, children: List[Node[T]]) {
    def link(that: Node[T]): Node[T] = {
      require(rank == that.rank)
      if (root < that.root) Node(root, rank + 1, that :: children)
      else Node(that.root, that.rank + 1, this :: that.children)
    }

    def skewLink(left: Node[T], right: Node[T]): Node[T] = {
      require(rank == 0 && left.rank == right.rank)
      if (left.root < right.root && left.root < root)
        Node(left.root, left.rank + 1, this :: right :: left.children)
      else if (right.root < root)
        Node(right.root, right.rank + 1, this :: left :: right.children)
      else Node(root, left.rank + 1, List(left, right))
    }
  }

  object Node {
    def apply[T <% Ordered[T]](e: T): Node[T] = Node(e, 0, Nil)
  }
}

case class SkewBinomialQueue[T <% Ordered[T]](nodes: List[Node[T]])
  extends StrictlyTypedPriorityQueue[T, SkewBinomialQueue[T]] {

  implicit private def asNodeList(q: SkewBinomialQueue[T]) = q.nodes
  implicit private def asSkewBinomialQueue(nodes: List[Node[T]]) =
    new SkewBinomialQueue(nodes)

  def isEmpty = nodes.isEmpty

  def insert(e: T): SkewBinomialQueue[T] = nodes match {
    case t1 :: t2 :: rest if t1.rank == t2.rank =>
      Node(e).skewLink(t1, t2) :: rest
    case ts => Node(e) :: ts
  }

  @tailrec
  private def insert(n: Node[T]): SkewBinomialQueue[T] = nodes match {
    case Nil => List(n)
    case t :: ts =>
      require(n.rank <= t.rank)
      if (n.rank < t.rank) n :: t :: ts else ts.insert(n.link(t))
  }

  def meldUniq(that: SkewBinomialQueue[T]): SkewBinomialQueue[T] =
    (nodes, that.nodes) match {
      case (Nil, q) => q
      case (q, Nil) => q
      case (t1 :: ts1, t2 :: ts2) =>
        if (t1.rank < t2.rank) t1 :: (ts1 meldUniq t2 :: ts2)
        else if (t1.rank > t2.rank) t2 :: (t1 :: ts1 meldUniq ts2)
        else ts1 meldUniq ts2 insert t1.link(t2)
    }

  def meld(that: SkewBinomialQueue[T]): SkewBinomialQueue[T] = (nodes, that.nodes) match {
    case (t1 :: ts1, t2 :: ts2) => ts1.insert(t1) meldUniq ts2.insert(t2)
    case _ => this meldUniq that
  }

  def min: T = {
    @tailrec
    def findMinAux(q: List[Node[T]], currMin: T): T = q match {
      case Nil => currMin
      case t :: ts => findMinAux(ts, if (currMin < t.root) currMin else t.root)
    }
    nodes match {
      case Nil => throw new NoSuchElementException
      case t :: ts => findMinAux(ts, t.root)
    }
  }

  def withoutMin: SkewBinomialQueue[T] = {
    def getMin(q: SkewBinomialQueue[T]): (Node[T], SkewBinomialQueue[T]) = q.nodes match {
      case Nil => throw new NoSuchElementException
      case List(n) => (n, Nil)
      case t :: ts =>
        val (t2, ts2) = getMin(ts)
        if (t.root < t2.root) (t, ts) else (t2, t :: ts2)
    }

    @tailrec
    def split(q: List[Node[T]],
              trees: List[Node[T]],
              singletons: List[T]): (SkewBinomialQueue[T], List[T]) = q match {
      case Nil => (trees, singletons)
      case t :: ts =>
        if(t.rank == 0) split(ts, trees, t.root :: singletons)
        else split(ts, t :: trees, singletons)
    }

    val (Node(_, _, c), ts2) = getMin(nodes)
    val (ts0, singletons) = split(c, Nil, Nil)
    singletons.foldLeft(ts0 meld ts2)(_ insert _)
  }
}
