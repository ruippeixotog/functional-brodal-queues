import java.util.NoSuchElementException

// import scala.annotation.tailrec

object BinomialQueue {
  type BinomialQueue[T] = List[Node[T]]

  implicit def asBinomialQueue[T <% Ordered[T]](q: BinomialQueue[T]) =
    new BinomialQueueOps(q)

  def apply[T <% Ordered[T]](seq: T*): BinomialQueue[T] =
    seq.foldLeft(List[Node[T]]())(_.insert(_))

  final class BinomialQueueOps[T <% Ordered[T]](self: BinomialQueue[T]) {

    def insert(e: T): BinomialQueue[T] = insert(Node(e, 0, Nil))

    def insert(n: Node[T]): BinomialQueue[T] = self match {
      case Nil => List(n)
      case h :: t => require(n.rank <= h.rank)
      if (n.rank < h.rank) n :: h :: t else t.insert(n.link(h))
    }

    def meld(that: BinomialQueue[T]): BinomialQueue[T] = (self, that) match {
      case (Nil, q) => q
      case (q, Nil) => q
      case (t1 :: ts1, t2 :: ts2) =>
        if (t1.rank < t2.rank) t1 :: (ts1 meld t2 :: ts2)
        else if (t1.rank > t2.rank) t2 :: (t1 :: ts1 meld ts2)
        else ts1 meld ts2 insert t1.link(t2)
    }

    def findMin: T = {
      def findMinAux(q: BinomialQueue[T], currMin: T): T = q match {
        case Nil => currMin
        case t :: ts => findMinAux(ts, if (currMin < t.root) currMin else t.root)
      }
      self match {
        case Nil => throw new NoSuchElementException
        case t :: ts => findMinAux(ts, t.root)
      }
    }

    def withoutMin: BinomialQueue[T] = {
      def getMin(q: BinomialQueue[T]): (Node[T], BinomialQueue[T]) = q match {
        case Nil => throw new NoSuchElementException
        case List(n) => (n, Nil)
        case t :: ts =>
          val (t2, ts2) = getMin(ts)
          if (t.root < t2.root) (t, ts) else (t2, t :: ts2)
      }
      val (Node(_, _, c), ts2) = getMin(self)
      c.reverse meld ts2
    }
  }

}

case class Node[T <% Ordered[T]](root: T, rank: Int, children: List[Node[T]]) {
  def link(that: Node[T]): Node[T] = {
    require(rank == that.rank)
    if (root < that.root) Node(root, rank + 1, that :: children)
    else Node(that.root, that.rank + 1, this :: that.children)
  }
}
