sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(_, l, r) => 1 + size(l) + size(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(_, l, r) => depth(l).max(depth(r)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(e) => Leaf(f(e))
    case Branch(e, l, r) => Branch(f(e), map(l)(f), map(r)(f))
  }
}

def maximum(tree: Tree[Int]): Int = tree match {
  case Leaf(e) => e
  case Branch(e, l, r) => e.max(maximum(l)).max(maximum(r))
}

assert(Tree.size(Branch(5, Branch(7, Leaf(3), Leaf(4)), Branch(9, Leaf(1), Leaf(2)))) == 7)

assert(maximum(Branch(5, Branch(7, Leaf(3), Leaf(4)), Branch(9, Leaf(1), Leaf(2)))) == 9)

assert(Tree.depth(Branch(5, Branch(7, Branch(3, Leaf(11), Leaf(0)), Leaf(4)), Branch(9, Leaf(1), Leaf(2)))) == 4)

assert(Tree.map(Branch(5, Branch(7, Leaf(3), Leaf(4)), Branch(9, Leaf(1), Leaf(2))))(_.toString) == Branch("5", Branch("7", Leaf("3"), Leaf("4")), Branch("9", Leaf("1"), Leaf("2"))))