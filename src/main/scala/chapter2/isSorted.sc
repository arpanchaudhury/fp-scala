def isSorted[A](as: List[A], f: (A,A) => Boolean): Boolean = as match {
  case Nil | List(_) => true
  case x :: xs => f(x, xs.head) && isSorted(xs, f)
}

assert(isSorted(List(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))
assert(isSorted(List(6, 5, 3), (a: Int, b: Int) => a > b))
assert(!isSorted(List(1, 6, 3, 4, 5), (a: Int, b: Int) => a < b))
assert(!isSorted(List(4, 5, 3), (a: Int, b: Int) => a > b))
