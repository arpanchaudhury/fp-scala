sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, list: List[A]) extends List[A]


object List {
  def empty[A] = Nil

  def apply[A](elements: A*): List[A] = {
    if (elements.isEmpty) Nil
    else Cons(elements.head, apply(elements.tail: _*))
  }

  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case Cons(x, xs: List[A]) => 1 + length(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("Error: tail of empty list invocation")
    case Cons(x, xs: List[A]) => xs
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    list match {
      case `list` if n == 0 => `list`
      case Cons(x, xs: List[A]) if n <= length(list) => drop(xs, n - 1)
      case _ => sys.error(s"There are less than $n elements in the list")
    }
  }

  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = {
    list match {
      case Cons(x, xs: List[A]) if predicate(x) => dropWhile(xs)(predicate)
      case _ | Nil => list
    }
  }

  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Nil => list
      case Cons(x, xs: List[A]) => Cons(head, xs)
    }
  }

  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs: List[A]) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs: List[A]) => foldLeft(xs, f(x, z))(f)
    }
  }

  def size[A](list: List[A]): Int = foldRight(list, 0)((a, z) => z + 1)

  def reverse[A](list: List[A]): List[A] =
    foldLeft[A, List[A]](list, Nil)((x, y) => Cons(x, y))

  def append[A](list: List[A], element: A): List[A] =
    foldRight[A, List[A]](list, Cons(element, Nil))((x, y) => Cons(x, y))

  def concat[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case Cons(x, xs) => Cons(x, concat(xs, list2))
  }

  def flatten[A](list: List[List[A]]): List[A] =
    foldRight(list, Nil: List[A])(concat(_, _))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match {
    case Nil => Nil
    case Cons(x, xs) => concat(f(x), flatMap(xs)(f))
  }

  def combine[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    (list1, list2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), combine(xs, ys)(f))
      case _ => Nil
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = ???
}

assert(List.tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))

assert(List.drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))

assert(List.dropWhile(List(1, 2, 3, 4, 5))(_ <= 3) == List(4, 5))

assert(List.setHead(List(1, 2, 3, 4, 5), 0) == List(0, 2, 3, 4, 5))

assert(List.init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))

List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

assert(List.size(List(1, 2, 3, 4)) == 4)

assert(List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)

def sum(list: List[Int]) = List.foldLeft(list, 0.0)(_ + _)

assert(sum(List(1, 2, 3, 4)) == 10)

def product(list: List[Int]) = List.foldLeft(list, 1.0)(_ * _)

assert(product(List(1, 2, 3, 4)) == 24)

def length[A](list: List[A]) = List.foldLeft(list, 0)((x, z) => z + 1)

assert(length(List(1, 2, 3, 4)) == 4)

assert(List.reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))

assert(List.append(List(1, 2, 3, 4), 5) == List(1, 2, 3, 4, 5))

assert(List.flatten(List(List(1, 2), Nil, List(3))) == List(1, 2, 3))

assert(List.map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))

assert(List.map(List(1, 2, 3))(_.toString) == List("1", "2", "3"))

assert(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))

assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

def filter[A](list: List[A])(f: A => Boolean): List[A] = List.flatMap(list) {
  case e if f(e) => Cons(e, Nil)
  case _ => Nil
}

assert(filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))

assert(List.combine(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))



