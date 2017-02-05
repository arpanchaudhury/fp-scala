def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(val_a => b.map(val_b => f(val_a, val_b)))

assert(map2(Some(5), Some(4))((a, b) => Math.pow(a, b)) == Some(625))

import java.util.regex._

import scala.util.Try

def pattern(s: String): Option[Pattern] =
  try {
    Some(Pattern.compile(s))
  } catch {
    case _: PatternSyntaxException => None
  }

def mkMatcher(pat: String): Option[String => Boolean] =
  pattern(pat) map (p => (s: String) => p.matcher(s).matches)

def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
  map2(mkMatcher(pat1), mkMatcher(pat2))((m1, m2) => m1(s) && m2(s))

assert(bothMatch(".*A.*", ".*B.*", "AB") == Some(true))

assert(bothMatch(".*A.*", ".*C.*", "AB") == Some(false))

assert(bothMatch("*A.*", ".*C.*", "AB") == None)

def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(List.empty[A])
    case Some(x) :: xOpts => sequence(xOpts).map(x :: _)
    case _ => None
  }


sequence(List(Some(1), Some(2), Some(3)))

assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))

assert(sequence(List(Some(1), None, Some(3))) == None)


def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(List.empty[B])
    case x :: xs if f(x).isDefined => traverse(xs)(f).map(f(x).get :: _)
    case _ => None
  }

def toInt(s: String) = Try(s.toInt).toOption

assert(traverse(List("1", "s"))(toInt) == None)

assert(traverse(List("1", "5"))(toInt) == Some(List(1, 5)))

def seq[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](a)(identity)

assert(seq(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))

assert(seq(List(Some(1), None, Some(3))) == None)





