import scala.language.postfixOps

case object CustomNone extends CustomOption[Nothing] {
  def isDefined = false
}

case class CustomSome[A](value: A) extends CustomOption[A] {
  def isDefined = true
}

sealed trait CustomOption[+A] {

  def isDefined: Boolean

  def map[B](f: A => B): CustomOption[B] = this match {
    case CustomSome(v) => CustomSome(f(v))
    case CustomNone => CustomNone
  }

  def flatMap[B](f: A => CustomOption[B]): CustomOption[B] = {
    val customOption: CustomOption[CustomOption[B]] = this map f
    customOption getOrElse CustomNone
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case CustomSome(v) => v
    case CustomNone => default
  }

  def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B] =
    if (this.isDefined) this else ob

  def filter(f: A => Boolean): CustomOption[A] = {
    if(this.map(f).getOrElse(false)) this else CustomNone
  }
}

assert(CustomSome(5).map(_.toString) == CustomSome("5"))

assert(CustomNone.map(_.toString) == CustomNone)

assert(CustomSome(5).flatMap(x => CustomSome(x * 3)) == CustomSome(15))

assert(CustomNone.flatMap(x => CustomSome(x toString)) == CustomNone)

assert(CustomNone.getOrElse(5) == 5)

assert(CustomSome(10).getOrElse(5) == 10)



