def curry[A, B, C](f :(A, B) => C): A => B => C = (x: A) => f(x, _)

val add = curry((a: Int, b: Int) => a + b)

assert(add(9)(5) == 14)
assert(add(-1)(5) != 3)