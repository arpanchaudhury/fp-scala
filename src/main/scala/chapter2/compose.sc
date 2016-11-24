def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

val identity = compose((x: Int) => Math.sqrt(x).toInt, (x: Int) => x * x)

assert(identity(5) == 5)
assert(identity(6) != 5)