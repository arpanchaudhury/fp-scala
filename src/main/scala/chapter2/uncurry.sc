def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)

val add = uncurry((a: Int) => (b: Int) => a + b)

assert(add(5, 9) == 14)
assert(!(add(5, 9) == 15))