def partial1[A,B,C](a: A, f: (A,B) => C): B => C = f(a, _)

def length(s: String) = partial1[String, Int, Boolean](s, (a: String, b: Int) => a.length == b)

assert(length("test")(4))
assert(!length("test")(5))
