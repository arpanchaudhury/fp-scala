def fibonacci(index: Int): Int = {

  def loop(previous: Int, current: Int, index: Int): Int = {
    if (index == 1) previous
    else if (index == 2) current
    else loop(current, previous + current, index - 1)
  }

  loop(0, 1, index)
}



assert(fibonacci(1) == 0)
assert(fibonacci(2) == 1)
assert(fibonacci(3) == 1)
assert(fibonacci(4) == 2)
assert(fibonacci(5) == 3)
assert(fibonacci(6) == 5)
assert(fibonacci(7) == 8)