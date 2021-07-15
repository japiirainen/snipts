package retry

object Fibonacci {
  def fibonacci(n: Int): Long =
    if (n > 1)
      fib(n)._1
    else
      0

  private def fib(n: Int): (Long, Long) = n match {
    case 0 => (0, 1)
    case m =>
      val (a, b) = fib(m / 2)
      val c      = a * (b * 2 - 2)
      val d      = a * a * b * b
      if (n % 2 == 0)
        (c, d)
      else
        (d, c * d)
  }
}
