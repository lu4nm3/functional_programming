package functional.programming

/**
 * @author lmedina
 */
object Chapter_2 {
  def main(args: Array[String]) {
    println(fib(0))
    println(fib(1))
    println(fib(12))
    println(fib(24))
    println(fib(48))
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
