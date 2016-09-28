package chapter02

/**
  * Created by prokosna on 16/09/27.
  */
object Exercise2_1 {
  def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  def fibFix(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    var sum = 0
    var start = System.nanoTime()
    (1 to 30).foreach(i => sum += fibFix(30))
    println(System.nanoTime() - start)

    start = System.nanoTime()
    (1 to 30).foreach(i => sum += fib(30))
    println(System.nanoTime() - start)

    println(sum)
  }
}
