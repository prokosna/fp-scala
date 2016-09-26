/**
  * Created by prokosna on 16/09/27.
  */
object Test {

  def main(args: Array[String]): Unit = {
    println(factorial(10))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }
}