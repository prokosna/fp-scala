package chapter01

/**
  * Created by prokosna on 16/09/27.
  */
object Exercise2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.sliding(2).foreach {
      case Array(a, b) =>
        if (!ordered(a, b)) return false
    }
    true
  }

  def isSortedTrue[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  def main(args: Array[String]): Unit = {
    var start = System.nanoTime()
    println(isSorted[Int]((0 to 1000000).toArray, (a, b) => a <= b))
    println(System.nanoTime() - start)

    start = System.nanoTime()
    println(isSortedTrue[Int]((0 to 1000000).toArray, (a, b) => a <= b))
    println(System.nanoTime() - start)
  }
}
