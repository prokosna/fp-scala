package chapter02

/**
  * Created by prokosna on 16/09/27.
  */
object Exercise2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
