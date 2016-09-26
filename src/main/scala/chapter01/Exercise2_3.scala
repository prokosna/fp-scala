package chapter01

/**
  * Created by prokosna on 16/09/27.
  */
object Exercise2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
}
