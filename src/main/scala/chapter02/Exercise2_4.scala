package chapter02

/**
  * Created by prokosna on 16/09/27.
  */
object Exercise2_4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}
