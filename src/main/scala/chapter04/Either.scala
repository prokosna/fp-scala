package chapter04

/**
  * Created by prokosna on 16/10/02.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  /**
    * args should be contravariance.
    * return should be covariance.
    * A => Either[EE, B] should be contravariance.
    * A should be contravariance => contravariance, thus A should be covariance.
    * A is declared as covariance, so it's ok.
    * Either[EE, B] should be covariance => contravariance, thus it should be contravariance.
    * But E is declared as covariance, so it's contradiction.
    * Thus EE >\: E is necessary.
    * 。
    *
    * @param f
    * @tparam B
    * @return
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  /**
    * Similar to flatMap, B should be contravariance.
    *
    * @param b
    * @tparam EE
    * @tparam B
    * @return
    */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }
  }
}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Test {
  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null)
      Left("Name is empty.")
    else
      Right(new Name(name))
  }
  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0)
      Left("Age is out of range")
    else
      Right(new Age(age))
  }
  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person)
  }
}