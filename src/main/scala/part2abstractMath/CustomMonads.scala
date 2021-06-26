package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {
  import cats.Monad

  type Identity[T] = T

  object IdentityMonad extends Monad[Identity] {

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] = f(fa)
    override def pure[A](x: A): Identity[A] = x

    @tailrec
    override def tailRecM[A, B](
        a: A
    )(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => value
    }

  }


  sealed trait Tree[+T]
  case class Leaf[+T](v: T) extends Tree[T]
  case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeMonad extends Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
	  case Leaf(v) => f(v)
	  case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = ???

    override def pure[A](x: A): Tree[A] = Leaf(x)


  }


  def main(args: Array[String]) = {}
}
