package part4typeclasses

import cats.Monad
import cats.Applicative

object Traversing {

  import cats.Traverse
  import cats.instances.list._
  import cats.instances.option._

  println(Traverse[List].traverse(List(1, 2, 3))(number => Option(number)))
  println(Traverse[List].sequence(List(Option(1), Option(2), None)))

  import cats.syntax.traverse._
  List(Option(1)).sequence

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.applicative._

  def listTraverse[F[_]: Monad, A, B](
      list: List[A]
  )(f: A => F[B]): F[List[B]] = {

    list.foldLeft(List.empty[B].pure[F]) { (accum, elem) =>
      val processed = f(elem)

      for {
        acc <- accum
        newElem <- processed
      } yield newElem :: acc
    }
  }

  def listTraverse[F[_]: Applicative, A, B](
      list: List[A]
  )(f: A => F[B]): F[List[B]] = {
    import cats.syntax.apply._

    list.foldLeft(List.empty[B].pure[F]) { (accum, elem) =>
      val processed = f(elem)

      (accum, processed).mapN(_ :+ _)
    }
  }


  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = {
    ???
  }
  def main(args: Array[String]): Unit = {}
}
