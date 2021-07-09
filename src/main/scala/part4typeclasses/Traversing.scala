package part4typeclasses

import cats.Monad
import cats.Applicative

object Traversing {

  import cats.Traverse
  import cats.instances.list._
  import cats.instances.option._

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

  def listTraverseApplicative[F[_]: Applicative, A, B](
      list: List[A]
  )(f: A => F[B]): F[List[B]] = {
    import cats.syntax.apply._

    list.foldLeft(List.empty[B].pure[F]) { (accum, elem) =>
      val processed = f(elem)

      (accum, processed).mapN(_ :+ _)
    }
  }

  import cats.syntax.apply._

  println {
    (Option(List[Int](2)), Option(1)).mapN(_ :+ _)
  }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverseApplicative(list)(identity)

  import cats.instances.vector._
  val allPairs = listSequence(
    List(Vector(1, 2), Vector(3, 4))
  )
  listSequence(
    List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
  )

  import cats.syntax.option._

  def main(args: Array[String]): Unit = {
  }
}
