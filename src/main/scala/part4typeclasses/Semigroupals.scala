package part4typeclasses

import cats.Monad
import cats.kernel.Semigroup
import cats.Semigroupal

object Semigroupals {

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit
      m: Monad[F]
  ): F[(A, B)] = {

    val product: F[(A, B)] = m.flatMap(fa) { a =>
      m.map(fb) { b =>
        (a, b)
      }
    }

    import cats.syntax.functor._
    import cats.syntax.flatMap._

    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]

  import cats.syntax.validated._

  val invalidCombination = validatedSemigroupal.product(
    List("Some error message 1").invalid[Int],
    List("Second error message").invalid[Int]
  )

  val validCombination = validatedSemigroupal.product(
    12.valid,
    155.valid
  )

  val semiValidCombination = validatedSemigroupal.product(
    14.valid[List[String]],
    List("Some excited erro").invalid[Int]
  )


  val zipSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb
  }

  def main(args: Array[String]): Unit = {
    println(invalidCombination)
    println(validCombination)
    println(semiValidCombination)
  }
}
