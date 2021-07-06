package part4typeclasses

import cats.Semigroupal
import cats.Functor
import cats.instances.string._
import cats.Applicative


object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val productTuple: W[(A, B)] = product(tuple._1, tuple._2)
      map(productTuple)(f.tupled)
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] = ???

  }

  trait MyApplicative[W[_]] extends Functor[W] with Semigroupal[W] {
    def pure[A](a: A): W[A]
  }

  def main(args: Array[String]) = {}
}
