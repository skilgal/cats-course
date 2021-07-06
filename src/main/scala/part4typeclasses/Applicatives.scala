package part4typeclasses

import cats.Applicative
import cats.syntax

object Applicatives {

  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit
      applicative: Applicative[W]
  ): W[(A, B)] = {
    import cats.syntax.functor._


    val mainFunc: W[B => (A, B)] = ap {
      applicative.pure((a: A) => (b: B) => (a, b))
    }(wa)
    val mainFunc2 = applicative.map(wa)(a => (b: B) => (a, b))

    val nextFunct: W[(A, B)] = ap(mainFunc)(wb)
    ap(mainFunc2)(wb)

    nextFunct
  }

  def productWithApplicativesSyntax[W[_]: Applicative, A, B](
      wa: W[A],
      wb: W[B]
  ): W[(A, B)] = {
    import cats.syntax.functor._
    import cats.syntax.applicative._

    val internalFunc: W[B => (A, B)] = wa.map(a => (b: B) => (a, b))

    ap(internalFunc)(wb)
  }

  def main(args: Array[String]): Unit = {}
}
