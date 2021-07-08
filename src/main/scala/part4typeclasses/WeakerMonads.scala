package part4typeclasses

import cats.Apply

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def ap[A, B](f: M[A => B])(ma: M[A]): M[B] = {

      flatMap(ma) { a =>
        map(f) { func =>
          func(a)
        }
      }
    }
  }

  def main(args: Array[String]) = {}
}
