package part4typeclasses

import cats.Monoid

object Folding {

  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((e, fl) => f(e) :: fl)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((fl, e) => fl.foldRight(f(e))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List[A]())((fl, e) => if (predicate(e)) e :: fl else fl)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((fe, e) => monoid.combine(fe, e))

  }

  import cats.Foldable
  import cats.instances.list._
  import cats.instances.int._

  println(Foldable[List].foldMap(List("1", "2", "3"))(_.toInt))

  def main(args: Array[String]) = {

  }
}
