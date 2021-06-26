package part2abstractMath

import cats.kernel.Monoid

object Functors {

  import cats.Functor

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  trait Tree[+T]
  object Tree {
    // "smart" constructors
    def leaf[T](v: T): Tree[T] = Leaf(v)
    def branch[T](v: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(v, left, right)
  }
  case class Leaf[+T](v: T) extends Tree[T]
  case class Branch[+T](v: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) =>
        Branch(f(v), map(left)(f), map(right)(f))
    }

  }

  def main(args: Array[String]) = {
    println(do10x(Tree.leaf(10)))
  }
}
