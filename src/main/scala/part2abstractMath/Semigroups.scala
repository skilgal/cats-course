package part2abstractMath

import cats._
import cats.implicits._

object Semigroups {

  case class Expence(id: Long, amount: Double)

  implicit val expSemigroup = Semigroup.instance[Expence] { (l, r) =>
    Expence(
      Semigroup[Long].combine(l.id, r.id),
      Semigroup[Double].combine(l.amount, r.amount)
    )
  }

  def reduceList[T](l: List[T])(implicit semigroup: Semigroup[T]): T =
    l.reduce(_ |+| _)

  def reduceList2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]) = {
    import cats.syntax.semigroup._

    val anIntSum = 2 |+| 3


  }
}
