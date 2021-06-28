package part3datamanipulation

object States {

  import cats.data.State

  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State { cart =>
      (ShoppingCart(cart.items :+ item, cart.total + price), cart.total + price)
    }

  val myCart = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Eletric cable", 8)
  } yield total

  def inspect[A, B](f: A => B): State[A, B] = State { prev => (prev, f(prev)) }

  def get[A]: State[A, A] = State { prev => (prev, prev) }

  def set[A](value: A): State[A, Unit] = State { _ => (value, ()) }

  def modify[A](f: A => A): State[A, Unit] = State { prev => (f(prev), ()) }

  import cats.data.State._

  val program = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(myCart.run(ShoppingCart(List(), 0)).value)

    println(
      program.run(0).value
    )
  }

}
