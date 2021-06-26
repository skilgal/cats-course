package part2abstractMath

import cats._
import cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object Monoids {

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance(
      ShoppingCart(List.empty, 0d),
      (l, r) =>
        ShoppingCart(
          Monoid[List[String]].combine(l.items, r.items),
          Monoid[Double].combine(l.total, r.total)
        )
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  trait MyMonoid[M[_]] {
    def pure[T](v: T): M[T]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(f andThen pure)
  }

  /*  */

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(config: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  class HttpServiceFuture(implicit ec: ExecutionContext)
      extends HttpService[Future] {

    def getConnection(config: Map[String, String]): Future[Connection] = {
      (for {
        host <- config.get("host")
        port <- config.get("port")
      } yield Connection(host, port))
        .map(Future.successful(_))
        .getOrElse(
          Future.failed(new Exception("some error during config reading"))
        )
    }

    def issueRequest(connection: Connection, payload: String): Future[String] =
      if (payload.length() < 20)
        Future.successful(s"request $payload has been accepted")
      else
        Future.failed(new IllegalArgumentException("payload is too long"))

  }

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]



  def main(args: Array[String]): Unit = {
    val phonebooks = List(
      Map(
        "Alice" -> 235,
        "Bob" -> 647
      ),
      Map(
        "Charlie" -> 372,
        "Daniel" -> 889
      ),
      Map(
        "Tina" -> 123
      )
    )
    combineFold(phonebooks)

  }
}
