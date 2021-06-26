package part2abstractMath

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import cats.instances.list._
import cats.data.EitherT
import scala.concurrent.Await

import cats.instances.future._

object MonadTransformers {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // Start from here
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 58,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("Some tet"), Right(2), Right(15))
  )

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None    => EitherT.left(Future(s"server $server is unreachable"))
      case Some(b) => EitherT.right(Future(b))
    }

  // Do not touch till here

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    b1 <- getBandwidth(s1)
    b2 <- getBandwidth(s2)
  } yield b1 + b2 > 250

  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Right(true) =>
        Right(s"Service $s1 and $s2 are able to keep the bandwidth")
      case Right(false) =>
        Left(s"Service $s1 and $s2 are not able to keep the bandwidth")
      case Left(message) => Left(message)
    }
  }

  def main(args: Array[String]): Unit = {
    import scala.concurrent.duration._
    println("we are inside")

    generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server3.rockthejvm.com"
    ).value.foreach(println)

    generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server3.test"
    ).value.foreach(println)

    generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server2.rockthejvm.com"
    ).value.foreach(println)

  }
}
