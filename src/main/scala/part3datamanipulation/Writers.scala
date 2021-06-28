package part3datamanipulation

object Writers {

  import cats.data.Writer

  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), n)
    else countAndLog(n - 1).mapWritten(_ :+ n.toString())
  }

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum (${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }


  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    import cats.instances.vector._
    if (n <= 0) Writer(Vector.empty, 0)
    else for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum (${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n

  }

  def main(args: Array[String]): Unit = {
    countAndSay(10)
    countAndLog(10).written.foreach(println)

    println(naiveSum(10))
    val result = sumWithLogs(10).run
    result._1.foreach(println)
    println(result._2)
  }
}
