package part3datamanipulation

object Readers {

  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = {
      "dispatched"
    }

    def getLastOrderId(username: String): Long = {
      3585L
    }
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, content: String): String = {
      s"From: $emailReplyTo, to: $address >>> $content"
    }
  }

  import cats.data.Reader

  val config: Configuration = Configuration(
    "username",
    "pass",
    "localhost",
    8080,
    8,
    "sender@gmail.com"
  )

  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))

  val emailServiceReader: Reader[Configuration, EmailService] =
    Reader(conf => EmailService(conf.emailReplyTo))

  // TODO
  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of the last order
    // email them with email service: "Your last order has the status: (status)"
    val emailUserReader : Reader[Configuration, String] = for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(orderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(
      userEmail,
      s"Your last order has the status: $lastOrderStatus"
    )

    emailUserReader.run(config)

  }

  def main(args: Array[String]) = {
    println(
      emailUser("Dmytro", "receiver@gamil.com")
    )
  }
}
