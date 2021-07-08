package part3datamanipulation

object DataValidation {
  import cats.data.Validated

  object FormValidation {

    type FormValidation[T] = Validated[List[String], T]

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      def value(key: String): FormValidation[String] = {
        Validated.fromOption(
          form.get(key),
          List(s"Key $key doesn't exist")
        )
      }

      def exists(key: String): FormValidation[String] =
        Validated.cond(
          form.contains(key),
          key,
          List(s"Key $key doesn't exists")
        )

      def nonBlanc(s: String): FormValidation[String] =
        Validated.cond(s.nonEmpty, s, List("value is empty"))

      def containsString(
          value: String,
          required: String
      ): FormValidation[String] =
        Validated.cond(
          value.contains(required),
          value,
          List(s"Value $value doesn't contain $required")
        )

      def checkLength(value: String): FormValidation[String] =
        Validated.cond(
          value.length() >= 10,
          value,
          List(s"Value $value must have length `ge` 10")
        )
      
      import cats.instances.string._
      import cats.instances.list._

      value("name")
        .andThen(nonBlanc)
        .combine(value("email").andThen(containsString(_, "@")))
        .combine(value("password").andThen(checkLength))
        .map(_ => "Success")
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      FormValidation.validateForm(
        Map(
          "name" -> "some test name",
          "password" -> "1234nheout'",
          "email"-> "some@test")

        )
      )
  }
      

}
