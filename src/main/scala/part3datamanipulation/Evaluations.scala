package part3datamanipulation

object Evaluations {

  import cats.Eval

  def defer[T](eval: => Eval[T]): Eval[T] = {
    Eval.later(()).flatMap(_ => eval)
  }

  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
    // val deffered = defer(Eval.now {
    //   println("Some string")
    //   42
    // }

    // println("Some test message")

    // println(deffered.value)

    println {
      reverseEval((1 to 10000).toList).value
    }


  }
}
