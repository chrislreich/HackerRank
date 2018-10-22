package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_QueueFromStacks {

  def processQuery(queries : List[String], leftStack : List[Int], rightStack : List[Int]) : Unit = {
    queries match {
      case x :: xs => {
        val arr = x.split(" ").map(_.toInt)
        (leftStack, rightStack, arr(0)) match {
          case (_, _, 1) => {
            processQuery(xs, arr(1) :: leftStack, rightStack)
          }
          case (_, r :: rs, 2) => {
            processQuery(xs, leftStack, rs)
          }
          case (_, _, 2) => {
            processQuery(xs, List.empty, leftStack.reverse.tail)
          }
          case (_, r :: rs, 3) => {
            println(r)
            processQuery(xs, leftStack, rightStack)
          }
          case (_ , _ , 3) => {
            val reversed = leftStack.reverse
            println(reversed.head)
            processQuery(xs, List.empty, reversed)
          }
        }
      }
    }
  }

  def run = {
    var queue = List[Int]()
    val q = readInt()
    val queries = for (i <- 0 until q) yield readLine()

    processQuery(queries.toList, List[Int](), List[Int]())
  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
