package Algorithms

/**
  * Created by creich on 6/5/18.
  */
object KangarooJumps {

  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    val intercepts = x1 - x2
    val slopes = v2 - v1
    if (slopes == 0) {
      "NO"
    }
    else {
      if ((intercepts / slopes) >= 0 && (intercepts % slopes) == 0) {
        "YES"
      }
      else {
        "NO"
      }
    }
  }

  def main(args : Array[String]) : Unit = {
    val line = readLine().split(" ").map(_.trim.toInt)

    val x1 = line(0)
    val v1 = line(1)

    val x2 = line(2)
    val v2 = line(3)

    println(kangaroo(x1, v1, x2, v2))

  }

}
