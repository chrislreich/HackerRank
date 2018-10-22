package FunctionalProgramming

/**
  * Created by creich on 1/22/18.
  */
object ConcavePolygon {

  private var minPoint = (Int.MaxValue, Int.MaxValue)
  private var inputList = List[(Int, Int)]()

  def crossProduct(p1 : (Int, Int), p2 : (Int, Int), p3 : (Int, Int)) : Int = {
    ((p2._1 - p1._1) * (p3._2 - p1._2)) - ((p2._2 - p1._2) * (p3._1 - p1._1))
  }

  def sortPointsRelativeToMinPoint(a : (Int, Int), b : (Int, Int)) : Boolean = {
    val cp = crossProduct(minPoint, a, b)
    if (cp == 0) {
      if (a._1 < b._1 || a._2 < b._2)
        true
      else
        false
    }
    else {
      if (cp > 0)
        true
      else
        false
    }
  }

  def convex_?(stack : List[(Int, Int)], remainingPoints : List[(Int, Int)]) : String = {
    remainingPoints match {
      case x :: xs => {
        val stackPoints = stack.take(2)
        val startPoint = stackPoints.tail.head
        val midPoint = stackPoints.head

        val cp = crossProduct(startPoint, midPoint, x)
        if (cp >= 0)
          convex_?(x :: stack, xs)
        else
          "YES"
      }
      case _ => "NO"
    }
  }


  def main(args : Array[String]) : Unit = {

    val c = readInt()

    for (i <- 0 until c) {
      val nextLine = readLine()
      val splitArr = nextLine.split(" ")
      val x = splitArr(0).toInt
      val y = splitArr(1).toInt

      if (y < minPoint._2) {
        minPoint = (x, y)
      }
      else {
        if (y == minPoint._2) {
          if (x < minPoint._1) {
            minPoint = (x, y)
          }
        }
      }
      inputList = (x, y) :: inputList
    }

    inputList = inputList.filter(_ != minPoint)

    inputList = inputList.sortWith(sortPointsRelativeToMinPoint)

    val initStack = List[(Int, Int)](inputList.head, minPoint)

    println(convex_?(initStack, inputList.tail))

  }

}
