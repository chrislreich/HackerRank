package FunctionalProgramming

/**
  * Created by creich on 1/10/18.
  */





object ConvexHull {

  private var minPoint : (Int, Int) = (Int.MaxValue, Int.MaxValue)
  private var inputPoints : List[(Int, Int)] = List()

  def sortPointsByAngle(a : (Int, Int), b: (Int, Int)) : Boolean = {
    val crossProduct = calculateCollinearity(minPoint, a, b)
    if (crossProduct == 0) {
      if (a._1 < b._1 || a._2 < b._2)
        true
      else
        false
    }
    else {
      if (crossProduct > 0)
        true
      else
        false
    }
  }

  def calculateCollinearity(p1 : (Int, Int), p2 : (Int, Int), p3 : (Int, Int)) : Int = {
    ((p2._1 - p1._1) * (p3._2 - p1._2)) - ((p2._2 - p1._2) * (p3._1 - p1._1))
  }



  def findPoints(curStack : List[(Int, Int)], remainingPoints : List[(Int, Int)]) : List[(Int, Int)] = {
    remainingPoints match {
      case x :: xs => {
        val crossProduct = calculateCollinearity(curStack(1), curStack(0), x)
        if (crossProduct >= 0)
          findPoints(x :: curStack, xs)
        else
          findPoints(curStack.tail, remainingPoints)
      }
      case _ => curStack.reverse
    }
  }


  def findHull() : List[(Int, Int)] = {
    val sortedPoints = inputPoints.sortWith(sortPointsByAngle)
    val minPointFinal = minPoint
    val stack = List[(Int, Int)](sortedPoints.head, minPointFinal)
    findPoints(stack, sortedPoints.tail)
  }

  def magnitude(p1 : (Int, Int), p2 : (Int, Int)) : Double = {
    val a = p2._1 - p1._1
    val b = p2._2 - p1._2
    val aSqrd = a * a
    val bSqrd = b * b
    scala.math.sqrt(aSqrd + bSqrd)
  }


  def calculatePerimeter(perimeterPoints : List[(Int, Int)]) : Double = {
    (perimeterPoints ++ List(perimeterPoints.head)).sliding(2, 1).foldLeft(0.0)((b, t) => b + magnitude(t(0), t(1)))
  }


  def removeCollinearPoints(inpt : List[(Int, Int)]) : List[(Int, Int)] = {

    def helper(startPoint : (Int, Int), midPoint : (Int, Int), retList : List[(Int, Int)], inputList : List[(Int, Int)]) : List[(Int, Int)] = {
      inputList match {
        case x :: xs => {
          val collinearity = calculateCollinearity(startPoint, midPoint, x)
          if (collinearity < 0) {
            helper(midPoint, x, startPoint :: retList, xs)
          }
          else {
            if (collinearity == 0)
              helper(startPoint, x, retList, xs)
            else
              helper(midPoint, x, startPoint :: retList, xs)
          }

        }
        case _ => midPoint :: (startPoint :: retList)
      }
    }

    val initialStart = inpt(0)
    val initialMid = inpt(1)
    val startList = List[(Int, Int)](initialMid, initialStart)
    helper(initialStart, initialMid, startList, inpt.drop(2)).reverse
  }



  def main(args : Array[String]) : Unit = {

    var cnt = 0

    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    while (cnt < n) {
      val x = sc.nextInt()
      val y = sc.nextInt()
      if (y < minPoint._2) {
        minPoint = (x, y)
      }
      else {
        if (y == minPoint._2){
          if (x < minPoint._1) minPoint = (x,y)
        }
      }
      inputPoints = (x, y) :: inputPoints
      cnt  = cnt + 1

    }

    inputPoints = inputPoints.filter(x => (x._1 != minPoint._1) || (x._2 != minPoint._2))

    val pts = findHull()
    val subset = removeCollinearPoints(pts)
    val perimeter = calculatePerimeter(pts)
    println(perimeter)
  }

}
