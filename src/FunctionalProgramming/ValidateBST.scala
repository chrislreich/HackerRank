package FunctionalProgramming

/**
  * Created by creich on 1/23/18.
  */

import scala.collection.immutable.SortedSet
object ValidateBST {

  def checkBST(list : List[Int], len : Int) : String = {
    def helper(l : List[Int], curMinValue : Int, numberSet : SortedSet[Int]) : Boolean = {
      l match {
        case x :: xs => {
          if (x < curMinValue)
            false
          else {
            val newSet = numberSet + x
            val setMax = newSet.filter(_ < x)
            val newMinValue = if (setMax.isEmpty) {
              curMinValue
            }
            else {
              val maxElement = setMax.max
              scala.math.max(maxElement, curMinValue)

            }

            helper(xs, newMinValue, newSet)

          }
        }
        case _ => true
      }
    }


    helper(list, Int.MinValue, SortedSet[Int]()) match {
      case true => "YES"
      case false => "NO"
    }

  }


  def main(args : Array[String]) : Unit = {
    val t = readInt()
    for (i <- 0 until t) {
      val n = readInt()
      val nextLine = readLine()
      val intList = nextLine.split(" ").toList.map(_.toInt)
      println(checkBST(intList, n))

    }
  }

}
