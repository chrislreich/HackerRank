package FunctionalProgramming

/**
  * Created by creich on 2/5/18.
  */
object MaximumSkyline {


  def findMaximumSkyline(list : List[(Int, Int)], stack : List[(Int, Int)], i : Int, maxArea : BigInt) : BigInt = {
    list match {
      case x :: xs => {
        if (stack.isEmpty || x._1 >= stack.head._1) {
          findMaximumSkyline(xs, x :: stack, i + 1, maxArea)
        }
        else {
          val newArea = stack.head._1 * (if (stack.tail.isEmpty) i else i - stack.tail.head._2 - 1)
          if (newArea > maxArea) {
            findMaximumSkyline(list, stack.tail, i, newArea)
          }
          else {
            findMaximumSkyline(list, stack.tail, i, maxArea)
          }
        }
      }
      case _ => {
        if (stack.isEmpty) {
          maxArea
        }
        else {
          val newArea = stack.head._1 * (if (stack.tail.isEmpty) i else i - stack.tail.head._2 - 1)
          if (newArea > maxArea) {
            findMaximumSkyline(list, stack.tail, i, newArea)
          }
          else {
            findMaximumSkyline(list, stack.tail, i, maxArea)
          }
        }
      }
    }

  }


  def main(args : Array[String]) : Unit = {
    val n = readInt()
    val line = readLine().split(" ").map(_.toInt).toList
    println(findMaximumSkyline(line.zipWithIndex,List(), 0, 0))


  }

}
