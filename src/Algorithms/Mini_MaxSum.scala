package Algorithms

/**
  * Created by creich on 6/1/18.
  */
object Mini_MaxSum {

  def filterNumber(list : List[Int], num : Int, retList : List[Int]) : List[Int] = {
    list match {
      case x :: xs => {
        if (x == num) {
          retList ++ xs
        }
        else {
          filterNumber(xs, num, x :: retList)
        }
      }
      case _ => {
        retList
      }
    }
  }

  def min(list : List[Int]) : Long = {
    val max = list.max
    filterNumber(list, max, List.empty).foldLeft(0L)(_ + _)
  }
  def max(list : List[Int]) : Long = {
    val min = list.min
    filterNumber(list, min, List.empty).foldLeft(0L)(_ + _)
  }

  def run = {
    val nums = readLine().split(" ").map(_.trim.toInt).toList
    val minimum = min(nums)
    val maximum = max(nums)
    println(minimum + " " + maximum)
  }

  def main(args : Array[String]) : Unit = {
    run
  }
}
