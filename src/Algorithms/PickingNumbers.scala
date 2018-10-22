package Algorithms

import scala.collection.immutable.SortedMap

/**
  * Created by creich on 8/10/18.
  */


object PickingNumbers {

  def pickingNumbers(a: Array[Int]): Int = {
    val map: SortedMap[Int, Int] =  SortedMap[Int, Int]() ++ a.groupBy(x => x).mapValues(_.length)
    if (map.size == 1) {
      map.values.toList.head
    }
    else {
      map.toArray.sliding(2, 1).map(x => {
        if (x(1)._1 - x(0)._1 == 1) x(0)._2 + x(1)._2 else scala.math.max(x(0)._2, x(1)._2)
      }).max
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val n = stdin.readLine.trim.toInt

    val a = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = pickingNumbers(a)

    println(result)
  }

}
