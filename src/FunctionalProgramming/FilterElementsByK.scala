package FunctionalProgramming

/**
  * Created by creich on 1/19/18.
  */

import scala.collection.immutable.HashMap
object FilterElementsByK {

  def filterList(l : List[Int], k : Int) : List[Int] = {
    def addToMap(map : HashMap[Int, Int], key : Int) : HashMap[Int, Int] = {
      val v = map.getOrElse(key, 0)
      map + (key -> (v + 1))
    }

    val countMap = l.foldLeft(new HashMap[Int, Int]())(addToMap)

    def filterNumbers(inputList : List[Int], currentInt : Int) : List[Int] = {
      if (!inputList.contains(currentInt) && countMap.getOrElse(currentInt, 0) >= k)
        currentInt :: inputList
      else
        inputList
    }


    val filteredList = l.foldLeft(List[Int]())(filterNumbers)

    if (filteredList.isEmpty)
      List(-1)
    else
      filteredList.reverse



  }


  def main(args : Array[String]) : Unit = {
    val t  = readInt()
    for (i <- 0 until t) {
      val line1 = readLine().split(" ").map(_.toInt)
      val n = line1(0)
      val k = line1(1)
      val a = readLine().split(" ").map(_.toInt)

      println(filterList(a.toList, k).mkString(" "))


    }
  }

}
