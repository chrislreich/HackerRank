package FunctionalProgramming

/**
  * Created by creich on 4/12/18.
  */

import scala.collection.immutable.HashMap
object KayleNimbers {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  def stringToSet(str : String) : Map[Int, Int] = {
    def helper(curString : String, curNum : Int, retMap : Map[Int, Int]) : Map[Int, Int] = {
      if (curString isEmpty) {
        if (curNum > 0) {
          retMap.get(curNum) match {
            case Some(i) => {
              retMap + (curNum -> (i + 1))
            }
            case None => {
              retMap + (curNum -> 1)
            }
          }
        }
        else {
          retMap
        }
      }
      else {
        if (curString.head == 'I') {
          helper(curString.tail, curNum + 1, retMap)
        }
        else {
          if (curNum > 0) {
            retMap.get(curNum) match {
              case Some(i) => {
                helper(curString.tail, 0, retMap + (curNum -> (i + 1)))
              }
              case None => {
                helper(curString.tail, 0, retMap + (curNum -> 1))
              }
            }
          }
          else {
            helper(curString.tail, 0, retMap)
          }
        }
      }
    }
    helper(str, 0, Map[Int, Int]())
  }

  def findMex(set : Set[Int]) : Int = {
    def mex(curNum : Int) : Int = {
      if (set(curNum)) {
        mex(curNum + 1)
      }
      else {
        curNum
      }
    }
    mex(0)
  }

  def genPartitions(left : Int, right : Int, retList : List[(Int, Int)]) : List[(Int, Int)] = {
    if (left > right) {
      retList
    }
    else {
      genPartitions(left + 1, right - 1, (left, right) :: retList)
    }
  }



  def calculateGrundy(current : Int, map : HashMap[Int, Int]) : HashMap[Int, Int] = {
    def mapXOR(t : (Int, Int)) : Int = {
      val t1 = map.get(t._1) match {case Some(x) => x}
      val t2 = map.get(t._2) match {case Some(x) => x}
      t1 ^ t2
    }

    if (current > 300) {
      map
    }
    else {
      val minusOne = map.get(current - 1) match {case Some(x) => x}
      val minusTwo = map.get(current - 2) match {case Some(x) => x}
      val splitByTwo = genPartitions(1, current - 3, Nil)
      val splitByOne = genPartitions(1, current - 2, Nil)
      val combinedAndXORed = (splitByOne ++ splitByTwo).map(mapXOR)
      val curSet = (minusOne :: minusTwo :: combinedAndXORed).toSet
      val curVal = findMex(curSet)
      calculateGrundy(current + 1, map + (current -> curVal))
    }
  }


  def calculateState(stateList : List[(Int, Int)], grundyMap : HashMap[Int, Int]) : Int = {
    def reduceSame(i : Int, grundyValue : Int, curNum : Int) : Int = {
      if (i == 0) {
        curNum
      }
      else {

        reduceSame(i - 1, grundyValue, curNum ^ grundyValue)
      }
    }

    val statesMapped = stateList.map(x => {val curGrundy = grundyMap.getOrElse(x._1, -1); reduceSame(x._2 - 1, curGrundy, curGrundy)})
    val statesReduced = statesMapped.reduce(_ ^ _)
    statesReduced
  }

  def run : Unit = {
    val initialMap = HashMap[Int, Int](0 -> 0, 1 -> 1, 2 -> 2)
    val completedMap = time{calculateGrundy(3, initialMap)}
    val t = readInt()
    for (i <- 0 until t) {
      val n = readInt()
      val pins = readLine()
      val numSet = stringToSet(pins)
      val thisState = calculateState(numSet.toList, completedMap)
      if (thisState > 0) {
        println("WIN")
      }
      else {
        println("LOSE")
      }
    }
  }




  def main(args : Array[String]) : Unit = {
    run
  }

}
