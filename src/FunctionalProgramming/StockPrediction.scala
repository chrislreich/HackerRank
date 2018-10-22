package FunctionalProgramming

/**
  * Created by creich on 2/9/18.
  */

import scala.collection.immutable.{HashMap, TreeMap}
object StockPrediction {

  val ordering : Ordering[Int] = Ordering[Int].on[Int](s => s)

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  class Range(val leftLimit : Int, val rightLimit : Int, val value : Int, val length : Int, var leftMap : TreeMap[Int, Int] = null, var rightMap : TreeMap[Int, Int] = null)

  def makeRangeMap(l : List[(Int, Int)], retMap : HashMap[Int, Range]) : HashMap[Int, Range] = {
    l match {
      case x :: xs => {
        xs.span(_._1 == x._1) match {
          case (Nil, t) => makeRangeMap(t, retMap + (x._2 -> new Range(x._2, x._2, x._1, 1)))
          case (h, t) => {
            val rightLimit = h.last._2

            val curRange = new Range(x._2, rightLimit, x._1, h.length + 1)
            var newMap = retMap
            for (r <- x._2 until rightLimit + 1) {
              newMap = newMap + (r -> curRange)
            }
            makeRangeMap(t, newMap)
          }
        }
      }
      case _ => retMap
    }
  }


  def populateIntervalMap(m : Map[Int, Range], length : Int, minValue : Int, minLeftIndex : Int, minRightIndex : Int) : Map[Int, Range] = {

    def finalMin(inputList : List[(TreeMap[Int, Int], Int)], curMap : TreeMap[Int, Int], isRight : Boolean) : Unit = {
      if (isRight) {
        inputList match {
          case x :: xs => {
            val combinedMap = combineMaps(x._1, curMap)
            val curRange = m.get(x._2) match {
              case Some(x) => x
            }
            curRange.rightMap = combinedMap
            finalMin(xs, combinedMap, isRight)
          }
          case _ => {
            val finalRange = m.get(minLeftIndex) match {
              case Some(x) => x
            }
            finalRange.rightMap = curMap
          }
        }
      }
      else {
        inputList match {
          case x :: xs => {
            val combinedMap = combineMaps(x._1, curMap)
            val curRange = m.get(x._2) match {
              case Some(x) => x
            }
            curRange.leftMap = combinedMap
            finalMin(xs, combinedMap, isRight)
          }
          case _ => {
            val finalRange = m.get(minRightIndex) match {
              case Some(x) => x
            }
            finalRange.leftMap = curMap
          }
        }
      }
    }


    def combineMaps(l : TreeMap[Int, Int], r : TreeMap[Int, Int]) : TreeMap[Int, Int] = {
      if (r.isEmpty) {
        l
      }
      else {
        val leftMax = l.max
        val split = r.span(_._1 <= leftMax._1)
        val newMinLength = if(split._1.isEmpty) {
          leftMax._2
        }
        else {
         leftMax._2 + split._1.last._2
        }
        val intermediateMap = l + (leftMax._1 -> newMinLength)
        split._2.foldLeft(intermediateMap)((b, t) => b + (t._1 -> (leftMax._2 + t._2)))
      }
    }

    def minRight(i : Int, minValue : Int, rightMap : TreeMap[Int, Int], mapStack : List[(TreeMap[Int, Int], Int)], curIndex : Int) : List[(TreeMap[Int, Int], Int)] = {
      if (i >= length) {
        (rightMap, curIndex) :: mapStack
      }
      else {
        val curRange = m.get(i) match {
          case Some(x) => x
        }
        if (curRange.value == minValue) {
          val maxTuple = rightMap.max
          minRight(curRange.rightLimit + 1, minValue, new TreeMap[Int, Int]()(ordering), ((rightMap + (maxTuple._1 -> (maxTuple._2 + curRange.length))), curIndex) :: mapStack, i)
        }
        else {
          if (rightMap.isEmpty) {
            minRight(curRange.rightLimit + 1, minValue, rightMap + ((curRange.value - minValue) -> curRange.length), mapStack, curIndex)
          }
          else {
            val maxTuple = rightMap.max
            if (maxTuple._1 >= curRange.value - minValue) {
              minRight(curRange.rightLimit + 1, minValue, rightMap + (maxTuple._1 -> (maxTuple._2 + curRange.length)), mapStack, curIndex)
            }
            else {
              minRight(curRange.rightLimit + 1, minValue, rightMap + ((curRange.value - minValue) -> (curRange.length + maxTuple._2)), mapStack, curIndex)
            }
          }
        }
      }
    }

    def minLeft(i : Int, minValue : Int, leftMap : TreeMap[Int, Int], mapStack : List[(TreeMap[Int, Int], Int)], curIndex : Int) : List[(TreeMap[Int, Int], Int)] = {
      if (i < 0) {
        (leftMap, curIndex) :: mapStack
      }
      else {
        val curRange = m.get(i) match {
          case Some(x) => x
        }
        if (curRange.value == minValue) {
          val maxTuple = leftMap.max
          minLeft(curRange.leftLimit - 1, minValue, new TreeMap[Int, Int]()(ordering), ((leftMap + (maxTuple._1 -> (maxTuple._2 + curRange.length))), curIndex) :: mapStack, i)
        }
        else {
          if (leftMap.isEmpty) {
            minLeft(curRange.leftLimit - 1, minValue, leftMap + ((curRange.value - minValue) -> curRange.length), mapStack, curIndex)
          }
          else {
            val maxTuple = leftMap.max
            if (maxTuple._1 >= curRange.value - minValue) {
              minLeft(curRange.leftLimit - 1, minValue, leftMap + (maxTuple._1 -> (maxTuple._2 + curRange.length)), mapStack, curIndex)
            }
            else{
              minLeft(curRange.leftLimit - 1, minValue, leftMap + ((curRange.value - minValue) -> (curRange.length + maxTuple._2)), mapStack, curIndex)
            }
          }
        }
      }
    }

    val rightMinRange = m.get(minLeftIndex) match {
      case Some(x) => x
    }

    val leftMinRange = m.get(minRightIndex) match {
      case Some(x) => x
    }

    val rightProcess = minRight(rightMinRange.rightLimit + 1, minValue, new TreeMap[Int, Int]()(ordering), List.empty, rightMinRange.rightLimit)

    val rightProcessLast = m.get(rightProcess.head._2) match {
      case Some(x) => x
    }

    rightProcessLast.rightMap = rightProcess.head._1

    finalMin(rightProcess.tail, rightProcess.head._1, true)

    val leftProcess = minLeft(leftMinRange.leftLimit - 1, minValue, new TreeMap[Int, Int]()(ordering), List.empty, leftMinRange.leftLimit)

    val leftProcessLast = m.get(leftProcess.head._2) match {
      case Some(x) => x
    }

    leftProcessLast.leftMap = leftProcess.head._1

    finalMin(leftProcess.tail, leftProcess.head._1, false)

    m

  }





  def findMaximumArrayLength(lookupMap : Map[Int, Range], a : Int, margin : BigInt, listLength: Int, minValue : Int) : Int = {
    val r = lookupMap.get(a) match {
      case Some(x) => x
    }
    val combined = r.value + margin

    def lookLeft(i : Int, retCount : Int) : Int = {
      if (i < 0) {
        retCount
      }
      else {
        val curRange = lookupMap.get(i) match {
          case Some(x) => x
        }
        if (curRange.value >= r.value && curRange.value <= combined) {
          lookLeft(curRange.leftLimit - 1, retCount + curRange.length)
        }
        else {
          retCount
        }
      }
    }

    def lookRight(i : Int, retCount : Int) : Int = {
      if (i >= listLength) {
        retCount
      }
      else {
        val curRange = lookupMap.get(i) match {
          case Some(x) => x
        }
        if (curRange.value >= r.value && curRange.value <= combined) {
          lookRight(curRange.rightLimit + 1, retCount + curRange.length)
        }
        else {
          retCount
        }
      }
    }


    if (r.value == minValue) {
      val leftNum = r.leftMap.takeWhile(_._1 <= margin).lastOption match {
        case Some(x) => x._2
        case None => 0
      }
      val rightNum = r.rightMap.takeWhile(_._1 <= margin).lastOption match {
        case Some(x) => x._2
        case None => 0
      }
      r.length + leftNum + rightNum
    }
    else {
      r.length + lookLeft(r.leftLimit - 1, 0) + lookRight(r.rightLimit + 1, 0)
    }
  }


  def main(args : Array[String]) : Unit = {
    val n = readInt()
    val arr = readLine().split(" ").map(_.toInt).toList.zipWithIndex
    val rangeMap = makeRangeMap(arr, new HashMap[Int, Range]())

    val minimumElement = arr.minBy(_._1)._1

    val minLeftIndex = arr.find(_._1 == minimumElement) match {
      case Some(x) => x._2
    }

    val minRightIndex = arr.reverse.find(_._1 == minimumElement) match {
      case Some(x) => x._2
    }


    val minRangeMap = populateIntervalMap(rangeMap, arr.length, minimumElement, minLeftIndex, minRightIndex)

    val q = readInt()

      for (i <- 0 until q) {
        val line = readLine().split(" ")
        val a = line(0).toInt
        val m = line(1).toInt
        println(findMaximumArrayLength(minRangeMap, a, m, n, minimumElement))


    }


  }

}
