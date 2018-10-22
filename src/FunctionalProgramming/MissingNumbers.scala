package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */

import scala.collection.GenTraversableOnce
import scala.collection.mutable.HashMap
object MissingNumbers {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def createMap(arr : GenTraversableOnce[Int]) : HashMap[Int, Int] = {
    val retMap = new HashMap[Int, Int]()
    for (e <- arr) {
      val mapVal = retMap.getOrElse(e, 0)
      retMap += (e -> (mapVal + 1))
    }
    retMap
  }


  def findMissingElements(a : HashMap[Int, Int], b : HashMap[Int, Int]) : List[Int] = {
    var retList = List[Int]()
    for (e <- b.keys) {
       a.get(e) match {
         case Some(aVal) => {
           val bVal = b.get(e) match {case Some(x) => x}
           if (bVal > aVal) {
             retList = e :: retList
           }
         }
         case None => {
           retList = e :: retList
         }
       }
    }
    retList
  }

  def run : Unit = {
    val a_n = readInt()
    val a = readLine().split(" ").map(_ toInt)
    val b_n = readInt()
    val b = readLine().split(" ").map(_ toInt)

    val aMap = createMap(a)
    val bMap = createMap(b)

    val missingList = findMissingElements(aMap, bMap)

    println(missingList.sorted.mkString(" "))
  }




  def genNums(n : Int) : List[Int] = {
    val r = new scala.util.Random()

    (for (i <- 0 until n) yield r.nextInt(200)).toList
  }

  def test : Unit = {
    val a = genNums(200000)
    val b = genNums(200000)

    time {

      val aMap = createMap(a)
      val bMap = createMap(b)

      val missing = findMissingElements(aMap, bMap)

      println(missing.sorted.mkString(" "))


    }






  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
