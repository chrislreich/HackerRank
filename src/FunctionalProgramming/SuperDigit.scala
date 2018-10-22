package FunctionalProgramming

/**
  * Created by creich on 1/19/18.
  */

import scala.math.BigInt
object SuperDigit {



  def findSuperDigit(s : String) : String = {
    if (s.length == 1)
      s
    else {
      findSuperDigit(s.foldLeft(BigInt(0))(_ + _.toString.toInt).toString)
    }
  }






  def main(args : Array[String]) : Unit = {
    val l = readLine().split(" ")
    val n = l(0)
    val k = l(1).toInt

    val initialSum = n.foldLeft(BigInt(0))(_ + _.toString.toInt)


    val retString = findSuperDigit((initialSum * BigInt(k)).toString)
    println(retString)
  }



}
