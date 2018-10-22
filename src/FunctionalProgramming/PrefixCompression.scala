package FunctionalProgramming

import scala.util.Random

/**
  * Created by creich on 1/16/18.
  */
object PrefixCompression {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0) / 10e8) + " s")
    result
  }





  def generateRandomString(len : Int) : Stream[Char] = {
    val generator = Random.alphanumeric

    generator take len
  }




  def main(args: Array[String]) : Unit = {

    val prefixLength = 1000

    val firstStream = generateRandomString(100000)
    val secondStream = firstStream.take(prefixLength) ++ generateRandomString(100000 - prefixLength)

    val firstString = firstStream.mkString
    val secondString = secondStream.mkString

    time {


      val zipped = firstString.zipAll(secondString, "", "")


      val listPair = zipped.span(x => x._1 == x._2)

      val prefixList = listPair._1.map(y => y._1)

      val suffixLists = listPair._2.unzip

      val firstSuffix = suffixLists._1.takeWhile(_ != "")
      val secondSuffix = suffixLists._2.takeWhile(_ != "")

      println(prefixList.length + " " + prefixList.mkString)
      println(firstSuffix.length + " " + firstSuffix.mkString)
      println(secondSuffix.length + " " + secondSuffix.mkString)

    }



  }



}
