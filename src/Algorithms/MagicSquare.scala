package Algorithms

import scala.collection.mutable.ArrayBuffer

/**
  * Created by creich on 8/10/18.
  */
object MagicSquare {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def findCost(magicSquare : ArrayBuffer[Int], inputBuffer: ArrayBuffer[Int]) : Int = {
    magicSquare.zip(inputBuffer).foldLeft(0)((b, x) => b + scala.math.abs(x._1 - x._2))
  }

  def validateMagicSquare(buf: ArrayBuffer[Int]) : Boolean = {
    (buf(0) + buf(1) + buf(2) == 15).
      && (buf(3) + buf(4) + buf(5) == 15).
      && (buf(6) + buf(7) + buf(8) == 15).
      && (buf(0) + buf(3) + buf(6) == 15).
      && (buf(1) + buf(4) + buf(7) == 15).
      && (buf(2) + buf(5) + buf(8) == 15).
      && (buf(0) + buf(4) + buf(8) == 15).
      && (buf(2) + buf(4) + buf(6) == 15)
  }

  def generateMagicSquares : ArrayBuffer[ArrayBuffer[Int]] = {
    def helper(curSet: Set[Int], curArr: ArrayBuffer[Int]) : ArrayBuffer[ArrayBuffer[Int]] = {
      if (curSet isEmpty) {
        ArrayBuffer(curArr)
      }
      else {
        curSet.foldLeft(ArrayBuffer[ArrayBuffer[Int]]())((b, x) => b ++ helper(curSet - x, curArr.clone += x))
      }
    }

    val set = Set[Int](1,2,3,4,5,6,7,8,9)
    helper(set, ArrayBuffer[Int]()).filter(validateMagicSquare)
  }

  def formingMagicSquare(s: Array[Array[Int]]): Int = {
    val asBuffer = s.flatten.to[ArrayBuffer]
    val magicSquares = generateMagicSquares
    magicSquares.map(x => findCost(x, asBuffer)).min
  }

  def main(args: Array[String]) : Unit = {
    val stdin = scala.io.StdIn


    val s = Array.ofDim[Int](3, 3)

    for (i <- 0 until 3) {
      s(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    println(formingMagicSquare(s))
  }

}
