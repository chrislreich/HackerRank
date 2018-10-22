package FunctionalProgramming

/**
  * Created by creich on 4/12/18.
  */

import scala.util.Random
object SubsetSum {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def binarySearch(vec : Vector[Long], value : Long) : Int = {
    def helper(left : Int, right : Int) : Int = {
      if (left + 1 == right) {
        if (vec(left) >= value) {
          left
        }
        else {
          right
        }
      }
      else {
        val mid = (left + right) / 2
        if (vec(mid) <= value) {
          helper(mid, right)
        }
        else {
          helper(left, mid)
        }
      }
    }
    helper(0, vec.length - 1)
  }

  def test : Unit = {
    val nCap = 1000000
    val lCap = 10000000000000000L
    val r = new Random(2283728)

    val longList = for (i <- 0 until nCap) yield scala.math.abs(r.nextLong())
    val sorted = longList.sortBy(x => -x).toVector
    val summed = sorted.scanLeft(0L)((b, L) => b + L).tail
    for (i <- 0 until nCap) {
      val nextLong = scala.math.abs(r.nextLong())
      binarySearch(summed, nextLong)
    }
  }

  def run : Unit = {
    val n = readInt()
    val input = readLine().split(" ").map(_ toLong)
    val sorted = input.sortBy(x => -x).toVector
    val summed = sorted.scanLeft(0L)((b, L) => b + L).tail

    val t = readInt()
    for (i <- 0 until t) {
      val curLong = readLine().toLong
      if (curLong > summed(n - 1)) {
        println(-1)
      }
      else {
        val res = binarySearch(summed, curLong)
        println(res + 1)
      }
    }


  }


  def main(args : Array[String]) : Unit = {
    run
  }

}
