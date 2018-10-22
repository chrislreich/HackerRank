package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object BeautifulDays {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def subtractReverse(i : Int) : Int = {
    val reverse = i.toString().reverse.toInt
    scala.math.abs(i - reverse)
  }

  def beautifulDays(i: Int, j: Int, k: Int): Int = {
    (for (c <- i until j + 1; if (subtractReverse(c) % k == 0)) yield c).size

  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val ijk = stdin.readLine.split(" ")

    val i = ijk(0).trim.toInt

    val j = ijk(1).trim.toInt

    val k = ijk(2).trim.toInt

    val result = time{beautifulDays(i, j, k)}

    println(result)
  }

}
