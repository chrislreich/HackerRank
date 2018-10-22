package FunctionalProgramming

/**
  * Created by creich on 2/23/18.
  */
import scala.collection.mutable.HashMap
object CombinationsDP {

  val m = BigInt(100000007)
  val map = new HashMap[(Int, Int), BigInt]()

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }






    def count(tuple: (Int, Int)) : BigInt = {
      if (tuple._2 == 0 || tuple._1 == tuple._2) {
        1
      }
      else {
        map.get(tuple) match {
          case Some(x) => x
          case None => {
            val left = count((tuple._1 - 1, tuple._2 - 1))
            val right = count((tuple._1 - 1, tuple._2))
            val sum = left + right
            map += (tuple -> sum)
            sum
          }
        }
      }
    }

  /*
  def run2 : Unit = {

    val n = readInt()

    val commandList = (for(i <- 0 until n) yield {val line = readLine().split(" "); (line(0).toInt, line(1).toInt)}).toList

    val res = memoize(commandList)

    println(res.mkString("\n"))


  }
  */




  def run : Unit = {
    val n = readInt()

    for (i <- 0 until n) {
      val line = readLine().split(" ")
      val n = line(0).toInt
      val k = line(1).toInt
      val result = count((n, k))
      println(result % m)
    }
  }


  def main(args: Array[String]) : Unit = {
    time{run}
  }

}
