package FunctionalProgramming

/**
  * Created by creich on 2/14/18.
  */
object BasicStockPrediction {



  def main(args : Array[String]) : Unit = {
    val n = readInt()
    val arr = readLine().split(" ").map(_.toInt).toList
    val q = readInt()
    for (i <- 0 until q) {
      val line = readLine().split(" ")
      val a = line(0).toInt
      val m = line(1).toInt
      val combinedSum = a + m

    }
  }

}
