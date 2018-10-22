package Algorithms

/**
  * Created by creich on 6/1/18.
  */
object BirthdayCandles_CountOfMax {

  def countCandles(candles : List[Int], curMax : Int, curMaxCount : Int) : Int = {
    candles match {
      case x :: xs => {
        if (x > curMax) {
          countCandles(xs, x, 1)
        }
        else {
          if (x == curMax) {
            countCandles(xs, curMax, curMaxCount + 1)
          }
          else {
            countCandles(xs, curMax, curMaxCount)
          }
        }
      }
      case _ => curMaxCount
    }
  }


  def main(args : Array[String]) : Unit = {
    val age = readInt()
    val candles = readLine.split(" ").map(_.trim.toInt).toList

    println(countCandles(candles.tail, candles.head, 1))
  }
}
