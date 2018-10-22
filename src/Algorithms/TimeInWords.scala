package Algorithms

/**
  * Created by creich on 8/16/18.
  */
object TimeInWords {

  val hourMap: Map[Int, String] = Map[Int, String](
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve"
  )

  val minuteMap: Map[Int, String] = Map[Int, String] (
    1 -> "one minute",
    2 -> "two minutes",
    3 -> "three minutes",
    4 -> "four minutes",
    5 -> "five minutes",
    6 -> "six minutes",
    7 -> "seven minutes",
    8 -> "eight minutes",
    9 -> "nine minutes",
    10 -> "ten minutes",
    11 -> "eleven minutes",
    12 -> "twelve minutes",
    13 -> "thirteen minutes",
    14 -> "fourteen minutes",
    15 -> "quarter",
    16 -> "sixteen minutes",
    17 -> "seventeen minutes",
    18 -> "eighteen minutes",
    19 -> "nineteen minutes",
    20 -> "twenty minutes",
    21 -> "twenty one minutes",
    22 -> "twenty two minutes",
    23 -> "twenty three minutes",
    24 -> "twenty four minutes",
    25 -> "twenty five minutes",
    26 -> "twenty six minutes",
    27 -> "twenty seven minutes",
    28 -> "twenty eight minutes",
    29 -> "twenty nine minutes",
    30 -> "half"
  )



  // Complete the timeInWords function below.
  def timeInWords(h: Int, m: Int): String = {
    if (m == 0) {
      val hour = hourMap.getOrElse(h, "")
      hour + " o' clock"
    }
    else {

      if (m <= 30) {
        val hour = hourMap.getOrElse(h, "")
        val minute = minuteMap.getOrElse(m, "")
        minute + " past " + hour
      }
      else {
        val hour = hourMap.getOrElse((h % 12) + 1, "")
        val minute = minuteMap.getOrElse((60 - m), "")
        minute + " to " + hour
      }
    }

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val h = stdin.readLine.trim.toInt

    val m = stdin.readLine.trim.toInt

    val result = timeInWords(h, m)

    println(result)
  }


}
