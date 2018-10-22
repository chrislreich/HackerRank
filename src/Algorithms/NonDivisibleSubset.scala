package Algorithms

/**
  * Created by creich on 8/14/18.
  */
object NonDivisibleSubset {

  // Complete the nonDivisibleSubset function below.
  def nonDivisibleSubset(k: Int, S: Array[Int]): Int = {
    val modList = S.map(_ % k)
    val groupedByModulo = modList.groupBy(x => x).mapValues(y => y.length)

    val halfModulo = if (k % 2 == 0) k / 2 else (k / 2) + 1

    val largerGroup = for (i <- 1 until halfModulo)
      yield {
        (groupedByModulo.get(i), groupedByModulo.get(k - i)) match {
          case (Some(v1), Some(v2)) => {
            if (v1 > v2) {
              (i, v1)
            }
            else {
              (k - i, v2)
            }
          }
          case (Some(v1), None) => {
            (i, v1)
          }
          case (None, Some(v2)) => {
            (k - i, v2)
          }
          case (None, None) => {
            (i, 0)
          }
        }
      }

    val halfModuloValue : Int = if (k % 2 == 0) {
      groupedByModulo.get(halfModulo) match {
        case Some(v) => 1
        case None => 0
      }
    }
    else {
      0
    }

    val zeroModuloValue: Int = groupedByModulo.get(0) match {
      case Some(v) => 1
      case None => 0
    }

    halfModuloValue + zeroModuloValue + largerGroup.foldLeft(0)((b, x) => x._2 + b)

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val nk = stdin.readLine.split(" ")

    val n = nk(0).trim.toInt

    val k = nk(1).trim.toInt

    val S = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = nonDivisibleSubset(k, S)

    println(result)
  }



}
