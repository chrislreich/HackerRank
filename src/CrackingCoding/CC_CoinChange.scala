package CrackingCoding

/**
  * Created by creich on 5/24/18.
  */
import scala.collection.immutable.HashMap
object CC_CoinChange {



  def iterate(n : Int, coins : List[Int]) : Long = {
    def iterateNumbers(c : Int, i : Int,  map : HashMap[Int, Long]) : HashMap[Int, Long] = {
      if (i > n) {
        map
      }
      else {
        val previousVal = map.getOrElse(i, 0L)
        val minusVal = map.getOrElse(i - c, 0L)
        iterateNumbers(c, i + 1, map + (i -> (previousVal + minusVal)))
      }
    }
    def iterateCoins(coinList : List[Int], map : HashMap[Int, Long]) : HashMap[Int, Long] = {
      coinList match {
        case x :: xs => {
          iterateCoins(xs, iterateNumbers(x, x, map))
        }
        case _ => {
          map
        }
      }
    }
    val retMap = iterateCoins(coins, HashMap[Int, Long](0 -> 1L))
    retMap.getOrElse(n, 0L)
  }

  def test = {
    val cArr = (1 to 50).toList
    val n = 250
    println(iterate(n, cArr))
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val nm = stdin.readLine.split(" ")

    val n = nm(0).trim.toInt

    val m = nm(1).trim.toInt

    val coins = stdin.readLine.split(" ").map(_.trim.toInt).sorted

    val answer = iterate(n, coins.toList)
    println(answer)

  }

}
