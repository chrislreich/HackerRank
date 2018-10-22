package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object BreakingMinMax {

  def countScores(remainingScores: List[Int], curMax : Int, curMin: Int, newMaxCount: Int, newMinCount: Int) : Array[Int] = {
    remainingScores match {
      case x :: xs => {
        if (x > curMax) {
          countScores(xs, x, curMin, newMaxCount + 1, newMinCount)
        }
        else {
          if (x < curMin) {
            countScores(xs, curMax, x, newMaxCount, newMinCount + 1)
          }
          else {
            countScores(xs, curMax, curMin, newMaxCount, newMinCount)
          }
        }
      }
      case _ => {
        Array(newMaxCount, newMinCount)
      }
    }
  }


  def breakingRecords(scores: Array[Int]) : Array[Int] = {
    val scoresList = scores.toList
    val firstScore = scoresList.head
    countScores(scoresList.tail, firstScore, firstScore, 0, 0)
  }



  def main(args: Array[String]) : Unit = {

  }

}
