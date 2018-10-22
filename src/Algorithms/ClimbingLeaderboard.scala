package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object ClimbingLeaderboard {

  // Complete the climbingLeaderboard function below.
  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
    def helper(aliceRemaining: List[Int], currentLeaderboard: List[(Int, Int)], retList : List[Int]) : List[Int] = {
      aliceRemaining match {
        case x :: xs => {
          val newLeaderboard = currentLeaderboard.dropWhile(v => v._1 <= x)
          val alicePosition = newLeaderboard.headOption match {
            case Some(v) => {
              v._2 + 1
            }
            case None => {
              1
            }
          }
          helper(xs, newLeaderboard, alicePosition :: retList)
        }
        case _ => {
          retList.reverse
        }
      }
    }

    val ascendingRanking = (0 :: scores.distinct.toList).zipWithIndex.tail.reverse
    val results = helper(alice.toList, ascendingRanking, List())
    results.toArray
  }

}
