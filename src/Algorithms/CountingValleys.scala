package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object CountingValleys {

  def countSteps(steps : List[Char], currentElevation: Int, numValleys: Int) : Int = {
    steps match {
      case x :: xs => {
        if (x == 'U') {
          if (currentElevation == -1) {
            countSteps(xs, 0, numValleys + 1)
          }
          else {
            countSteps(xs, currentElevation + 1, numValleys)
          }
        }
        else {
          countSteps(xs, currentElevation - 1, numValleys)
        }
      }
      case _ => {
        numValleys
      }
    }
  }

  def countingValleys(n: Int, s: String): Int = {
    countSteps(s.toList, 0, 0)
  }



}
