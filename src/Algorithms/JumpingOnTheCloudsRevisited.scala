package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object JumpingOnTheCloudsRevisited {

  def jumpingOnClouds(c: Array[Int], k: Int): Int = {
    val numClouds = c.length

    def helper(index: Int, energy: Int) : Int = {
      if (index == 0 && energy < 100) {
        energy
      }
      else {
        val newIndex = (index + k) % numClouds
        if (c(newIndex) == 1) {
          helper(newIndex, energy - 3)
        }
        else {
          helper(newIndex, energy - 1)
        }
      }
    }

    helper(0, 100)
  }

}
