package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object HurdleRace {

  def hurdleRace(k: Int, height: Array[Int]): Int = {
    scala.math.max(height.max - k, 0)
  }

}
