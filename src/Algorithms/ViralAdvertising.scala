package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object ViralAdvertising {

  def viralAdvertising(n: Int): Int = {
    def helper(day: Int, likedYesterday: Int, likedTotal: Int) : Int = {
      if (day == n) {
        likedTotal
      }
      else {
        val likedToday = (likedYesterday * 3) / 2
        helper(day + 1, likedToday, likedToday + likedTotal)
      }
    }

    helper(1, 2, 2)
  }

}
