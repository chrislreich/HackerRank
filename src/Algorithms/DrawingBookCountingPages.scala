package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object DrawingBookCountingPages {

  def countFromFront(p: Int) : Int = {
    p / 2
  }

  def countFromBack(p : Int, n : Int) : Int = {
    if (n % 2 == 0) {
      (n - p + 1) / 2
    }
    else {
      (n - p) / 2
    }
  }

  def pageCount(n: Int, p: Int): Int = {
    scala.math.min(countFromFront(p), countFromBack(p, n))
  }

}
