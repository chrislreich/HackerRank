package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object SaveThePrisoner {

  def saveThePrisoner(n: Int, m: Int, s: Int): Int = {
    val seatNumber = (m + (s - 1)) % n
    seatNumber match {
      case 0 => n
      case _ => seatNumber
    }
  }

}
