package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object BonAppetit {

  def bonAppetit(bill: Array[Int], k: Int, b: Int) {

    bill(k) = 0

    val total = bill.sum

    if (total / 2 == b) {
      println("Bon Appetit")
    }
    else {
      println(b - (total / 2))
    }

  }

}
