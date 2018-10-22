package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object UtopianTree {

  def utopianTree(n: Int): Int = {
    def iterate(count: Int, currentNumber: Int) : Int = {
      if (count == n) {
        currentNumber
      }
      else {
        if (count % 2 == 1) {
          iterate(count + 1, currentNumber + 1)
        }
        else {
          iterate(count + 1, currentNumber * 2)
        }
      }
    }

    iterate(0, 1)
  }

}
