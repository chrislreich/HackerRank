package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object ElectronicsShopSum {

  def getMoneySpent(keyboards: Array[Int], drives: Array[Int], b: Int): Int = {

    def iterate(d: Int, k: List[Int], lastSum: Int) : Int = {
      k match {
        case x :: xs => {
          if (x + d > b) {
            lastSum
          }
          else {
            iterate(d, xs, x + d)
          }
        }
        case _ => {
          lastSum
        }
      }
    }

    val filteredKeyboards = keyboards.sorted.distinct.takeWhile(_ < b).toList
    val filteredDrives = drives.sorted.distinct.takeWhile(_ < b).toList

    val sums = for (fd <- filteredDrives) yield iterate(fd, filteredKeyboards, 0)

    if (sums isEmpty) {
      -1
    }
    else {
      sums.max
    }

  }

}
