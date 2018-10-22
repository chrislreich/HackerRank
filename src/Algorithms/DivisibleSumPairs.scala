package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object DivisibleSumPairs {

  def divisibleSumPairs(n: Int, k: Int, ar: Array[Int]): Int = {

    def helper(curList : List[Int], numPairs : Int) : Int = {
      curList match {
        case x :: xs => {
          helper(xs, numPairs + xs.count(v => (v + x) % k == 0))
        }
        case _ => {
          numPairs
        }
      }
    }

    helper(ar.toList, 0)

  }

}
