package CrackingCoding

/**
  * Created by creich on 6/1/18.
  */
object CC_MergeSort_Inversion {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def findInversionCount(array : Array[Int]) : Long = {
    def combineArrays(left : Array[Int], right : Array[Int], leftIndex : Int, rightIndex : Int, inversions : Long, combined : Array[Int], combinedIndex : Int) :  (Array[Int], Long) = {
      (leftIndex < left.length, rightIndex < right.length) match {
        case (true, true) => {
          if (left(leftIndex) <= right(rightIndex)) {
            combined(combinedIndex) = left(leftIndex)
            combineArrays(left, right, leftIndex + 1, rightIndex,  inversions, combined, combinedIndex + 1)
          }
          else {
            combined(combinedIndex) = right(rightIndex)
            combineArrays(left, right, leftIndex, rightIndex + 1,  inversions + (left.length - leftIndex), combined, combinedIndex + 1)
          }
        }
        case (true, false) => {
          combined(combinedIndex) = left(leftIndex)
          combineArrays(left, right, leftIndex + 1, rightIndex, inversions, combined, combinedIndex + 1)
        }
        case (false, true) => {
          combined(combinedIndex) = right(rightIndex)
          combineArrays(left, right, leftIndex, rightIndex + 1, inversions, combined, combinedIndex + 1)
        }
        case (false, false) => {
          (combined, inversions)
        }
      }
    }
    def helper(currentArray : Array[Int]) : (Array[Int], Long) = {
      val len = currentArray.length
      if (len == 1) {
        (currentArray, 0L)
      }
      else {
        val midPoint = len / 2
        val leftSlice = currentArray.slice(0, midPoint)
        val rightSlice = currentArray.slice(midPoint, len)

        val leftResult = helper(leftSlice)
        val rightResult = helper(rightSlice)

        val combinedResult = combineArrays(leftResult._1, rightResult._1, 0, 0, 0L, new Array[Int](len), 0)
        val sum = leftResult._2 + rightResult._2 + combinedResult._2
        (combinedResult._1, sum)
      }
    }
    helper(array)._2
  }

  def run = {
    val q = readLine().trim.toInt
    for (i <- 0 until q) {
      val n = readLine().trim.toInt
      val arr = readLine().split(" ").map(_ toInt)
      println(findInversionCount(arr))
    }
  }

  def test = {
    val r = scala.util.Random

    for (j <- 0 until 15) {
      val a = for(i <- 0 until 100000) yield r.nextInt(10000000)
      time{println(findInversionCount(a.toArray))}
    }


  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
