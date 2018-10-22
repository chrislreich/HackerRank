package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_BubbleSort {

  def bubbleSort(array : Array[Int], n : Int) : (Int, Array[Int]) ={
    def helper(i : Int, j : Int, counter : Int, arr : Array[Int], isSorted : Boolean) : (Int, Array[Int]) = {
      (j == n - 1, i == n, isSorted) match {
        case (true, _, true) => {
          (counter, arr)
        }
        case (true, _, false) => {
          helper(i + 1, 0, counter, arr, true)
        }
        case (false, true, _) => {
          (counter, arr)
        }
        case (false, false, _) =>{
          if (arr(j) > arr(j + 1)) {
            val tempVal = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = tempVal
            helper(i, j + 1, counter + 1, arr, false)
          }
          else {
            helper(i, j + 1, counter, arr, isSorted)
          }
        }
      }
    }

    helper(0, 0, 0, array, true)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var a = new Array[Int](n);
    for(a_i <- 0 to n-1) {
      a(a_i) = sc.nextInt();
    }

    val result = bubbleSort(a, n)
    val numSwaps = result._1
    val firstNum = result._2(0)
    val lastNum = result._2(n - 1)
    println(s"Array is sorted in $numSwaps swaps.")
    println(s"First Element: $firstNum")
    println(s"Last Element: $lastNum")
  }

}
