package Algorithms

/**
  * Created by creich on 8/21/18.
  */
object HighestValuePalindrome {

  def findMinimumChangesNeeded(s: String): Int = {
    val len = s.length()
    val middle = len / 2
    def helper(i: Int, acc: Int): Int = {
      if (i >= middle) {
        acc
      }
      else {
        if (s(i) == (s(len - i - 1))) {
          helper(i + 1, acc)
        }
        else {
          helper(i + 1, acc + 1)
        }
      }
    }
    helper(0, 0)
  }


  def highestValuePalindrome(s: String, n: Int, k: Int): String = {

    val len = s.length()
    val middle = len / 2

    def helper(i: Int, arr: Array[Char], remainingChangesNeeded: Int, remainingChangesAvailable: Int): Array[Char] = {
      if (i >= middle) {
        if (len % 2 == 1 && remainingChangesAvailable > 0) {
          arr(middle) = '9'
          arr
        }
        else {
          arr
        }
      }
      else {
        val leftChar = arr(i)
        val rightChar = arr(len - i - 1)
        (leftChar == '9', rightChar == '9', leftChar == rightChar) match {
          case (true, true, _) => {
            helper(i + 1, arr, remainingChangesNeeded, remainingChangesAvailable)
          }
          case (true, false, false) => {
            arr(len - i - 1) = '9'
            helper(i + 1, arr, remainingChangesNeeded - 1, remainingChangesAvailable - 1)
          }
          case (false, true, false) => {
            arr(i) = '9'
            helper(i + 1, arr, remainingChangesNeeded - 1, remainingChangesAvailable - 1)
          }
          case (false, false, true) => {
            if (remainingChangesAvailable - remainingChangesNeeded >= 2) {
              arr(i) = '9'
              arr(len - i - 1) = '9'
              helper(i + 1, arr, remainingChangesNeeded, remainingChangesAvailable - 2)
            }
            else {
              helper(i + 1, arr, remainingChangesNeeded, remainingChangesAvailable)
            }
          }
          case (false, false, false) => {
            if (remainingChangesAvailable - remainingChangesNeeded >= 1) {
              arr(i) = '9'
              arr(len - i - 1) = '9'
              helper(i + 1, arr, remainingChangesNeeded - 1, remainingChangesAvailable - 2)
            }
            else {
              if (leftChar > rightChar) {
                arr(len - i - 1) = leftChar
                helper(i + 1, arr, remainingChangesNeeded - 1, remainingChangesAvailable - 1)
              }
              else {
                arr(i) = rightChar
                helper(i + 1, arr, remainingChangesNeeded - 1, remainingChangesAvailable - 1)
              }
            }
          }
        }
      }
    }

    val minimumChangesNeeded = findMinimumChangesNeeded(s)

    if (minimumChangesNeeded > k) {
      "-1"
    }
    else {
      val result = helper(0, s.toCharArray, minimumChangesNeeded, k)
      result.mkString
    }

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val nk = stdin.readLine.split(" ")

    val n = nk(0).trim.toInt

    val k = nk(1).trim.toInt

    val s = stdin.readLine

    val result = highestValuePalindrome(s, n, k)

    println(result)
  }

}
