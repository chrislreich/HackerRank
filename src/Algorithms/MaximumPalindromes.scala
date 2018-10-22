package Algorithms

/**
  * Created by creich on 8/22/18.
  */
object MaximumPalindromes {

  class TreeNode(val leftIndex: Int, val rightIndex: Int, val charMap: Map[Char, Int], val leftChild: Option[TreeNode] = None, val rightChild: Option[TreeNode] = None)

  var rootNode: TreeNode = null
  val m: Int = 1000000007

  val factorialMap = scala.collection.mutable.HashMap[Int, Int](1 -> 1)
  val inverseFactorialMap = scala.collection.mutable.HashMap[Int, Int](1 -> 1)

  var maxFactorialCalculated = 1

  def combineMaps(m1: Map[Char, Int], m2: Map[Char, Int]): Map[Char, Int] = {
    val tuples = for (k <- (m1.keySet | m2.keySet))
      yield {
        (m1.get(k), m2.get(k)) match {
          case (Some(v1), Some(v2)) => {
            (k, v1 + v2)
          }
          case (Some(v1), None) => {
            (k, v1)
          }
          case (None, Some(v2)) => {
            (k, v2)
          }
          case (None, None) => {
            (k, 0)
          }
        }
      }
    tuples.toMap
  }

  def buildSegmentTree(inputString: String): TreeNode = {
    def helper(leftIndex: Int, rightIndex: Int): TreeNode = {
      if (leftIndex == rightIndex) {
        val map = Map[Char, Int](inputString(leftIndex - 1) -> 1)
        new TreeNode(leftIndex, rightIndex, map)
      }
      else {
        val midPoint = (leftIndex + rightIndex) / 2
        val leftChild = helper(leftIndex, midPoint)
        val rightChild = helper(midPoint + 1, rightIndex)
        val thisMap = combineMaps(leftChild.charMap, rightChild.charMap)
        new TreeNode(leftIndex, rightIndex, thisMap, Some(leftChild), Some(rightChild))
      }
    }
    val stringLength = inputString.length
    helper(1, stringLength)
  }

  def query(l: Int, r: Int): Option[Map[Char, Int]] = {
    def helper(left: Int, right: Int, node: Option[TreeNode]) : Option[Map[Char, Int]] = {
      node match {
        case Some(n) => {
          if (left > n.rightIndex || right < n.leftIndex) {
            None
          }
          else {
            if (left <= n.leftIndex && right >= n.rightIndex) {
              Some(n.charMap)
            }
            else {
              (helper(left, right, n.leftChild), helper(left, right, n.rightChild)) match {
                case (Some(ml), Some(mr)) => {
                  val combined = combineMaps(ml, mr)
                  Some(combined)
                }
                case (Some(m1), None) => {
                  Some(m1)
                }
                case (None, Some(m2)) => {
                  Some(m2)
                }
                case (None, None) => {
                  None
                }
              }
            }
          }
        }
        case None => {
          None
        }
      }
    }

    helper(l, r, Some(rootNode))
  }

  def calculateFactorial(n: Int) = {
    val n_1 = factorialMap.getOrElse(n - 1, -1)
    val n_val = (n_1 * n) % m
    factorialMap += (n -> n_val)
  }

  def calculateInverseFactorial(n: Int) = {
    val dp = m % n
    val mapVal = inverseFactorialMap.getOrElse(dp, -1)
    val subtractionVal = m - (m / n)
    val inverse = mapVal * subtractionVal % m
    inverseFactorialMap += (n -> inverse)
  }



  def populateFactorialMaps(n: Int, max: Int): Unit = {
    if (n >= max) {
      calculateFactorial(n)
      calculateInverseFactorial(n)
    }
    else {
      calculateFactorial(n)
      calculateInverseFactorial(n)
      populateFactorialMaps(n + 1, max)
    }
  }

  def findCombinations(map: Map[Char, Int]) : Int = {
    if (map.size == 1) {
      1
    }
    else {
      val numPairs = map.mapValues(x => x / 2)
      val numerator = numPairs.foldLeft(0)((b, x) => x._2 + b)

      if (numerator > maxFactorialCalculated) {
        populateFactorialMaps(maxFactorialCalculated + 1, numerator)
      }

      val numeratorVal = factorialMap.getOrElse(numerator, 0)

      val denominatorVal = numPairs.foldLeft(1)((b, x) => (inverseFactorialMap.getOrElse(x._2, 0) * b) % m)

      val combinations = numeratorVal * denominatorVal % m

      val numExtraChars = map.mapValues(x => x % 2).foldLeft(0)((b, y) => b + y._2)

      if (numExtraChars == 0) {
        combinations
      }
      else {
        if (combinations == 0) {
          numExtraChars
        }
        else {
          numExtraChars * combinations % m
        }
      }
    }
  }



  // Complete the initialize function below.
  def initialize(s: String) {
    rootNode = buildSegmentTree(s)
  }

  // Complete the answerQuery function below.
  def answerQuery(l: Int, r: Int): Int = {
    query(l, r) match {
      case Some(k) => {
        findCombinations(k)
      }
      case None => {
        0
      }
    }

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val s = stdin.readLine

    initialize(s)
    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val lr = stdin.readLine.split(" ")

      val l = lr(0).trim.toInt

      val r = lr(1).trim.toInt

      val result = answerQuery(l, r)

      println(result)
    }
  }

}
