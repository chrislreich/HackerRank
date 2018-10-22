package FunctionalProgramming

/**
  * Created by creich on 3/19/18.
  */
object MinimumMultiple {

  val lookupArray = primeFactorize
  val m = 1000000007

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  def primeSieve : List[Int] = {

    def helper(inputList : List[Int], returnList : List[Int]) : List[Int] = {
      inputList match {
        case x :: xs => {
          helper(xs.filterNot(e => e % x == 0), x :: returnList)
        }
        case _ => returnList.reverse
      }
    }

    val initialList = (2 to 100).toList
    helper(initialList, Nil)
  }

  def primeFactorize : Array[List[(Int, Int)]] = {
    val returnArray = new Array[List[(Int, Int)]](101)
    val primes = primeSieve

    def helper(primeList : List[Int], lastPrime : Int, currentVal : Int, returnList : List[(Int, Int)]) : List[(Int, Int)] = {
      if (currentVal == 1) {
        returnList.reverse
      }
      else {
        primeList match {
          case x :: xs => {
            if (currentVal % x == 0) {
              if (x == lastPrime) {
                val addOne = returnList.head
                helper(primeList, lastPrime, currentVal / x, (x, addOne._2 + 1) :: returnList.tail)
              }
              else {
                helper(primeList, x, currentVal / x, (x, 1) :: returnList)
              }
            }
            else {
              helper(xs, lastPrime, currentVal, returnList)
            }
          }
          case _ => returnList.reverse
        }
      }
    }

    returnArray(1) = List()
    for (k <- 2 until 101) {
      returnArray(k) = helper(primes, -1, k, Nil)
    }
    returnArray
  }

  class TreeNode(val start : Int, val end : Int, val primeFactors : List[(Int, Int)], val leftChild : Option[TreeNode] = None, val rightChild : Option[TreeNode] = None) {
    override def toString() : String = {
      "Start: " + start + "\nEnd: " + end + "\nPrime Factors: " + primeFactors.mkString(" ")
    }
  }

  def combinePrimeFactors(left : List[(Int, Int)], right : List[(Int, Int)], retList : List[(Int, Int)]) : List[(Int, Int)] = {
    (left, right) match {
      case (l :: ls, r :: rs) => {
        if (l._1 == r._1) {
          combinePrimeFactors(ls, rs, (l._1, scala.math.max(l._2, r._2)) :: retList)
        }
        else {
          if (l._1 < r._1) {
            combinePrimeFactors(ls, right, l :: retList)
          }
          else {
            combinePrimeFactors(left, rs, r :: retList)
          }
        }
      }
      case (Nil, r :: rs) => {
        combinePrimeFactors(Nil, rs, r :: retList)
      }
      case (l :: ls, Nil) => {
        combinePrimeFactors(ls, Nil, l :: retList)
      }
      case (Nil, Nil) => {
        retList.reverse
      }
    }
  }


  def makeSegmentTree(input : Array[Int]) : TreeNode = {

    def helper(s : Int, e : Int) : TreeNode = {
      if (s == e) {
        new TreeNode(s, e, lookupArray(input(s)))
      }
      else {
        val midPoint = (s + e) / 2
        val leftChild = helper(s, midPoint)
        val rightChild = helper(midPoint + 1, e)
        val combinedFactors = combinePrimeFactors(leftChild.primeFactors, rightChild.primeFactors, Nil)
        new TreeNode(s, e, combinedFactors, Some(leftChild), Some(rightChild))
      }
    }

    helper(0, input.length - 1)
  }

  def queryTree(s : Int, e : Int, root : TreeNode) : Int = {

    def tupleToInt(num : Int, pow : Int, current : Long) : Long = {
      if (pow == 0) {
        current
      }
      else {
        tupleToInt(num, pow - 1, (current * num) % m)
      }

    }

    def helper(t : TreeNode) : Option[TreeNode] = {
      if (t.start > e || t.end < s) {
        None
      }
      else {
        if (t.start >= s && t.end <= e) {
          Some(t)
        }
        else {
          val leftOption = t.leftChild match {
            case None => None
            case Some(v) => helper(v)
          }
          val rightOption = t.rightChild match {
            case None => None
            case Some(v) => helper(v)
          }

          (leftOption, rightOption) match {
            case (Some(l), Some(r)) => {
              Some(new TreeNode(t.start, t.end, combinePrimeFactors(l.primeFactors, r.primeFactors, Nil)))
            }
            case (Some(l), None) => {
              Some(new TreeNode(t.start, t.end, l.primeFactors))
            }
            case (None, Some(r)) => {
              Some(new TreeNode(t.start, t.end, r.primeFactors))
            }
            case (None, None) => {
              None
            }
          }
        }
      }
    }

    val returnValue = helper(root) match {
      case Some(r) => r
      case None => new TreeNode(-1, -1, Nil)
    }

    returnValue.primeFactors.foldRight(1L)((t, b) => (tupleToInt(t._1, t._2, 1L) * b) % m).toInt
  }


  def updateTree(index : Int, newValue : Int, root : TreeNode) : TreeNode = {
    def multiplyFactorLists(left : List[(Int, Int)], right : List[(Int, Int)], retList : List[(Int, Int)]) : List[(Int, Int)] = {
      (left, right) match {
        case (l :: ls, r :: rs) => {
          if (l._1 == r._1) {
            multiplyFactorLists(ls, rs, (l._1, l._2 + r._2) :: retList)
          }
          else {
            if (l._1 < r._1) {
              multiplyFactorLists(ls, right, l :: retList)
            }
            else {
              multiplyFactorLists(left, rs, r :: retList)
            }
          }
        }
        case (Nil, r :: rs) => {
          multiplyFactorLists(Nil, rs, r :: retList)
        }
        case (l :: ls, Nil) => {
          multiplyFactorLists(ls, Nil, l :: retList)
        }
        case (Nil, Nil) => {
          retList.reverse
        }
      }
    }
    def helper(t : TreeNode) : TreeNode = {
      if (t.start == t.end && t.start == index) {
        val multipleFactor = lookupArray(newValue)
        val multipliedFactors = multiplyFactorLists(t.primeFactors, multipleFactor, Nil)
        new TreeNode(t.start, t.start, multipliedFactors)
      }
      else {
        val midPoint = (t.start + t.end) / 2
        val currentLeftChild = t.leftChild match {
          case Some(leftChild) => leftChild
          case None => new TreeNode(-1, -1, Nil)
        }
        val currentRightChild = t.rightChild match {
          case Some(rightChild) => rightChild
          case None => new TreeNode(-1, -1, Nil)
        }
        if (index <= midPoint) {
          val newLeftChild = helper(currentLeftChild)
          val newPrimeFactors = combinePrimeFactors(newLeftChild.primeFactors, currentRightChild.primeFactors, Nil)
          new TreeNode(t.start, t.end, newPrimeFactors, Some(newLeftChild), Some(currentRightChild))
        }
        else {
          val newRightChild = helper(currentRightChild)
          val newPrimeFactors = combinePrimeFactors(currentLeftChild.primeFactors, newRightChild.primeFactors, Nil)
          new TreeNode(t.start, t.end, newPrimeFactors, Some(currentLeftChild), Some(newRightChild))
        }
      }
    }

    helper(root)
  }


  def run : Unit = {
    val n = readInt()
    val inputArray = readLine().split(" ").map(_ toInt)
    var runningRoot = makeSegmentTree(inputArray)
    val q = readInt()

    for (i <- 0 until q) {
      val line = readLine().split(" ")
      if (line(0) == "Q") {
        println(queryTree(line(1).toInt, line(2).toInt, runningRoot))
      }
      else {
        runningRoot = updateTree(line(1).toInt, line(2).toInt, runningRoot)
      }
    }
  }


  def main(args : Array[String]) : Unit = {
     run
  }

}
