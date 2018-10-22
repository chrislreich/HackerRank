package FunctionalProgramming

/**
  * Created by creich on 2/1/18.
  */

import scala.collection.mutable
object MaxSubArraySegmentTree {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  class SegmentTree(inputList : Array[Int]) {
    private val rootNode : TreeNode =  makeSegmentTree(1, inputList.length)



    class TreeNode(val start : Int, val end : Int) {
      var left : TreeNode = null
      var right : TreeNode = null
      var maxSubArray : (Int, Int, BigInt) = null
      var prefixMax : (Int, Int, BigInt) = null
      var suffixMax : (Int, Int, BigInt) = null
      var childrenSum : BigInt = null
    }

    private def compareMaxSum(left : TreeNode, right : TreeNode) : (Int, Int, BigInt) = {
      val combinedMax = left.suffixMax._3 + right.prefixMax._3
      val leftMax = left.maxSubArray._3
      val rightMax = right.maxSubArray._3

      if (combinedMax > leftMax) {
        if (combinedMax >= rightMax) {
          (left.suffixMax._1, right.prefixMax._2, combinedMax)
        }
        else {
          right.maxSubArray
        }
      }
      else {
        if (leftMax >= rightMax) {
          left.maxSubArray
        }
        else {
          right.maxSubArray
        }
      }
    }

      private def comparePrefix(left : TreeNode, right : TreeNode) : (Int, Int, BigInt) = {
        val combined = left.childrenSum + right.prefixMax._3

        if (combined > left.prefixMax._3) {
          (left.start, right.prefixMax._2, combined)
        }
        else {
          left.prefixMax
        }
      }

    private def compareSuffix(left : TreeNode, right : TreeNode) : (Int, Int, BigInt) = {
      val combined = right.childrenSum + left.suffixMax._3

      if (combined >= right.suffixMax._3) {
        (left.suffixMax._1, right.end, combined)
      }
      else {
        right.suffixMax
      }
    }


    private def makeSegmentTree(s : Int, e : Int) : TreeNode = {
      if (e - s == 0) {
        val num = inputList(s - 1)
        val arr = (s,s, BigInt(num))
        val retNode = new TreeNode(s,s)
        retNode.maxSubArray = arr
        retNode.prefixMax = arr
        retNode.suffixMax = arr
        retNode.childrenSum = num
        retNode
      }
      else {
        val retNode = new TreeNode(s, e)
        val midPoint = (s + e)/2
        val leftChild = makeSegmentTree(s, midPoint)
        val rightChild = makeSegmentTree(midPoint + 1, e)

        retNode.maxSubArray = compareMaxSum(leftChild, rightChild)
        retNode.prefixMax = comparePrefix(leftChild, rightChild)
        retNode.suffixMax = compareSuffix(leftChild, rightChild)
        retNode.childrenSum = leftChild.childrenSum + rightChild.childrenSum

        retNode.left = leftChild
        retNode.right = rightChild

        retNode
      }
    }


    def query(i : Int, j : Int) : Option[(Int, Int, BigInt)] = {
      def helper(t : TreeNode) : Option[TreeNode] = {
        if (t.start > j || t.end < i) {
          None
        }
        else {
          if (i <= t.start && t.end <= j) {
            Some(t)
          }
          else {
            val leftOption = if (t.left == null) {
              None
            }
            else {
              helper(t.left)
            }

            val rightOption = if (t.right == null) {
              None
            }
            else {
              helper(t.right)
            }
            (leftOption, rightOption) match {
              case (None, None) => None
              case (None, Some(r)) => Some(r)
              case (Some(l), None) => Some(l)
              case (Some(l), Some(r)) => {
                val newNode = new TreeNode(t.start, t.end)
                newNode.maxSubArray = compareMaxSum(l, r)
                newNode.prefixMax = comparePrefix(l, r)
                newNode.suffixMax = compareSuffix(l, r)
                newNode.childrenSum = l.childrenSum + r.childrenSum
                Some(newNode)
              }
            }
          }
        }
      }

      helper(rootNode) match {
        case Some(x) => Some(x.maxSubArray)
        case None => None
      }

    }


  }




  def disjointSums(list : Array[Int], k : Int) : List[(Int, Int, BigInt)] = {
    val segmentTree = new SegmentTree(list)
    val initialMax: (Int, Int, BigInt) = segmentTree.query(1, list.length) match {
      case Some(m) => m
    }

    val pqOrdering: Ordering[((Int, Int), (Int, Int, BigInt))] = Ordering[(BigInt, Int, Int)].on[((Int, Int), (Int, Int, BigInt))](s => (s._2._3, -s._2._1, -s._2._2))
    val pq = new mutable.PriorityQueue[((Int, Int), (Int, Int, BigInt))]()(pqOrdering)
    pq += (((1, list.length), initialMax))




    def findSum(k: Int, pq: mutable.PriorityQueue[((Int, Int), (Int, Int, BigInt))], retList: List[(Int, Int, BigInt)]): List[(Int, Int, BigInt)] = {
      if (k == 0 || pq.isEmpty) {
        retList.reverse
      }
      else {
        val curMaxSub = pq.dequeue()

        if (curMaxSub._2._3 < 0) {
          retList.reverse
        }
        else {
          val partitionStart = curMaxSub._1._1
          val partitionEnd = curMaxSub._1._2
          val maxStart = curMaxSub._2._1 - 1
          val maxEnd = curMaxSub._2._2 + 1

          segmentTree.query(partitionStart, maxStart) match {
            case Some(l) => pq += (((partitionStart, maxStart), l))
            case None =>
          }
          segmentTree.query(maxEnd, partitionEnd) match {
            case Some(r) => pq += (((maxEnd, partitionEnd), r))
            case None =>
          }
          findSum(k - 1, pq, curMaxSub._2 :: retList)
        }
      }
    }


    findSum(k, pq, List())
  }





  def main(args: Array[String]) : Unit = {

    val line = readLine().split(" ")
    val n = line(0).toInt
    val k = line(1).toInt
    val inputList = readLine().split(" ").map(_.toInt)

    val result = disjointSums(inputList, k).map(x => x._3)

    println(result.mkString("\n"))
  }

}
