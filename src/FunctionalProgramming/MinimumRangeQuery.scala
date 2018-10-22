package FunctionalProgramming

/**
  * Created by creich on 2/9/18.
  */
object MinimumRangeQuery {


  class SegmentTree(inputList : List[Int])  {
    val root = makeSegmentTree(inputList.zipWithIndex)


    class TreeNode(val minimum : Int, val start : Int, val end : Int, val leftChild : TreeNode = null, val rightChild : TreeNode = null)


    def makeSegmentTree(l : List[(Int, Int)]) : TreeNode = {
      if (l.length == 1) {
        new TreeNode(l.head._1, l.head._2, l.head._2)
      }
      else {
        val splitNum = ((l.last._2 - l.head._2) / 2) + 1
        val splitLists = l.splitAt(splitNum)
        val leftList = splitLists._1
        val rightList = splitLists._2

        val leftChild = makeSegmentTree(leftList)
        val rightChild = makeSegmentTree(rightList)

        val newMin = scala.math.min(leftChild.minimum, rightChild.minimum)

        new TreeNode(newMin, l.head._2, l.last._2, leftChild, rightChild)
      }
    }

    def query(i : Int, j : Int) : Int = {
      def helper(t : TreeNode) : Option[Int] = {
        if (t.start > j || t.end < i) {
          None
        }
        else {
          if (j >= t.end && i <= t.start) {
            Some(t.minimum)
          }
          else {
            val leftOption = if (t.leftChild == null) {
              None
            }
            else {
              helper(t.leftChild)
            }

            val rightOption = if (t.rightChild == null) {
              None
            }
            else {
              helper(t.rightChild)
            }

            (leftOption, rightOption) match {
              case (None, None) => None
              case (Some(l), None) => Some(l)
              case (None, Some(r)) => Some(r)
              case (Some(l), Some(r)) => Some(scala.math.min(l, r))
            }
          }
        }
      }

      helper(root) match {
        case Some(k) => k
        case None => -999999
      }
    }
  }


  def main(args : Array[String]) : Unit = {
    val firstLine = readLine().split(" ")
    val n = firstLine(0).toInt
    val m = firstLine(1).toInt
    val arr = readLine().split(" ").map(_.toInt).toList

    val segTree = new SegmentTree(arr)




    for(i <- 0 until m) {
      val line = readLine().split(" ")
      val min = line(0).toInt
      val max = line(1).toInt

      println(segTree.query(min, max))

    }

  }

}
