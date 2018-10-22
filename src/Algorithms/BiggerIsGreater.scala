package Algorithms

/**
  * Created by creich on 8/16/18.
  */
object BiggerIsGreater {

  def findIndex(input: String) : Option[(Char, Int)] = {
    def helper(list : List[(Char, Int)], curRetVal: Option[(Char, Int)]) : Option[(Char, Int)] = {
      list match {
        case x :: xs => {
          if (xs.exists(y => y._1 > x._1)) {
            helper(xs, Some(x))
          }
          else {
            helper(xs, curRetVal)
          }
        }
        case _ => {
          curRetVal
        }
      }
    }
    val zipped: Seq[(Char, Int)] = input.zipWithIndex
    helper(zipped.toList, None)
  }

  def biggerIsGreater(w: String): String = {

    findIndex(w) match {
      case None => {
        "no answer"
      }
      case Some(v) => {
        val split = w.splitAt(v._2)
        val prefix = split._1
        val suffix = split._2.toList
        val changeChar: Char = suffix.head
        val sortedSuffixTail: List[Char] = suffix.tail.sortWith((c1, c2) => c1 < c2 && c1 > changeChar)
        val newChangeChar = sortedSuffixTail.head
        val newSuffixTail = (changeChar :: sortedSuffixTail.tail).sorted
        prefix ++ newChangeChar.toString() ++ newSuffixTail
      }
    }

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn



    val T = stdin.readLine.trim.toInt

    for (i <- 1 to T) {
      val w = stdin.readLine

      val result = biggerIsGreater(w)

      println(result)
    }


  }

}
