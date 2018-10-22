package FunctionalProgramming

/**
  * Created by creich on 1/25/18.
  */
object KnuthMorrisPratt {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def substring_?(w : String, p : String) : String = {
    val inputLength = p.length()
    def prefixValue(s : String, prefix : String) : Int = {
      if (prefix.isEmpty)
        0
      else {
        val len = prefix.length
        if (prefix == s.takeRight(len)) {
          len
        }
        else {
          prefixValue(s, prefix.init)
        }
      }
    }

    def computePrefixTable(input : String) : Map[Int, Int] = {


      def helper(curMap : Map[Int, Int], curCount : Int) : Map[Int, Int] = {
        if (curCount > inputLength) {
          curMap
        }
        else {

          helper(curMap + (curCount -> prefixValue(input.take(curCount), input.take(curCount - 1))), curCount + 1)
        }
      }
      val h = helper(Map[Int, Int](0 -> -1), 1)

      h
    }

    val prefixTable = time {computePrefixTable(p)}



    def traverse(curString : String) : Boolean = {
      if (curString.length < inputLength) {
        false
      }
      else {
        val numMatchedChars = (p zip curString).prefixLength(x => x._1 == x._2)
        if (numMatchedChars == inputLength){
          true
        }
        else {
          val shiftAmount = numMatchedChars - prefixTable.getOrElse(numMatchedChars, 0)
          traverse(curString.drop(shiftAmount))
        }
      }

    }



    val result = time {traverse(w)}

    if (result) {
      "YES"
    }
    else {
      "NO"
    }


  }


  def main(args : Array[String]) : Unit = {
    val t = readInt()

    for (i <- 0 until t) {
      val word = readLine()
      val pattern = readLine()
      println(substring_?(word, pattern))
      return
    }
  }

}

object Sol {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }




  def is_substring(w : String, p : String) : String = {
    val costTable = scala.collection.mutable.HashMap[Int, Int]()
    def computeTable : Unit = {
      def subroutine(k : Int, curChar : Char) : Int = {
        val compChar = p(k + 1)
        if (curChar == compChar) {
          k + 1
        }
        else {
          if (k < 0) {
            k
          }
          else {
            subroutine(costTable.getOrElse(k, -9), curChar)
          }
        }

      }

      val len = p.length

      costTable += (0 -> -1)
      var k = -1
      for (i <- 1 until len) {
        k = subroutine(k, p(i))
        costTable += (i -> k)

      }



    }

    computeTable

    def findSubstring : Boolean = {
      def subroutine(q : Int, curChar : Char) : Int = {
        val compChar = p(q + 1)
        if (curChar == compChar) {
          q + 1
        }
        else {
          if (q < 0) {
            q
          }
          else {
            subroutine(costTable.getOrElse(q, -9), curChar)
          }
        }

      }

      def iterate(i : Int, q : Int) : Boolean = {
        if (w.length - i < p.length - (q + 1)) {
          false
        }
        else {
          val curChar = w(i)
          val newQ = subroutine(q, curChar)
          if (newQ == p.length - 1) {
            true
          }
          else{
            iterate(i + 1, newQ)
          }
        }
      }

      iterate(0, -1)
    }



    if (findSubstring)
      "YES"
    else
      "NO"




  }

  def s_2 (w : String, p : String) : Boolean = {

    val makeTable : Array[Int] = {
      val retArr = new Array[Int](p.length)
      retArr(0) = -1
      var k = -1
      for (i <- 1 until p.length) {
        val curChar = p(i)
        while(k >= 0 && curChar != p(k + 1)) {
          k = retArr(k)
        }
        if (curChar == p(k + 1)) {
          k += 1
        }
        retArr(i) = k
      }
      retArr
    }


    val arr = time{makeTable}

    true
  }


  def main(args : Array[String]) : Unit = {
    val t = readInt()

    for (i <- 0 until t) {
      val word = readLine()
      val pattern = readLine()
      println(is_substring(word, pattern))
     // s_2(word, pattern)
    }
  }

}
