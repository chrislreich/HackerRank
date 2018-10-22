package FunctionalProgramming

/**
  * Created by creich on 3/15/18.
  */

object FightingArmies2 {

  /*

  class InputReader(val stream : InputStream = System.in, val bufferSize: Int = 1 << 16) {
    private var c : Int = 0

    private val ints : Array[Int] = new Array[Int](58)
    private var value = 0
    for (i <- 48 until 58) {
      ints(i) == value
      value = value + 1
    }

    private val buf : Array[Byte] = new Array[Byte](bufferSize)
    private var bufIndex : Int = 0
    private var numBytesRead : Int = 0


    private val EOF : Byte = -1
    private val NL : Byte = 10
    private val SP : Byte = 32
    private val DASH : Byte = 45
    private val DOT : Byte = 46




    private def readJunk(token : Int) : Int  = {
      if (numBytesRead == EOF) {
        return EOF
      }
      else {
        do {
          while(bufIndex < numBytesRead) {
            if (buf(bufIndex) > token) {
              return 0
            }
            else {
              bufIndex = bufIndex + 1
            }
          }

          numBytesRead = stream.read(buf)
          if (numBytesRead == EOF) {
            return EOF
          }
          else {
            bufIndex = 0
          }
        } while (true)
      }
    }

    def nextInt() : Int = {
      if (readJunk(DASH - 1) == EOF) {
        throw new IOException()
      }
      var sgn : Int = 1
      var res : Int = 0

      c = buf(bufIndex)
      if (c == DASH) {
        sgn = -1
        bufIndex = bufIndex + 1
      }

      do {
        while (bufIndex < numBytesRead) {
          if (buf(bufIndex) > SP) {
            res = (res << 3) + (res << 1)
            res = res + ints(buf(bufIndex))
            bufIndex = bufIndex + 1
          }
          else {
            bufIndex = bufIndex + 1
            return res * sgn
          }
        }

        numBytesRead = stream.read(buf)
        if (numBytesRead == EOF) {
          return res * sgn
        }
        bufIndex = 0
      } while (true)
    }

  }




*/




  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  var map : Array[Option[(Boolean, List[Int])]] = null


  def nextState(command : String) : Unit = {
    val commandArr = command.split(" ")
    val i = commandArr(1).toInt

    commandArr(0).toInt match {
      case 1 => {
        val retVal = map(i) match {
          case Some(x) => x
        }
        if (retVal._1) {
          println(retVal._2.head)
        }
        else {
          val sortedList = retVal._2.sortBy(e => -e)
          println(sortedList.head)
          map(i) = Some((true, sortedList))
        }
      }
      case 2 => {
        val retVal = map(i) match {
          case Some(x) => x
        }
        if (retVal._1) {
          map(i) = Some((true, retVal._2.tail))
        }
        else {
          val sortedList = retVal._2.sortBy(e => -e)
          map(i) = Some((true, sortedList.tail))
        }
      }
      case 3 => {
        val c = commandArr(2).toInt
        map(i) match {
          case None => {
            map(i) = Some((true, List(c)))
          }
          case Some(x) => {
            if (x._1) {
              if (c > x._2.head) {
                map(i) = Some((true, c :: x._2))
              }
              else {
                map(i) = Some((false, c :: x._2))
              }
            }
            else {
              map(i) = Some((false, c :: x._2))
            }
          }
        }
      }
      case 4 => {
        val j = commandArr(2).toInt
        val retVali = map(i) match {case Some(x) => x}
        val retValj = map(j) match {case Some(x) => x}
        map(i) = Some((false, retVali._2 ::: retValj._2))
        map(j) = None
      }
    }
  }




  def stdInSource : Unit = {
    val firstLine = scala.io.StdIn.readLine().split(" ")
    val n = firstLine(0).toInt
    map = Array.fill[Option[(Boolean, List[Int])]](n + 1)(None)
    val q = firstLine(1).toInt

time {
  for (i <- 0 until q) {
    nextState(scala.io.StdIn.readLine())
  }
}

  }



  def main(args : Array[String]) : Unit = {
    stdInSource
  }

}
