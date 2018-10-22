package FunctionalProgramming



/**
  * Created by creich on 3/16/18.
  */

import java.io.{File, FileInputStream, IOException, InputStream}
object FightingArmies3 {

  class InputReader(val stream : InputStream = System.in, val bufferSize: Int = 1 << 16) {
    private var c : Int = 0

    private val ints : Array[Int] = new Array[Int](58)
    private var value = 0
    for (i <- 48 until 58) {
      ints(i) = value
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

      do {
        while (bufIndex < numBytesRead) {
          if (buf(bufIndex) > token) {
            return 0
          }
          bufIndex = bufIndex + 1
        }

        numBytesRead = stream.read(buf)
        if (numBytesRead == EOF) {
          return EOF
        }
        bufIndex = 0
      } while (true)

      return -999999

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
      return -99999
    }

  }


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }



  def run(n : Int, q: Int, sc : InputReader) : Unit = {
    var i = 0
    var c = 0
    var j = 0


    val arr = new Array[(Boolean, List[Int])](n + 1)

    for (z <- 0 until q) {
      sc.nextInt() match {
        case 1 => {
          i = sc.nextInt()
          arr(i) match {
            case (true, l) => {
              println(l.head)
            }
            case (false, l) => {
              val sorted = l.sortBy(e => -e)
              println(sorted.head)
              arr(i) = (true, sorted)
            }
          }
        }
        case 2 => {
          i = sc.nextInt()
          arr(i) match {
            case (true, l) => {
              arr(i) = (true, l.tail)
            }
            case (false, l) => {
              val sorted = l.sortBy(e => -e)
              arr(i) = (true, sorted.tail)
            }
          }
        }
        case 3 => {
          i = sc.nextInt()
          c = sc.nextInt()
          if (arr(i) == null) {
            arr(i) = (true, List(c))
          }
          else {
            arr(i) match {
              case (true, l) => {
                if (c >= l.head) {
                  arr(i) = (true, c :: l)
                }
                else {
                  arr(i) = (false, c :: l)
                }
              }
              case (_ , l) => {
                arr(i) = (false, c :: l)
              }
            }
          }
        }
        case 4 => {
          i = sc.nextInt()
          j = sc.nextInt()
          val armyI = arr(i)
          val armyJ = arr(j)
          arr(j) = null
          arr(i) = (false, armyI._2 ::: armyJ._2)
        }
      }
    }


  }


  def main(args : Array[String]) : Unit = {
    val f : File = new File("/Users/creich/Desktop/army_17.txt")
    val is : InputStream  = new FileInputStream(f)
    val sc = new InputReader(is)
    val n = sc.nextInt()
    val q = sc.nextInt()
    time{run(n, q, sc)}
  }

}
