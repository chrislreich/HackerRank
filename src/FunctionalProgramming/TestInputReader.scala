package FunctionalProgramming

import java.io.{IOException, InputStream}

/**
  * Created by creich on 3/16/18.
  */
object TestInputReader {


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


  def main(args : Array[String]) : Unit = {
    val stdinReader = new InputReader()
    val i1 = stdinReader.nextInt()
    println("!! " + i1)
    val i2 = stdinReader.nextInt()
    println("?? " + i2)
    val i3 = stdinReader.nextInt()
    println("$$ " + i3)

  }

}
