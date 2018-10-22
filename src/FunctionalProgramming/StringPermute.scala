package FunctionalProgramming

/**
  * Created by creich on 1/10/18.
  */
object StringPermute {

  def permuteString(str : String) : Iterator[Char] = {
    val charList = str.toList
    charList.sliding(2,2).flatMap(x => x.reverse)
  }


  def printString(s : String) : Unit = {
    permuteString(s) foreach print
  }


  def main(args : Array[String]) : Unit = {
    val sc = new java.util.Scanner(System.in)
    val t = sc.nextLine().toInt
    var cnt = 0
    while (cnt < t) {
      val line = sc.nextLine()
      printString(line)
      println()
      cnt = cnt + 1
    }

  }

}
