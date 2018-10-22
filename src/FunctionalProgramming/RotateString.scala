package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */
object RotateString {

  def stringRotations(s : String) : List[String] = {
    def helper(count : Int, curVec : Vector[Char], retList : List[String]) : List[String] = {
      if (count == 0) {
        retList
      }
      else {
        helper(count - 1, curVec.last +: curVec.init, curVec.mkString :: retList)
      }
    }

    helper(s.length, s.toVector, Nil)
  }

  def run : Unit = {
    val n = readInt()
    for (i <- 0 until n) {
      println(stringRotations(readLine()).mkString(" "))
    }
  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
