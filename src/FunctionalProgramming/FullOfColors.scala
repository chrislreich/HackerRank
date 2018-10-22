package FunctionalProgramming

/**
  * Created by creich on 1/19/18.
  */
object FullOfColors {

  def fullOfColor_?(str : String) : Boolean = {

    def helper(curString: List[Char], r: Int, g: Int, b: Int, y: Int): Boolean = {
      if (scala.math.abs(r - g) > 1 || scala.math.abs(b - y) > 1)
        false
      else {
        curString match {
          case x :: xs => {
            x match {
              case 'R' => helper(xs, r + 1, g, b, y)
              case 'G' => helper(xs, r, g + 1, b, y)
              case 'B' => helper(xs, r, g, b + 1, y)
              case 'Y' => helper(xs, r, g, b, y + 1)
            }
          }
          case _ => {
            if (r == g && b == y)
              true
            else
              false
          }
        }
      }
    }

    helper(str.toList, 0, 0, 0, 0)
  }

  def main(args : Array[String]) : Unit = {
    val t = readInt()
    for (i <- 0 until t)
      println(fullOfColor_?(readLine()) match {case true => "True"; case false => "False"})
  }

}
