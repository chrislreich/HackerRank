package FunctionalProgramming

/**
  * Created by creich on 1/9/18.
  */
object PrintPascalTriangle {

  def factorial(i : Int) : Int = {
    def helper(cur: Int, product : Int) : Int = {
      if (cur == 0) product
      else helper(cur - 1, product * cur)
    }
    helper(i, 1)
  }

  def computePascalElement(row : Int, column : Int) : Int = {
    factorial(row)/(factorial(column) * factorial(row - column))
  }

  def rowString(i : Int) : IndexedSeq[Int] = {
    for (x <- 0 until i + 1) yield computePascalElement(i, x)
  }

  def formatTriangleRow(seq : IndexedSeq[Int]) : String = {
    seq.addString(new StringBuilder(), " ").toString()
  }



  def main(args : Array[String]) : Unit = {
    val sc = new java.util.Scanner (System.in);
    val k = sc.nextInt()
    val triangle = for (j <- 0 until k) yield rowString(j)

    for (row <- triangle) println(formatTriangleRow((row)))

  }

}
