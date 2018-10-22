package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */
object KunduBubblePop {

  val e_m_constant_approx : Double = 0.5772156649

  def addTerm(n : Int) : Double = {
    ((1).toDouble)/(2 * n)
  }

  def minusTerm(n : Int) : Double = {
    ((1).toDouble)/(12 * n * n)
  }


  def calculate(n : Int) : Double = {
    if (n == 1) {
      1.toDouble
    }
    else {
      val nLog = scala.math.log(n)
      val nHarmonicApprox = nLog + addTerm(n) + e_m_constant_approx - minusTerm(n)
      nHarmonicApprox * n
    }
  }

  def run : Unit = {
    val line = readLine().split(" ").map(_ toInt)
    val num = line(0) * line(1)
    println(calculate(num))
  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
