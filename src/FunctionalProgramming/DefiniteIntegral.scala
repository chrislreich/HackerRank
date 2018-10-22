package FunctionalProgramming

/**
  * Created by creich on 1/9/18.
  */
object DefiniteIntegral {

  def f(coefficients:List[Int],powers:List[Int],x:Double):Double =
  {
    val retSum = (coefficients zip powers).map{case (c, p) => scala.math.pow(x, p) * c * 0.001}.sum
    //println(retSum)
    retSum
  }

  def area(coefficients:List[Int],powers:List[Int],x:Double):Double =
  {
    val retSum = (coefficients zip powers).map{case (c, p) => scala.math.pow(x, p) * c}.sum
    retSum * retSum * scala.math.Pi *  0.001
  }




  def summation(func:(List[Int],List[Int],Double)=>Double,upperLimit:Int,lowerLimit:Int,coefficients:List[Int],powers:List[Int]):Double =
  {
    def createIntervals(upper: Int, lower : Int) : List[Double] = {
      def helper(current : Double, upperLim : Double, curList : List[Double]) : List[Double] = {
        if (current >= upperLim) curList
        else helper(current + 0.001, upperLim, current :: curList)
      }

      helper(lower.toDouble, upper.toDouble, List.empty)
    }


    createIntervals(upperLimit, lowerLimit).map(k => func(coefficients, powers, k)).sum

  }





  def lineToList(str : String) : List[Int] = {
    str.split(" ").toList.map(x => x.toInt)
  }


  def main(args : Array[String]): Unit = {
    val sc = new java.util.Scanner (System.in);
    val aCoeffs = sc.nextLine()
    val bCoeffs = sc.nextLine()
    val interval = sc.nextLine()

    val aList = lineToList(aCoeffs)
    val bList = lineToList(bCoeffs)
    val intervalList = lineToList(interval)
    val lower = intervalList(0)
    val upper = intervalList(1)



    println("%.1f".format(summation(f, upper, lower, aList, bList)))
    println("%.1f".format(summation(area, upper, lower, aList, bList)))


  }

}
