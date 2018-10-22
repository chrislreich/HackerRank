package FunctionalProgramming

/**
  * Created by creich on 1/9/18.
  */
object ComputeEToTheX {
  def computeFactorial(i : Int) : Int = {
    def factHelper(inpt : Int, product : Int) : Int = {
      if (inpt > 0) factHelper(inpt - 1, product * inpt)
      else product
    }

    factHelper(i, 1)
  }

  def computeTerm(d : Double, termNumber : Int) : Double = {
    scala.math.pow(d, termNumber)/computeFactorial(termNumber)
  }

  def computeX(d : Double) : Double = {
    1.0 + (1 to 9).toList.foldLeft(0.0)((x, y) => x + computeTerm(d, y))
  }


  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var a0 = 0;
    while(a0 < n){
      var x = sc.nextDouble();
      a0+=1;
      println("%.4f".format(computeX(x)))

    }
  }

}
