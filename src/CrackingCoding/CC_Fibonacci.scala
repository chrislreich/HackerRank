package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_Fibonacci {

  def fibonacci(x:Int):Int = {
    def helper(x1 : Int, x2 : Int, counter : Int) : Int = {
      if (counter == x) {
        x2
      }
      else {
        helper(x2, x1 + x2, counter + 1)
      }
    }

    x match {
      case 0 => 0
      case 1 => 1
      case 2 => 1
      case _ => helper(1, 1, 2)
    }
  }

  def main(args: Array[String]) {
    /** This will handle the input and output**/
    println(fibonacci(readInt()))

  }

}
