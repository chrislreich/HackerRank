package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_Primality {

  def isPrime(n : Int) : Boolean = {
    def helper(i : Int, root : Int) : Boolean = {
      if (i > root) {
        true
      }
      else {
        if (n % i == 0) {
          false
        }
        else {
          helper(i + 1, root)
        }
      }
    }
    if (n == 1) false
    else {
      val root = scala.math.sqrt(n).toInt
      helper(2, root)
    }

  }

  def run = {
    val p = readInt()
    for (i <- 0 until p) {
      val result = isPrime(readInt())
      if (result) {
        println("Prime")
      }
      else {
        println("Not prime")
      }
    }

  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
