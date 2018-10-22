package Algorithms

/**
  * Created by creich on 8/9/18.
  */
object CatAndMouse {

  def catAndMouse(x: Int, y: Int, z: Int): String = {
    val catADistance = scala.math.abs(z - x)
    val catBDistance = scala.math.abs(z - y)

    catADistance.compareTo(catBDistance) match {
      case 1 => "Cat B"
      case -1 => "Cat A"
      case 0 => "Mouse C"
    }
  }
}
