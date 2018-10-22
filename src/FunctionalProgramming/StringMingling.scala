package FunctionalProgramming

/**
  * Created by creich on 1/10/18.
  */
object StringMingling {

  def zipStrings(p : String, q : String) : List[(Char, Char)] = {
    (p.toList zip q.toList)


  }

  def main(args : Array[String]) : Unit = {
    val sc =  new java.util.Scanner(System.in)
    val p = sc.nextLine()
    val q = sc.nextLine()
    for (t <- zipStrings(p,q)) {
      print(t._1)
      print(t._2)
    }
  }

}
