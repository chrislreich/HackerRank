package CrackingCoding

/**
  * Created by creich on 5/23/18.
  */
object CC_StackMatchingBrackets {

  def matchBrackets(c1 : Char, c2 : Char) : Boolean = {
    (c1, c2) match {
      case (']', '[') => {
        true
      }
      case ('}', '{') => {
        true
      }
      case (')', '(') => {
        true
      }
      case (_, _) => {
        false
      }
    }
  }


  def consume(str : String, stack : List[Char]) : List[Char] = {
    if (str isEmpty) {
      stack
    }
    else {
      if (stack isEmpty) {
        consume(str.tail, str.head :: stack)
      }
      else {
        if (matchBrackets(str.head, stack.head)) {
          consume(str.tail, stack.tail)
        }
        else {
          consume(str.tail, str.head :: stack)
        }
      }
    }
  }



  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
      val expression = stdin.readLine

      consume(expression, List[Char]()) match {
        case x :: xs => println("NO")
        case _ => println("YES")
      }

    }
  }

}
