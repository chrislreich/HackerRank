package CrackingCoding

/**
  * Created by creich on 6/1/18.
  */
object CC_HashTable_RansomNote {


  def createMap(input : List[String]) : Map[String, Int] = {
    def helper(l : List[String], retMap : Map[String, Int]) : Map[String, Int] = {
      if (l isEmpty) {
        retMap
      }
      else {
        val previousValue = retMap.getOrElse(l.head, 0)
        helper(l.tail, retMap + (l.head -> (previousValue + 1)))
      }
    }
    helper(input, Map[String, Int]())
  }

  def canMakeNote(ransomWords : List[(String, Int)], magazineMap : Map[String, Int]) : Boolean = {
    ransomWords match {
      case x :: xs => {
        magazineMap get(x._1) match {
          case None => {
            false
          }
          case Some(n) => {
            if (n >= x._2) {
              canMakeNote(xs, magazineMap)
            }
            else {
              false
            }
          }
        }
      }
      case _ => {
        true
      }
    }
  }

  def run = {
    val firstLine = readLine()
    val magazine = readLine.split(" ").map(_ trim)
    val ransom = readLine.split(" ").map(_ trim)

    val magazineMap = createMap(magazine.toList)
    val ransomMap = createMap(ransom.toList)

    val result = canMakeNote(ransomMap.toList, magazineMap)
    if (result) {
      println("Yes")
    }
    else {
      println("No")
    }

  }
  def main(args : Array[String]) : Unit = {
    run
  }
}
