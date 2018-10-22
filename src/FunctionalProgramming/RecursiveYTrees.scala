package FunctionalProgramming

/**
  * Created by creich on 1/10/18.
  */
object RecursiveYTrees {


  def findNewRoot(curRoot : (Int, Int, Int), leftRoot : Boolean) : (Int, Int, Int) = {
    if (leftRoot) {
      ((curRoot._1 - curRoot._3), (curRoot._2 - (curRoot._3 * 2)), (curRoot._3 / 2))

    }
    else {
      ((curRoot._1 + curRoot._3), (curRoot._2 - (curRoot._3 * 2)), (curRoot._3 / 2))

    }
  }



  def findRoots(curRoot : (Int, Int, Int), depth : Int) : Set[(Int, Int, Int)] = {
    if (depth == 1) Set(curRoot)
    else Set(curRoot) ++ findRoots(findNewRoot(curRoot, true), depth - 1) ++ findRoots(findNewRoot(curRoot, false), depth - 1)
  }


  def findTrunkPoints(root : (Int, Int, Int)) : IndexedSeq[(Int, Int)] = {
    for (j <- root._2 until (root._2 - root._3) by -1) yield (root._1, j)
  }

  def findLeftBranchPoints(root : (Int, Int, Int)) : IndexedSeq[(Int, Int)] = {
    for (j <- 1 until root._3 + 1) yield ((root._1 - j), (root._2 - root._3 - j + 1))
  }
  def findRightBranchPoints(root : (Int, Int, Int)) : IndexedSeq[(Int, Int)] = {
    for (j <- 1 until root._3 + 1) yield ((root._1 + j), (root._2 - root._3 - j  + 1))
  }

  def findTreePointsFromRoot(root : (Int, Int, Int)) : Set[(Int, Int)] = {
    (findTrunkPoints(root) ++ findLeftBranchPoints(root) ++ findRightBranchPoints(root)).toSet
  }

  def convertRootsToTrees(roots : Set[(Int, Int, Int)]) : Set[(Int, Int)] = {
    roots.foldLeft(Set[(Int, Int)]())((b, t) => b ++ findTreePointsFromRoot(t))
  }


  def generateIndices() : IndexedSeq[(Int, Int)] = {
    for (i <- 1 until 64;
         j <- 1 until 101) yield (j, i)
  }

  def drawTrees(n : Int) : Unit = {

    for (v <- generateIndices()) {

      //println(v)

      if (convertRootsToTrees(findRoots((50, 63, 16), n)).contains(v)) print("1")
      else print("_")
      if (v._1 == 100) print("\n")

    }

  }




  def main(args : Array[String]) : Unit = {
    drawTrees(readInt())
  }

}
