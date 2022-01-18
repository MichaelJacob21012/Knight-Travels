// Part 2 about finding a single tour for a board using the Warnsdorf Rule
//=========================================================================

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

import scala.annotation.tailrec
type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
			if (x._1 < 0 || x._1 > dim - 1){
				false
			}
			else if (x._2 < 0 || x._2 > dim -1){
				false
			}
			else if (path.contains(x)){
				false
		  }
			else {
				true
			}
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
		val xPos = x._1
		val yPos = x._2
		val allMoves = List((xPos + 1, yPos + 2), (xPos + 2, yPos + 1), (xPos + 2, yPos - 1),(xPos + 1, yPos - 2),(xPos - 1, yPos - 2),(xPos - 2, yPos - 1),(xPos - 2, yPos + 1),(xPos - 1, yPos + 2))
		val moves = for (move <- allMoves if is_legal(dim,path,move)) yield (move)
		for (move <- moves if !(path.contains(move))) yield (move)
}

@tailrec
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
		if (xs == Nil){
			None
		}
		else{
		  val funct = f(xs.head)
			if (funct != None){
				funct
			}
			else {
				first(xs.drop(1), f)
			}
		}
}

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

def onward_moves(dim : Int, path: Path, pos: Pos) : Int = {
  legal_moves(dim, pos::path, pos).size
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = legal_moves(dim,path,x)
  moves.sortBy(onward_moves(dim,path,_))
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
   val tail = path.last
   if (path.size == dim*dim && legal_moves(dim, path.dropRight(1), path.head).contains(tail)){
      Option(path)
    }
   else{
      val moves = ordered_moves(dim, path, path.head)
      if (moves == None){
        None
      }
      else {
        first(moves, head  => first_closed_tour_heuristic(dim,head::path))
      }
    }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] ={
  if (path.size == dim*dim){
    Option(path)
  }
  else {
    val moves = ordered_moves(dim, path, path.head)
    if (moves == None){
        None
      }
      else {
        first(moves, head  => first_tour_heuristic(dim,head::path))
      }
  }
}

