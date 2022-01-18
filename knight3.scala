// Finding a single tour on a "mega" board
//=========================================


// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
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
def onward_moves(dim : Int, path: Path, pos: Pos) : Int = {
  legal_moves(dim, pos::path, pos).size
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = legal_moves(dim,path,x)
  moves.sortBy(onward_moves(dim,path,_))
}


type Move = (Pos,Int) 


def first_tour_tail_rec(dim : Int, futureMoves : List[Move], acc : List[Move]) : Option[Path] = {
  val path = for (move <- acc) yield (move._1)
  if (futureMoves == Nil){
    None
  }
  else {
    val nextPos = futureMoves.head._1
    val targets = ordered_moves(dim,nextPos::path,nextPos)
    if (futureMoves.head._2 == acc.head._2){
      first_tour_tail_rec(dim, futureMoves, acc.tail)
    }
    else if (targets == Nil){
      if (acc.size == dim*dim - 1){
        Some(nextPos::path)
      }
      else {
        first_tour_tail_rec(dim, futureMoves.tail,acc)
      }
    }
    else {
      val moves = for( target <- targets) yield ((target,futureMoves.head._2 + 1))
      first_tour_tail_rec(dim, moves:::futureMoves.tail,futureMoves.head::acc)
    }
  }
}



//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
  val availableMoves = for (pos <- ordered_moves(dim,path,path.head)) yield (pos,1)
  val alreadyVisited = for (pos <- path) yield (pos,0)
  first_tour_tail_rec(dim, availableMoves, alreadyVisited)
}

