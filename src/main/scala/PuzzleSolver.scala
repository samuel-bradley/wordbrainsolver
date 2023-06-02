package com.wordbrainsolver.application

class PuzzleSolver(dictionary: Seq[String]) {

  def findPossibleSolutions(puzzle: Puzzle): Seq[Seq[GridPathAndWord]] = {
    val possiblePaths: Seq[PossiblePaths] = findPossibleGridPaths(puzzle.grid, puzzle.wordLengths)
    possiblePaths.flatMap(_.getGridPaths).map { gridPaths: Seq[GridPath] =>
      gridPaths.zip(gridPathsToWords(puzzle.grid, gridPaths)).map { case (gridPath, word) =>
        GridPathAndWord(gridPath, word)
      }
    }
  }

  private def gridPathsToWords(grid: Grid, gridPaths: Seq[GridPath]): Seq[String] = {
    gridPaths.headOption.map { firstPath =>
      Seq(grid.wordAt(firstPath)) ++ gridPathsToWords(grid.withWordRemoved(firstPath), gridPaths.tail)
    }.getOrElse(Nil)
  }

  private def findPossibleGridPaths(grid: Grid, wordLengths: Seq[Int]): Seq[PossiblePaths] = {
    // Find paths for this word length which would leave enough contiguous letters for the next word (if there is one)
    val gridPaths: Seq[GridPath] = findGridPathsLeavingNextWordFindable(grid, wordLengths.head, wordLengths.tail.headOption)
    if (wordLengths.length > 1) {
      // For each path for this word, recursively find paths for the next word, stopping if there are none
      gridPaths.flatMap { gridPath =>
        val remainingGridPaths: Seq[PossiblePaths] = findPossibleGridPaths(grid.withWordRemoved(gridPath), wordLengths.tail)
        if (remainingGridPaths.nonEmpty) Some(PossiblePaths(gridPath, remainingGridPaths)) else None
      }
    } else {
      // This is the last word - return its paths
      gridPaths.map(gridPath => PossiblePaths(gridPath, Nil))
    }
  }

  def findGridPathsLeavingNextWordFindable(grid: Grid, wordLength: Int, nextWordLength: Option[Int]): Seq[GridPath] = {
    findGridPathsStartingAnywhere(grid, wordLength).filter { gridPath =>
      nextWordLength match {
        case Some(length) => grid.withWordRemoved(gridPath).wordOfLengthCouldExist(length)
        case None => true
      }
    }
  }

  def findGridPathsStartingAnywhere(grid: Grid, wordLength: Int): Seq[GridPath] = {
    grid.nonEmptyCells().flatMap(c => findGridPathsStartingAtCell(grid, c, wordLength))
  }

  def findGridPathsStartingAtCell(grid: Grid, cell: Cell, wordLength: Int): Seq[GridPath] = {
    findGridPathsStartingWithPartialPaths(grid, Seq(GridPath(Seq(cell))), wordLength)
  }

  private def findGridPathsStartingWithPartialPaths(grid: Grid, pathsSoFar: Seq[GridPath], wordLength: Int): Seq[GridPath] = {
    pathsSoFar.flatMap { pathSoFar =>
      if (pathSoFar.cells.length == wordLength) Seq(pathSoFar) else {
        // Extend each path so far by each of its possible next cells
        val newPathsSoFar = findPossibleNextCellsForGridPath(grid, pathSoFar, wordLength).map(pathSoFar.add)
        findGridPathsStartingWithPartialPaths(grid, newPathsSoFar, wordLength)
      }
    }
  }

  def findPossibleNextCellsForGridPath(grid: Grid, gridPathSoFar: GridPath, wordLength: Int): Seq[Cell] = {
    grid.nonEmptyNeighbouringCells(gridPathSoFar.cells.last)
      .filterNot(gridPathSoFar.cells.contains) // exclude already-visited cells
      .filter { neighbour =>
        val putativeWordSoFar = grid.wordAt(gridPathSoFar) + grid.letterAt(neighbour).getOrElse("")
        dictionary.exists(word => word.startsWith(putativeWordSoFar) && word.length == wordLength)
      }
  }

}
