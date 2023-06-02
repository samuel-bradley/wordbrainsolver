package com.wordbrainsolver.application

class PuzzleSolver(dictionary: Seq[String]) {

  def findPossibleSolutions(puzzle: Puzzle): Seq[Seq[GridPathAndWord]] = {
    val possiblePaths: Seq[PossiblePathsAndWords] = findPossibleGridPathsAndWords(puzzle.grid, puzzle.wordLengths)
    possiblePaths.flatMap(_.getGridPathsAndWords)
  }

  private def findPossibleGridPathsAndWords(grid: Grid, wordLengths: Seq[Int]): Seq[PossiblePathsAndWords] = {
    // Find paths for this word length which would leave enough contiguous letters for the next word (if there is one)
    val pathsAndWords = findGridPathsLeavingNextWordFindable(grid, wordLengths.head, wordLengths.tail.headOption).map { path =>
      GridPathAndWord(path, grid.wordAt(path))
    }
    if (wordLengths.length > 1) {
      // For each path for this word, recursively find paths for the next word, stopping if there are none
      pathsAndWords.flatMap { pathAndWord =>
        val remainingPathsAndWords = findPossibleGridPathsAndWords(grid.withWordRemoved(pathAndWord.gridPath), wordLengths.tail)
        if (remainingPathsAndWords.nonEmpty) Some(PossiblePathsAndWords(pathAndWord, remainingPathsAndWords)) else None
      }
    } else {
      // This is the last word to find - return the possible paths and words
      pathsAndWords.map(pathAndWord => PossiblePathsAndWords(pathAndWord, Nil))
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
