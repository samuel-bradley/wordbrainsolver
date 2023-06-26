package com.wordbrainsolver.application

class PuzzleSolver(dictionary: Dictionary) {

  def findPossibleSolutions(puzzle: Puzzle): Seq[Seq[GridPathAndWord]] = {
    val possiblePaths: Seq[PossiblePathsAndWords] = findPossibleGridPathsAndWords(puzzle.grid, puzzle.wordsToFind)
    possiblePaths.flatMap(_.getGridPathsAndWords)
  }

  private def findPossibleGridPathsAndWords(grid: Grid, wordsToFind: Seq[WordToFind]): Seq[PossiblePathsAndWords] = {
    // Find paths for this word length which would leave enough contiguous letters for the next word (if there is one)
    val pathsAndWords = findGridPathsLeavingNextWordFindable(grid, wordsToFind.head, wordsToFind.tail.headOption.map(_.length)).map {
      path => GridPathAndWord(path, grid.wordAt(path))
    }
    if (wordsToFind.length > 1) {
      // For each path for this word, recursively find paths for the next word, stopping if there are none
      pathsAndWords.flatMap { pathAndWord =>
        val remainingPathsAndWords = findPossibleGridPathsAndWords(grid.withWordRemoved(pathAndWord.gridPath), wordsToFind.tail)
        if (remainingPathsAndWords.nonEmpty) Some(PossiblePathsAndWords(pathAndWord, remainingPathsAndWords)) else None
      }
    } else {
      // This is the last word to find - return the possible paths and words
      pathsAndWords.map(pathAndWord => PossiblePathsAndWords(pathAndWord, Nil))
    }
  }

  def findGridPathsLeavingNextWordFindable(grid: Grid, wordToFind: WordToFind, nextWordLength: Option[Int]): Seq[GridPath] = {
    findGridPathsStartingAnywhere(grid, wordToFind).filter { gridPath =>
      nextWordLength match {
        case Some(length) => grid.withWordRemoved(gridPath).wordOfLengthCouldExist(length)
        case None => true
      }
    }
  }

  def findGridPathsStartingAnywhere(grid: Grid, wordToFind: WordToFind): Seq[GridPath] = {
    grid.nonEmptyCells().flatMap(c => findGridPathsStartingAtCell(grid, c, wordToFind))
  }

  def findGridPathsStartingAtCell(grid: Grid, cell: Cell, wordToFind: WordToFind): Seq[GridPath] = {
    findGridPathsStartingWithPartialPaths(grid, Seq(GridPath(Seq(cell))), wordToFind)
  }

  private def findGridPathsStartingWithPartialPaths(grid: Grid, pathsSoFar: Seq[GridPath], wordToFind: WordToFind): Seq[GridPath] = {
    pathsSoFar.flatMap { pathSoFar =>
      if (pathSoFar.cells.length == wordToFind.length) Seq(pathSoFar) else {
        // Extend each path so far by each of its possible next cells
        val newPathsSoFar = findPossibleNextCellsForGridPath(grid, pathSoFar, wordToFind).map(pathSoFar.add)
        findGridPathsStartingWithPartialPaths(grid, newPathsSoFar, wordToFind)
      }
    }
  }

  def findPossibleNextCellsForGridPath(grid: Grid, gridPathSoFar: GridPath, wordToFind: WordToFind): Seq[Cell] = {
    grid.nonEmptyNeighbouringCells(gridPathSoFar.cells.last)
      .filterNot(gridPathSoFar.cells.contains) // exclude already-visited cells
      .filter { neighbour =>
        val putativeWordSoFar = grid.wordAt(gridPathSoFar) + grid.letterAt(neighbour).getOrElse("")
        dictionary.wordExists(putativeWordSoFar, wordToFind.revealedPart, wordToFind.length)
      }
  }

}
