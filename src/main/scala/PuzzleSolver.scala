package com.wordbrainsolver.application

class PuzzleSolver(dictionary: Seq[String]) {

  def findPossibleSolutions(puzzle: Puzzle): Seq[Seq[GridPath]] = {
    val possiblePaths = findPossibleGridPaths(puzzle.grid, puzzle.wordLengths)
    possiblePaths.flatMap(_.getGridPaths)
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
    grid.nonEmptyCoordinates().flatMap(c => findGridPathsStartingWithCoordinates(grid, c, wordLength))
  }

  def findGridPathsStartingWithCoordinates(grid: Grid, coordinates: Coordinates, wordLength: Int): Seq[GridPath] = {
    findGridPathsStartingWithPartialPaths(grid, Seq(GridPath(Seq(coordinates))), wordLength)
  }

  private def findGridPathsStartingWithPartialPaths(grid: Grid, pathsSoFar: Seq[GridPath], wordLength: Int): Seq[GridPath] = {
    pathsSoFar.flatMap { pathSoFar =>
      if (pathSoFar.coordinates.length == wordLength) Seq(pathSoFar) else {
        // Extend each path so far by each of its possible next coordinates
        val newPathsSoFar = findPossibleNextCoordinatesForGridPath(grid, pathSoFar, wordLength).map(pathSoFar.add)
        findGridPathsStartingWithPartialPaths(grid, newPathsSoFar, wordLength)
      }
    }
  }

  def findPossibleNextCoordinatesForGridPath(grid: Grid, gridPathSoFar: GridPath, wordLength: Int): Seq[Coordinates] = {
    grid.nonEmptyNeighbouringCoordinates(gridPathSoFar.coordinates.last)
      .filterNot(gridPathSoFar.coordinates.contains) // exclude already-visited coordinates
      .filter { neighbour =>
        val putativeWordSoFar = grid.wordAt(gridPathSoFar) + grid.letterAt(neighbour).getOrElse("")
        dictionary.exists(word => word.startsWith(putativeWordSoFar) && word.length == wordLength)
      }
  }

}
