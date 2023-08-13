package com.wordbrainsolver.application

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class PuzzleSolver(dictionaryPath: Path, additionalWords: Seq[String]) {

  def findPossibleSolutions(puzzle: Puzzle): Seq[Seq[GridPathAndWord]] = {
    val dictionary = buildDictionary(puzzle)
    val possiblePaths: Seq[PossiblePathsAndWords] = findPossibleGridPathsAndWords(puzzle.grid, puzzle.wordsToFind, dictionary)
    possiblePaths.flatMap(_.getGridPathsAndWords)
  }

  private def buildDictionary(puzzle: Puzzle) = {
    val words: Seq[String] = Files.readAllLines(dictionaryPath).asScala.toSeq ++ additionalWords
    val relevantWords: Seq[String] = words.filter { word: String =>
      puzzle.wordsToFind.map(_.length).contains(word.length) && word.forall { c: Char =>
        puzzle.grid.letters.flatten.contains(c)
      }
    }
    SearchTreeDictionary.of(relevantWords)
  }

  private def findPossibleGridPathsAndWords(grid: Grid, wordsToFind: Seq[WordToFind], dictionary: Dictionary): Seq[PossiblePathsAndWords] = {
    // Find paths for this word length which would leave enough contiguous letters for the next word (if there is one)
    val pathsAndWords = findGridPathsLeavingNextWordFindable(grid, wordsToFind.head, wordsToFind.tail.headOption.map(_.length), dictionary).map {
      path => GridPathAndWord(path, grid.wordAt(path))
    }
    if (wordsToFind.length > 1) {
      // For each path for this word, recursively find paths for the next word, stopping if there are none
      pathsAndWords.flatMap { pathAndWord =>
        val remainingPathsAndWords = findPossibleGridPathsAndWords(grid.withWordRemoved(pathAndWord.gridPath), wordsToFind.tail, dictionary)
        if (remainingPathsAndWords.nonEmpty) Some(PossiblePathsAndWords(pathAndWord, remainingPathsAndWords)) else None
      }
    } else {
      // This is the last word to find - return the possible paths and words
      pathsAndWords.map(pathAndWord => PossiblePathsAndWords(pathAndWord, Nil))
    }
  }

  def findGridPathsLeavingNextWordFindable(grid: Grid, wordToFind: WordToFind, nextWordLength: Option[Int], dictionary: Dictionary): Seq[GridPath] = {
    findGridPathsStartingAnywhere(grid, wordToFind, dictionary).filter { gridPath =>
      nextWordLength match {
        case Some(length) => grid.withWordRemoved(gridPath).wordOfLengthCouldExist(length)
        case None => true
      }
    }
  }

  def findGridPathsStartingAnywhere(grid: Grid, wordToFind: WordToFind, dictionary: Dictionary): Seq[GridPath] = {
    grid.nonEmptyCells().flatMap(c => findGridPathsStartingAtCell(grid, c, wordToFind, dictionary))
  }

  def findGridPathsStartingAtCell(grid: Grid, cell: Cell, wordToFind: WordToFind, dictionary: Dictionary): Seq[GridPath] = {
    findGridPathsStartingWithPartialPaths(grid, Seq(GridPath(Seq(cell))), wordToFind, dictionary)
  }

  private def findGridPathsStartingWithPartialPaths(grid: Grid, pathsSoFar: Seq[GridPath], wordToFind: WordToFind, dictionary: Dictionary): Seq[GridPath] = {
    pathsSoFar.flatMap { pathSoFar =>
      if (pathSoFar.cells.length == wordToFind.length) Seq(pathSoFar) else {
        // Extend each path so far by each of its possible next cells
        val newPathsSoFar = findPossibleNextCellsForGridPath(grid, pathSoFar, wordToFind, dictionary).map(pathSoFar.add)
        findGridPathsStartingWithPartialPaths(grid, newPathsSoFar, wordToFind, dictionary)
      }
    }
  }

  def findPossibleNextCellsForGridPath(grid: Grid, gridPathSoFar: GridPath, wordToFind: WordToFind, dictionary: Dictionary): Seq[Cell] = {
    grid.nonEmptyNeighbouringCells(gridPathSoFar.cells.last)
      .filterNot(gridPathSoFar.cells.contains) // exclude already-visited cells
      .filter { neighbour =>
        val putativeWordSoFar = grid.wordAt(gridPathSoFar) + grid.letterAt(neighbour).getOrElse("")
        dictionary.wordExists(putativeWordSoFar, wordToFind.revealedPart, wordToFind.length)
      }
  }

}
