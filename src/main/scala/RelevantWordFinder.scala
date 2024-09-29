package com.wordbrainsolver.application

object RelevantWordFinder {
  /**
   * TODO: consider partially-completed words in the puzzle
   * @return the subset of the given words which could plausibly be one of the hidden words in the puzzle
   */
  def findRelevantWords(puzzle: Puzzle, words: Seq[String]): Seq[String] = {
    words.filter { word: String =>
      puzzle.wordsToFind.map(_.length).contains(word.length) &&
        wordHasLettersInConsecutiveGridColumns(puzzle.grid, word)
    }
  }

  private def wordHasLettersInConsecutiveGridColumns(grid: Grid, word: String): Boolean = {
    /* We can potentially make a word in the grid with collapses if it's possible to make the word by traversing only
        consecutive columns */
    grid.locationsOf(Some(word.head)).exists { cell: Cell =>
      lettersAreInConsecutiveGridColumnsFromCell(grid, word.tail, cell, Seq(cell))
    }
  }

  private def lettersAreInConsecutiveGridColumnsFromCell(grid: Grid, letters: String, cell: Cell, excludeCells: Seq[Cell]): Boolean = {
    if (letters.isEmpty) true else {
      cellsWithLetterWithinOneColumnOfCell(grid, letters.head, cell, excludeCells) match {
        case cells if cells.nonEmpty =>
          // If we found the first letter, continue and see if we can find subsequent letters
          cells.exists(c => lettersAreInConsecutiveGridColumnsFromCell(grid, letters.tail, c, excludeCells :+ c))
        case _ =>
          // We didn't even find the first letter
          false
      }
    }
  }

  private def cellsWithLetterWithinOneColumnOfCell(grid: Grid, letter: Char, cell: Cell, excludeCells: Seq[Cell]): Seq[Cell] = {
    grid.nonEmptyCellsWithinOneColumn(cell).filterNot(excludeCells.contains).filter(grid.letterAt(_).contains(letter))
  }
}
