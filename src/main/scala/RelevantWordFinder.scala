package com.wordbrainsolver.application

object RelevantWordFinder {
  /**
   * TODO: consider partially-completed words in the puzzle
   * TODO: instead of identifying letter pairs that aren't in the grid, consider letter sequences that couldn't be made
   * @return the subset of the given words which could plausibly be one of the hidden words in the puzzle
   */
  def findRelevantWords(puzzle: Puzzle, words: Seq[String]): Seq[String] = {
    val separatedPairs: Seq[Pair] = RelevantWordFinder.findSeparatedPairs(puzzle.grid)
    val gridLetters: Seq[Char] = puzzle.grid.letters.flatten
    words.filter { word: String =>
      puzzle.wordsToFind.map(_.length).contains(word.length) &&
        gridContainsEnoughLettersForWord(gridLetters, word) &&
        separatedPairs.forall { pair: Pair => !word.contains(pair.alphabetically) && !word.contains(pair.reverseAlphabetically) }
    }
  }

  private def gridContainsEnoughLettersForWord(gridLetters: Seq[Char], word: String): Boolean = {
    val lettersInWord = word.distinct.map(letter => (letter, word.count(_ == letter))).toMap
    lettersInWord.forall { case (letter, count) => gridLetters.count(_ == letter) >= count }
  }

  private def findSeparatedPairs(grid: Grid): Seq[Pair] = {
    // First get a sequence of every pair of cells and their letters in the grid
    val allCellPairs: Seq[(Cell, Cell)] = grid.nonEmptyCells().combinations(2).map(pair => (pair.head, pair.last)).toSeq
    val allPairs: Seq[Pair] = allCellPairs.map { case (cell1: Cell, cell2: Cell) =>
      Pair(CellAndLetter(cell1, grid.letterAt(cell1).get), CellAndLetter(cell2, grid.letterAt(cell2).get))
    }
    // Now find which pairs of letters exist only when more than one column apart
    val separatedPairs: Seq[Pair] = allPairs.groupBy(_.alphabetically).values.filter { similarPairs: Seq[Pair] =>
      similarPairs.forall(_.columnDifference > 1)
    }.flatten.toSeq
    separatedPairs.distinctBy(_.alphabetically)
  }
}

case class Pair(one: CellAndLetter, two: CellAndLetter) {
  val columnDifference: Int = Math.abs(one.cell.col - two.cell.col)
  def alphabetically: String = Seq(one.letter, two.letter).sorted.mkString
  def reverseAlphabetically: String = alphabetically.reverse.sorted.mkString
}

case class CellAndLetter(cell: Cell, letter: Char)

