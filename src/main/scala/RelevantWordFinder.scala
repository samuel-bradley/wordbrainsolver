package com.wordbrainsolver.application

object RelevantWordFinder {
  def findRelevantWords(puzzle: Puzzle, words: Seq[String]): Seq[String] = {
    val separatedPairs: Seq[Pair] = RelevantWordFinder.findSeparatedPairs(puzzle.grid)
    words.filter { word: String =>
      puzzle.wordsToFind.map(_.length).contains(word.length) &&
        word.forall { c: Char => puzzle.grid.letters.flatten.contains(c) } &&
        separatedPairs.forall { pair: Pair => !word.contains(pair.alphabetically) && !word.contains(pair.reverseAlphabetically) }
    }
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

