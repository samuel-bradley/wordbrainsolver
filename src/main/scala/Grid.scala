package com.wordbrainsolver.application

case class Grid(letters: Seq[Option[Char]], width: Int) {
  require(letters.length % width == 0, s"Number of letters (${letters.length}) must be multiple of width ($width)")
  require(!lettersContainEmptyGaps(), s"Grid may not contain empty gaps in columns - grid:\n" + toString)

  private val height: Int = letters.length / width

  def letterAt(row: Int, col: Int): Option[Char] = {
    letters((row - 1) * width + col - 1)
  }

  def nonEmptyNeighbouringCoordinates(row: Int, col: Int): Seq[(Int, Int)] = {
    if (row > width || col > height)
      throw new IllegalArgumentException(s"Cannot get neighbours for ($row, $col) outside grid of ($width, $height")
    val possibleNeighbours: Seq[(Int, Int)] = Seq(
      (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
      (row, col - 1), (row, col + 1),
      (row + 1, col - 1), (row + 1, col), (row + 1, col + 1),
    )
    possibleNeighbours.filter { case (possibleRow, possibleCol) =>
      coordinatesAreInGrid(possibleRow, possibleCol) && letterAt(possibleRow, possibleCol).isDefined
    }
  }

  def withWordRemoved(letterCoordinates: Seq[(Int, Int)]): Grid = {
    def indexToCoordinates(index: Int): (Int, Int) = (index / width + 1, index % width + 1)
    val lettersWithNewEmpties: Seq[Option[Char]] = letters.zipWithIndex.map { case (letter: Option[Char], index: Int) =>
        if (letterCoordinates.contains(indexToCoordinates(index))) None
        else letter
    }
    Grid(collapseLetters(lettersWithNewEmpties), width)
  }

  private def collapseLetters(letters: Seq[Option[Char]]): Seq[Option[Char]] = {
    val collapsedColumns: Seq[Seq[Option[Char]]] = (1 to width).map { col =>
      val (emptyLetters, nonEmptyLetters) = getColumn(letters, col).partition(_.isEmpty)
      emptyLetters ++ nonEmptyLetters
    }
    (1 to height).flatMap(row => collapsedColumns.map(column => column(row - 1)))
  }

  private def coordinatesAreInGrid(row: Int, col: Int): Boolean = {
    row >= 1 && row <= width &&
      col >= 1 && col <= height
  }

  private def lettersContainEmptyGaps(): Boolean = {
    (1 to width).exists(col => columnContainsEmptyGap(getColumn(letters, col)))
  }

  private def getColumn(letters: Seq[Option[Char]], col: Int): Seq[Option[Char]] = {
    val rows = letters.grouped(width)
    rows.map(row => row(col - 1)).toSeq
  }
  private def columnContainsEmptyGap(column: Seq[Option[Char]]): Boolean = {
    column.zipWithIndex.exists { case (letter, index) =>
      val letterAbove: Option[Char] = if (index == 0) None else column(index - 1)
      letter.isEmpty && letterAbove.isDefined
    }
  }

  override def toString: String = {
    letters.grouped(width).map((rowLetters: Seq[Option[Char]]) => {
      rowLetters.map(_.getOrElse(Grid.emptyLetterDisplayChar)).mkString
    }).mkString("\n")
  }
}

case object Grid {
  private val emptyLetterDisplayChar: Char = '_'

  def fromString(string: String): Grid = {
    val width = string.indexOf('\n')
    val letters: Seq[Option[Char]] = string.toCharArray.filterNot(_ == '\n').map {
      case Grid.emptyLetterDisplayChar => None
      case char: Char => Some(char)
    }
    Grid(letters, width)
  }
}
