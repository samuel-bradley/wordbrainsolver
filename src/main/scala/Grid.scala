package com.wordbrainsolver.application

case class Grid(letters: Seq[Option[Char]], width: Int) {
  require(letters.length % width == 0, s"Number of letters (${letters.length}) must be multiple of width ($width)")

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

  private def coordinatesAreInGrid(row: Int, col: Int): Boolean = {
    row >= 1 && row <= width &&
      col >= 1 && col <= height
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
