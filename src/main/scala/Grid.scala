package com.wordbrainsolver.application

case class Grid(letters: Seq[Option[Char]], width: Int) {
  require(letters.length % width == 0, s"Number of letters (${letters.length}) must be multiple of width ($width)")
  require(!lettersContainEmptyGaps(), s"Grid may not contain empty gaps in columns - grid:\n" + toString)

  private val height: Int = letters.length / width

  def letterAt(coordinates: Coordinates): Option[Char] = {
    letters((coordinates.row - 1) * width + coordinates.col - 1)
  }

  def wordAt(gridPath: GridPath): String = {
    gridPath.coordinates.flatMap(letterAt).mkString
  }

  def nonEmptyNeighbouringCoordinates(coordinates: Coordinates): Seq[Coordinates] = {
    if (coordinates.row > height || coordinates.col > width)
      throw new IllegalArgumentException(s"Cannot get neighbours for $coordinates outside grid of ($width, $height)")

    (for (rowOffset <- -1 to 1; colOffset <- -1 to 1 if !(rowOffset == 0 && colOffset == 0)) yield (rowOffset, colOffset))
      .map { case (rowOffset, colOffset) => Coordinates(coordinates.row + rowOffset, coordinates.col + colOffset) }
      .filter(coordinates => coordinatesAreInGrid(coordinates) && letterAt(coordinates).isDefined)
  }

  def withWordRemoved(gridPath: GridPath): Grid = {
    def indexToCoordinates(index: Int): Coordinates = Coordinates(index / width + 1, index % width + 1)
    val lettersWithNewEmpties: Seq[Option[Char]] = letters.zipWithIndex.map { case (letter: Option[Char], index: Int) =>
        if (gridPath.coordinates.contains(indexToCoordinates(index))) None
        else letter
    }
    Grid(collapseLetters(lettersWithNewEmpties), width)
  }

  def wordOfLengthCouldExist(wordLength: Int): Boolean = {
    findLetterIslandSizes().exists(_ >= wordLength)
  }

  def nonEmptyCoordinates(): Seq[Coordinates] = {
    (for (row <- 1 to height; col <- 1 to width) yield Coordinates(row, col)).flatMap { coordinates =>
      if (letterAt(coordinates).isDefined) Some(coordinates) else None
    }
  }

  private def findLetterIslandSizes(): Seq[Int] = {
    val nonEmptyCols = (1 to width).filter(col => getColumn(letters, col).exists(_.isDefined))
    // Find the number of each non-empty column which is the first in a run of non-empty columns
    val nonEmptyColGroupStartCols: Seq[Int] = nonEmptyCols.filter(col => nonEmptyCols.contains(col + 1) || col + 1 > width)
    // Starting from each first non-empty column, build the group of the following non-empty columns
    val nonEmptyColGroups: Seq[Seq[Int]] = nonEmptyColGroupStartCols.map { startCol =>
      (startCol to width).takeWhile(col => nonEmptyCols.contains(col))
    }
    nonEmptyColGroups.map { nonEmptyColGroup: Seq[Int] =>
      // For each group of non-empty columns, sum the number of letters in each column
      nonEmptyColGroup.fold(0) { case (sum, col) =>
        sum + getColumn(letters, col).count(_.isDefined)
      }
    }
  }

  private def collapseLetters(letters: Seq[Option[Char]]): Seq[Option[Char]] = {
    val collapsedColumns: Seq[Seq[Option[Char]]] = (1 to width).map { col =>
      val (emptyLetters, nonEmptyLetters) = getColumn(letters, col).partition(_.isEmpty)
      emptyLetters ++ nonEmptyLetters
    }
    (1 to height).flatMap(row => collapsedColumns.map(column => column(row - 1)))
  }

  private def coordinatesAreInGrid(coordinates: Coordinates): Boolean = {
    coordinates.row >= 1 && coordinates.row <= height &&
      coordinates.col >= 1 && coordinates.col <= width
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
