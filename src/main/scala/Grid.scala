package com.wordbrainsolver.application

case class Grid(letters: Seq[Option[Char]], width: Int) {
  require(letters.length % width == 0, s"Number of letters (${letters.length}) must be multiple of width ($width)")
  require(!lettersContainEmptyGaps(), s"Grid may not contain empty gaps in columns - grid:\n" + toString)

  private val height: Int = letters.length / width

  def letterAt(cell: Cell): Option[Char] = {
    letters((cell.row - 1) * width + cell.col - 1)
  }

  def wordAt(gridPath: GridPath): String = {
    gridPath.cells.flatMap(letterAt).mkString
  }

  def nonEmptyNeighbouringCells(cell: Cell): Seq[Cell] = {
    if (cell.row > height || cell.col > width)
      throw new IllegalArgumentException(s"Cannot get neighbours for $cell outside grid of ($width, $height)")

    (for (rowOffset <- -1 to 1; colOffset <- -1 to 1 if !(rowOffset == 0 && colOffset == 0)) yield (rowOffset, colOffset))
      .map { case (rowOffset, colOffset) => Cell(cell.row + rowOffset, cell.col + colOffset) }
      .filter(cell => cellIsInGrid(cell) && letterAt(cell).isDefined)
  }

  def nonEmptyCellsWithinOneColumn(originCell: Cell): Seq[Cell] = {
    (for (row <- 1 to height; col <- originCell.col - 1 to originCell.col + 1) yield (row, col))
      .map { case (row, col) => Cell(row, col) }
      .filter(cell => cellIsInGrid(cell) && letterAt(cell).isDefined && cell != originCell)
  }

  def withWordRemoved(gridPath: GridPath): Grid = {
    val lettersWithNewEmpties: Seq[Option[Char]] = letters.zipWithIndex.map { case (letter: Option[Char], index: Int) =>
        if (gridPath.cells.contains(indexToCell(index))) None
        else letter
    }
    Grid(collapseLetters(lettersWithNewEmpties), width)
  }

  def wordOfLengthCouldExist(wordLength: Int): Boolean = {
    findLetterIslandSizes().exists(_ >= wordLength)
  }

  def nonEmptyCells(): Seq[Cell] = {
    (for (row <- 1 to height; col <- 1 to width) yield Cell(row, col)).flatMap { cell =>
      if (letterAt(cell).isDefined) Some(cell) else None
    }
  }

  def locationsOf(letter: Option[Char]): Seq[Cell] = {
    letters.zipWithIndex.filter(_._1 == letter).map {
      case (_, index) => indexToCell(index)
    }
  }

  def findLetterIslandSizes(): Seq[Int] = {
    val populatedCols = (1 to width).filter(col => getColumn(letters, col).exists(_.isDefined))
    // Find the number of each non-empty column which is the first in a run of non-empty columns
    val populatedColGroupStartCols: Seq[Int] = populatedCols.filter(col => populatedCols.contains(col) && !populatedCols.contains(col - 1))
    // Find the number of each non-empty column which is the last in a run of non-empty columns
    val populatedColGroupEndCols: Seq[Int] = populatedCols.filter(col => populatedCols.contains(col) && !populatedCols.contains(col + 1))
    // Starting from each first non-empty column, build the group of the following non-empty columns
    val populatedColGroups: Seq[Seq[Int]] = populatedColGroupStartCols.zip(populatedColGroupEndCols).map { case (startCol, endCol) =>
      (startCol to endCol)
    }
    populatedColGroups.map { nonEmptyColGroup: Seq[Int] =>
      // For each group of non-empty columns, sum the number of letters in each column
      nonEmptyColGroup.fold(0) { case (sum, col) =>
        sum + getColumn(letters, col).count(_.isDefined)
      }
    }
  }

  private def indexToCell(index: Int): Cell = Cell(index / width + 1, index % width + 1)

  private def collapseLetters(letters: Seq[Option[Char]]): Seq[Option[Char]] = {
    val collapsedColumns: Seq[Seq[Option[Char]]] = (1 to width).map { col =>
      val (emptyLetters, nonEmptyLetters) = getColumn(letters, col).partition(_.isEmpty)
      emptyLetters ++ nonEmptyLetters
    }
    (1 to height).flatMap(row => collapsedColumns.map(column => column(row - 1)))
  }

  private def cellIsInGrid(cell: Cell): Boolean = {
    cell.row >= 1 && cell.row <= height && cell.col >= 1 && cell.col <= width
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
