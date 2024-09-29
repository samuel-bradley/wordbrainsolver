package com.wordbrainsolver.application

import org.specs2.mutable.Specification

class GridSpec extends Specification {

  "instantiating a Grid" should {
    "throw an exception if the number of letters is not a multiple of the width" in {
      Grid(Seq(Some('a'), Some('b'), Some('c')), 2) must throwAn[IllegalArgumentException]
      Grid(Seq(Some('a'), Some('b'), None), 2) must throwAn[IllegalArgumentException]
    }
    "not throw an exception if the number of letters is a multiple of the width" in {
      Grid(Seq(Some('a'), Some('b'), Some('c'), Some('d')), 2) must not(throwAn[Exception])
      Grid(Seq(None, Some('b'), Some('c'), Some('d')), 2) must not(throwAn[Exception])
    }
    "throw an exception if any columns contain empty gaps below letters" in {
      Grid.fromString(
        """abc
          |def
          |g_i""".stripMargin) must throwAn[IllegalArgumentException]
    }
    "throw an exception if any columns contain empty gaps between letters" in {
      Grid.fromString(
        """abc
          |_ef
          |ghi""".stripMargin) must throwAn[IllegalArgumentException]
    }
    "not throw an exception if a column is entirely empty" in {
      Grid.fromString(
        """ab_
          |de_
          |gh_""".stripMargin) must not(throwAn[Exception])
    }
    "not throw an exception if a column contains empty gaps above letters" in {
      Grid.fromString(
        """ab_
          |de_
          |ghi""".stripMargin) must not(throwAn[Exception])
    }
  }

  "converting a Grid to a String" should {
    "return the letters separated into rows by newlines, representing empty cells as underscores" in {
      Grid(Seq(Some('a'), None, Some('c'), Some('d')), 2).toString mustEqual
        """a_
          |cd""".stripMargin
    }
  }

  "converting a String to a Grid" should {
    "return the correct Grid, parsing underscores as empty cells" in {
      Grid.fromString(
        """a_
          |cd""".stripMargin) mustEqual Grid(Seq(Some('a'), None, Some('c'), Some('d')), 2)
    }
    "work for a grid with no empty letters" in {
      Grid.fromString(
        """ab
          |cd""".stripMargin) mustEqual Grid(Seq(Some('a'), Some('b'), Some('c'), Some('d')), 2)
    }
  }

  "retrieving the letter at a particular cell" should {
    val grid: Grid = Grid.fromString(
      """a_c
        |def
        |ghi""".stripMargin
    )
    "return a letter from the top-left corner" in {
      grid.letterAt(Cell(1, 1)) must beSome('a')
    }
    "return a letter from the middle of the grid" in {
      grid.letterAt(Cell(2, 2)) must beSome('e')
    }
    "return a letter from the bottom-right corner" in {
      grid.letterAt(Cell(3, 3)) must beSome('i')
    }
    "return None for an empty cell" in {
      grid.letterAt(Cell(1, 2)) must beNone
    }
    "throw an IndexOutOfBoundsException for a row and column outside the grid" in {
      grid.letterAt(Cell(100, 100)) must throwAn[IndexOutOfBoundsException]
    }
  }

  "retrieving the cells of all neighbouring non-blank letters" should {
    val grid: Grid = Grid.fromString(
      """ab_
        |def
        |ghi""".stripMargin
    )
    "return just rightward and downward cells for the top-left corner" in {
      grid.nonEmptyNeighbouringCells(Cell(1, 1)) must containTheSameElementsAs(Seq(
        Cell(1, 2),
        Cell(2, 1), Cell(2, 2)
      ))
    }
    "return the full set of cells for a letter in the middle of the grid, ignoring blanks" in {
      grid.nonEmptyNeighbouringCells(Cell(2, 2)) must containTheSameElementsAs(Seq(
        Cell(1, 1), Cell(1, 2),
        Cell(2, 1), Cell(2, 3),
        Cell(3, 1), Cell(3, 2), Cell(3, 3)
      ))
    }
    "return just the leftward and upward cells for the bottom-right corner" in {
      grid.nonEmptyNeighbouringCells(Cell(3, 3)) must containTheSameElementsAs(Seq(
        Cell(2, 2), Cell(2, 3),
        Cell(3, 2)
      ))
    }
    "throw an IllegalArgumentException for a row and column outside the grid" in {
      grid.nonEmptyNeighbouringCells(Cell(100, 100)) must throwAn[IllegalArgumentException]
    }
  }

  "retrieving the cells of all non-blank letters within one column" should {
    val grid: Grid = Grid.fromString(
      """ab_x
        |defy
        |ghiz""".stripMargin
    )
    "return the correct cells" in {
      grid.nonEmptyCellsWithinOneColumn(Cell(2, 2)) must containTheSameElementsAs(Seq(
        Cell(1, 1), Cell(1, 2),
        Cell(2, 1), Cell(2, 3),
        Cell(3, 1), Cell(3, 2), Cell(3, 3),
      ))
    }
  }

  "removing a word from a grid" should {
    "return a new grid with empty letters collapsed" in {
      val grid: Grid = Grid.fromString(
        """abcde
          |fghij
          |klmno""".stripMargin)
      // Word is "chin":
      grid.withWordRemoved(GridPath(Seq(Cell(1, 3), Cell(2, 3), Cell(2, 4), Cell(3, 4)))) mustEqual Grid.fromString(
        """ab__e
          |fg__j
          |klmdo""".stripMargin)
    }
  }

  "checking whether a word of a given length could exist in the grid" should {
    "return true when a letter island exists big enough to contain the word" in {
      val grid: Grid = Grid.fromString(
        """a____
          |f_h_j
          |k_mno""".stripMargin)
      (1 to 5).map { wordLength => grid.wordOfLengthCouldExist(wordLength) must beTrue }
    }
    "return false when no letter island exists big enough to contain the word" in {
      val grid: Grid = Grid.fromString(
        """a____
          |f_h_j
          |k_mno""".stripMargin)
      grid.wordOfLengthCouldExist(6) must beFalse
    }
  }

  "getting the cells of non-empty letters in the grid" should {
    "return only the cells of non-empty letters" in {
      val grid: Grid = Grid.fromString(
        """_b_
          |_ef
          |_hi""".stripMargin)
      grid.nonEmptyCells() must containTheSameElementsAs(Seq(Cell(1, 2), Cell(2, 2), Cell(3, 2), Cell(2, 3), Cell(3, 3)))
    }
    "return the cells of non-empty letters in a non-square grid" in {
      val grid: Grid = Grid.fromString(
        """___
          |t__
          |hy_
          |ac_""".stripMargin)
      grid.nonEmptyCells() must containTheSameElementsAs(Seq(
        Cell(2, 1),
        Cell(3, 1), Cell(3, 2),
        Cell(4, 1), Cell(4, 2)
      ))
    }
  }

}
