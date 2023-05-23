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
          |g_i
          |""".stripMargin) must throwAn[IllegalArgumentException]
    }
    "throw an exception if any columns contain empty gaps between letters" in {
      Grid.fromString(
        """abc
          |_ef
          |ghi
          |""".stripMargin) must throwAn[IllegalArgumentException]
    }
    "not throw an exception if a column is entirely empty" in {
      Grid.fromString(
        """ab_
          |de_
          |gh_
          |""".stripMargin) must not(throwAn[Exception])
    }
    "not throw an exception if a column contains empty gaps above letters" in {
      Grid.fromString(
        """ab_
          |de_
          |ghi
          |""".stripMargin) must not(throwAn[Exception])
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
  }

  "retrieving the letter at a particular row and column" should {
    val grid: Grid = Grid.fromString(
      """a_c
        |def
        |ghi
        |""".stripMargin
    )
    "return a letter from the top-left corner" in {
      grid.letterAt(1, 1) must beSome('a')
    }
    "return a letter from the middle of the grid" in {
      grid.letterAt(2, 2) must beSome('e')
    }
    "return a letter from the bottom-right corner" in {
      grid.letterAt(3, 3) must beSome('i')
    }
    "return None for an empty cell" in {
      grid.letterAt(1, 2) must beNone
    }
    "throw an IndexOutOfBoundsException for a row and column outside the grid" in {
      grid.letterAt(100, 100) must throwAn[IndexOutOfBoundsException]
    }
  }

  "retrieving the coordinates of all neighbouring non-blank letters" should {
    val grid: Grid = Grid.fromString(
      """ab_
        |def
        |ghi
        |""".stripMargin
    )
    "return just rightward and downward coordinates for the top-left corner" in {
      grid.nonEmptyNeighbouringCoordinates(1, 1) must containTheSameElementsAs(Seq(
        (1, 2),
        (2, 1), (2, 2)
      ))
    }
    "return the full set of coordinates for a letter in the middle of the grid, ignoring blanks" in {
      grid.nonEmptyNeighbouringCoordinates(2, 2) must containTheSameElementsAs(Seq(
        (1, 1), (1, 2),
        (2, 1), (2, 3),
        (3, 1), (3, 2), (3, 3)
      ))
    }
    "return just the leftward and upward coordinates for the bottom-right corner" in {
      grid.nonEmptyNeighbouringCoordinates(3, 3) must containTheSameElementsAs(Seq(
        (2, 2), (2, 3),
        (3, 2)
      ))
    }
    "throw an IllegalArgumentException for a row and column outside the grid" in {
      grid.nonEmptyNeighbouringCoordinates(100, 100) must throwAn[IllegalArgumentException]
    }
  }

  "removing a word from a grid" should {
    "return a new grid with empty letters collapsed" in {
      val grid: Grid = Grid.fromString(
        """abcde
          |fghij
          |klmno
          |""".stripMargin)
      // Word is "chin":
      grid.withWordRemoved(Seq((1, 3), (2, 3), (2, 4), (3, 4))) mustEqual Grid.fromString(
        """ab__e
          |fg__j
          |klmdo
          |""".stripMargin)
    }
  }

  "checking whether a word of a given length could exist in the grid" should {
    "return true when a letter island exists big enough to contain the word" in {
      val grid: Grid = Grid.fromString(
        """a____
          |f_h_j
          |k_mno
          |""".stripMargin)
      (1 to 5).map { wordLength => grid.wordOfLengthCouldExist(wordLength) must beTrue }
    }
    "return false when no letter island exists big enough to contain the word" in {
      val grid: Grid = Grid.fromString(
        """a____
          |f_h_j
          |k_mno
          |""".stripMargin)
      grid.wordOfLengthCouldExist(6) must beFalse
    }
  }

  "getting the coordinates of non-empty letters in the grid" should {
    "return only the coordinates of non-empty letters" in {
      val grid: Grid = Grid.fromString(
        """_b_
          |_ef
          |_hi
          |""".stripMargin
      )
      grid.nonEmptyCoordinates() must containTheSameElementsAs(Seq((1, 2), (2, 2), (3, 2), (2, 3), (3, 3)))
    }
  }

}
