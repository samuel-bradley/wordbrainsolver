package com.wordbrainsolver.application

import org.specs2.mutable.Specification

class RelevantWordFinderSpec extends Specification {

  "Finding relevant words for a puzzle" should {
    val grid = Grid.fromString(
      """agbs
        |toak
        |sdir
        |atoe
        |phts""".stripMargin)
    val puzzle = Puzzle(grid, unrevealedWords(Seq(5, 6, 5, 4)))

    "return only words matching the lengths in the puzzle, even if they can be formed in the grid" in {
      val words = Seq(
        "sap", // length 3 not in puzzle
        "pasta", "hotdog", "steak", "ribs", // actual solution
        "toerids" // length 7 not in puzzle
      )
      RelevantWordFinder.findRelevantWords(puzzle, words) must containTheSameElementsAs(Seq("pasta", "hotdog", "steak", "ribs"))
    }
    "return only words for which there are enough letters in the grid, even if they are the right length" in {
      val words = Seq(
        "quine", // length 5 but 'q' not in grid
        "pasta", "hotdog", "steak", "ribs", // actual solution
        "vaxes", // length 5 but 'z' and 'x' not in grid
        "babes" // length 5 but only one 'b' in the grid
      )
      RelevantWordFinder.findRelevantWords(puzzle, words) must containTheSameElementsAs(Seq("pasta", "hotdog", "steak", "ribs"))
    }
    "return only words not containing consecutive pairs of letters which can never be consecutive in the grid, even if they are the right length" in {
      val words = Seq(
        "prat", // length 4 but 'p' and 'r' can never be consecutive
        "pasta", "hotdog", "steak", "ribs", // actual solution
        "drat" // length 4 but 'd' and 'r' can never be consecutive
      )
      RelevantWordFinder.findRelevantWords(puzzle, words) must containTheSameElementsAs(Seq("pasta", "hotdog", "steak", "ribs"))
    }
  }

  private def unrevealedWords(lengths: Seq[Int]): Seq[WordToFind] = lengths.map(WordToFind(_, ""))

}
