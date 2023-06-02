package com.wordbrainsolver.application

import org.specs2.mutable.Specification

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class PuzzleSolverSpec extends Specification{

  // https://www-personal.umich.edu/~jlawler/wordlist
  // TODO it was necessary to add "ribs" to make a test pass, so this file is evidently not sufficient
  private val dictionary = (Files.readAllLines(Path.of("C:\\Users\\Samuel\\Documents\\wordbrainsolver\\src\\main\\scala\\dictionary.txt"))
    .asScala.toSeq :+ "ribs").sorted
  private val solver = new PuzzleSolver(dictionary)

  "Solving a puzzle" should {
    "find the correct path and word for the smallest possible" in {
      // This test asserts that the grid path matches the word - subsequent tests will just check the words for brevity
      val grid = Grid.fromString(
        """fi
          |sh
          |""".stripMargin)
      val puzzle = Puzzle(grid, Seq(4))
      solver.findPossibleSolutions(puzzle) mustEqual Seq(Seq(
        GridPathAndWord(GridPath(Seq(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2))), "fish")
      ))
    }
    "find the possible solutions for a small puzzle" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val puzzle = Puzzle(grid, Seq(4, 5))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(
        Seq("tack", "thebe"),
        Seq("tack", "thebe"),
        Seq("back", "teeth"), // actual solution
        Seq("back", "teeth") // actual solution
      )
    }
    "find the possible solutions for a puzzle with only one word" in {
      val grid = Grid.fromString(
        """gnn
          |lti
          |igh""".stripMargin)
      val puzzle = Puzzle(grid, Seq(9))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(Seq("lightning"))
    }
    "find the possible solutions for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      val puzzle = Puzzle(grid, Seq(7, 5))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(
        Seq("bicycle", "yacht")
      )
    }
    "find the possible solutions for a grid with three words" in {
      val grid = Grid.fromString(
        """ehs
          |sls
          |ufo
          |abm""".stripMargin)
      val puzzle = Puzzle(grid, Seq(4, 4, 4))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("fuse", "bosh", "alms"),
        Seq("bush", "leaf", "moss"),
        Seq("bush", "flea", "moss"),
        Seq("bush", "moss", "leaf"), // actual solution
        Seq("bush", "moss", "flea"),
        Seq("bosh", "alms", "fuse"),
        Seq("bosh", "fuse", "alms"),
        Seq("moss", "bush", "leaf"),
        Seq("moss", "bush", "flea")
      ))
    }
    "find the possible solutions for a five-by-four grid with four words" in {
      val grid = Grid.fromString(
        """agbs
          |toak
          |sdir
          |atoe
          |phts""".stripMargin)
      val puzzle = Puzzle(grid, Seq(5, 6, 5, 4))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("pasta", "hotdog", "rites", "bask"),
        Seq("pasta", "hotdog", "steak", "ribs"), // actual solution
        Seq("pasta", "hotdog", "stirk", "base"),
        Seq("pasta", "hotdog", "stirk", "sabe"),
        Seq("stork", "pastis", "adobe", "gath"),
        Seq("stork", "pastis", "adobe", "ghat"),
      ))
    }
    "find nothing when there are no possible words" in {
      val grid = Grid.fromString(
        """xxx
          |xxx
          |xxx""".stripMargin)
      val puzzle = Puzzle(grid, Seq(3, 6))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq()
    }
  }

  "Finding possible paths for a word leaving the grid able to contain a word of the next length" should {
    val grid = Grid.fromString(
      """thk
        |etc
        |eba""".stripMargin)
    solver.findGridPathsLeavingNextWordFindable(grid, 4, Some(5)).map { path: GridPath =>
      path.cells.map(grid.letterAt(_).getOrElse("")).mkString
    } must containTheSameElementsAs(Seq( // same as below, but missing one "bath" and two "beth"s
      "abet",
      "abet",
      "abet",
      "ache",
      "bach",
      "back",
      "bate",
      "bate",
      "beet",
      "beet",
      "beet",
      "beta",
      "beta",
      "bete",
      "bete",
      "beth",
      "etch",
      "etch",
      "hebe",
      "tack",
      "tete",
      "teth",
      "teth",
      "thee",
      "thee"
    ))
  }

  "Finding possible paths for a word starting anywhere" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsStartingAnywhere(grid, 4).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "abet",
        "abet",
        "abet",
        "ache",
        "bach",
        "back",
        "bate",
        "bate",
        "bath",
        "beet",
        "beet",
        "beet",
        "beta",
        "beta",
        "bete",
        "bete",
        "beth",
        "beth",
        "beth",
        "etch",
        "etch",
        "hebe",
        "tack",
        "tete",
        "teth",
        "teth",
        "thee",
        "thee"
      ))
    }
    "return the correct paths for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      solver.findGridPathsStartingAnywhere(grid, 7).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "acyclic", "bicycle"
      ))
    }
  }

  "Finding possible paths for a word starting at a given cell" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsStartingAtCell(grid, Cell(3, 2), 4) must containTheSameElementsAs(Seq(
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 3), Cell(1, 2))), // bach
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 3), Cell(1, 3))), // back
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 2), Cell(2, 1))), // bate
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 2), Cell(3, 1))), // bate
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 2), Cell(1, 2))), // bath
        GridPath(Seq(Cell(3, 2), Cell(2, 1), Cell(3, 1), Cell(2, 2))), // beet
        GridPath(Seq(Cell(3, 2), Cell(3, 1), Cell(2, 1), Cell(1, 1))), // beet
        GridPath(Seq(Cell(3, 2), Cell(3, 1), Cell(2, 1), Cell(2, 2))), // beet
        GridPath(Seq(Cell(3, 2), Cell(2, 1), Cell(2, 2), Cell(3, 3))), // beta
        GridPath(Seq(Cell(3, 2), Cell(3, 1), Cell(2, 2), Cell(3, 3))), // beta
        GridPath(Seq(Cell(3, 2), Cell(2, 1), Cell(2, 2), Cell(3, 1))), // bete
        GridPath(Seq(Cell(3, 2), Cell(2, 1), Cell(1, 1), Cell(1, 2))), // beth
        GridPath(Seq(Cell(3, 2), Cell(2, 1), Cell(2, 2), Cell(1, 2))), // beth
        GridPath(Seq(Cell(3, 2), Cell(3, 1), Cell(2, 2), Cell(1, 2))), // beth
        GridPath(Seq(Cell(3, 2), Cell(3, 1), Cell(2, 2), Cell(2, 1))), // bett
      ))
    }
    "return the correct paths for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      solver.findGridPathsStartingAtCell(grid, Cell(4, 3), 7).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "bicycle"
      ))
      "return the correct paths for a grid with empty letters" in {
        val grid = Grid.fromString(
          """___s
            |__bk
            |__ar
            |__ie
            |__ts""".stripMargin)
        solver.findGridPathsStartingAtCell(grid, Cell(5, 4), 5).map(grid.wordAt) must containTheSameElementsAs(Seq(
          "seria", "stirk", "steak"
        ))
      }
    }
  }

  "Finding possible next cells in an unknown word of a given length" should {
    "return the correct cells" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val gridPath = GridPath(Seq(Cell(3, 2))) // b...
      solver.findPossibleNextCellsForGridPath(grid, gridPath, 4) must containTheSameElementsAs(Seq(
        Cell(2, 1), // be...
        Cell(3, 1), // be...
        Cell(3, 3) // ba...
      ))
    }
  }



}
