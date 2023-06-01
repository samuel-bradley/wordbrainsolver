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
    "find the possible solutions for a small puzzle" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val puzzle = Puzzle(grid, Seq(4, 5))
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) mustEqual Seq(
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
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) mustEqual Seq(Seq("lightning"))
    }
    "find the possible solutions for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      val puzzle = Puzzle(grid, Seq(7, 5))
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) mustEqual Seq(
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
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) must containTheSameElementsAs(Seq(
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
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) must containTheSameElementsAs(Seq(
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
      solver.findPossibleSolutions(puzzle).map(paths => gridPathsToWords(grid, paths)) mustEqual Seq()
    }
  }

  "Finding possible paths for a word leaving the grid able to contain a word of the next length" should {
    val grid = Grid.fromString(
      """thk
        |etc
        |eba""".stripMargin)
    solver.findGridPathsLeavingNextWordFindable(grid, 4, Some(5)).map { path: GridPath =>
      path.coordinates.map(grid.letterAt(_).getOrElse("")).mkString
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

  "Finding possible paths for a word starting at a given letter" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsStartingWithCoordinates(grid, Coordinates(3, 2), 4) must containTheSameElementsAs(Seq(
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 3), Coordinates(2, 3), Coordinates(1, 2))), // bach
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 3), Coordinates(2, 3), Coordinates(1, 3))), // back
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 3), Coordinates(2, 2), Coordinates(2, 1))), // bate
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 3), Coordinates(2, 2), Coordinates(3, 1))), // bate
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 3), Coordinates(2, 2), Coordinates(1, 2))), // bath
        GridPath(Seq(Coordinates(3, 2), Coordinates(2, 1), Coordinates(3, 1), Coordinates(2, 2))), // beet
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 1), Coordinates(2, 1), Coordinates(1, 1))), // beet
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 1), Coordinates(2, 1), Coordinates(2, 2))), // beet
        GridPath(Seq(Coordinates(3, 2), Coordinates(2, 1), Coordinates(2, 2), Coordinates(3, 3))), // beta
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 1), Coordinates(2, 2), Coordinates(3, 3))), // beta
        GridPath(Seq(Coordinates(3, 2), Coordinates(2, 1), Coordinates(2, 2), Coordinates(3, 1))), // bete
        GridPath(Seq(Coordinates(3, 2), Coordinates(2, 1), Coordinates(1, 1), Coordinates(1, 2))), // beth
        GridPath(Seq(Coordinates(3, 2), Coordinates(2, 1), Coordinates(2, 2), Coordinates(1, 2))), // beth
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 1), Coordinates(2, 2), Coordinates(1, 2))), // beth
        GridPath(Seq(Coordinates(3, 2), Coordinates(3, 1), Coordinates(2, 2), Coordinates(2, 1))), // bett
      ))
    }
    "return the correct paths for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      solver.findGridPathsStartingWithCoordinates(grid, Coordinates(4, 3), 7).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "bicycle"
      ))
      "return the correct paths for a grid with empty letters" in {
        val grid = Grid.fromString(
          """___s
            |__bk
            |__ar
            |__ie
            |__ts""".stripMargin)
        solver.findGridPathsStartingWithCoordinates(grid, Coordinates(5, 4), 5).map(grid.wordAt) must containTheSameElementsAs(Seq(
          "seria", "stirk", "steak"
        ))
      }
    }
  }

  "Finding possible next coordinates in an unknown word of a given length" should {
    "return the correct coordinates" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val gridPath = GridPath(Seq(Coordinates(3, 2))) // b...
      solver.findPossibleNextCoordinatesForGridPath(grid, gridPath, 4) must containTheSameElementsAs(Seq(
        Coordinates(2, 1), // be...
        Coordinates(3, 1), // be...
        Coordinates(3, 3) // ba...
      ))
    }
  }

  private def gridPathsToWords(grid: Grid, gridPaths: Seq[GridPath]): Seq[String] = {
    gridPaths.headOption.map { firstPath =>
      Seq(grid.wordAt(firstPath)) ++ gridPathsToWords(grid.withWordRemoved(firstPath), gridPaths.tail)
    }.getOrElse(Nil)
  }

}
