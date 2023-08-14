package com.wordbrainsolver.application

import org.specs2.mutable.Specification

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class PuzzleSolverSpec extends Specification{

  // https://www.wordgamedictionary.com/sowpods/download/sowpods.txt
  private val dictionaryPath = Path.of("C:\\Users\\Samuel\\Documents\\wordbrainsolver\\src\\main\\scala\\dictionary.txt")
  private val dictionary = new ListDictionary(Files.readAllLines(dictionaryPath).asScala.toSeq)
  private val solver = new PuzzleSolver(dictionaryPath)

  "Solving a puzzle" should {
    "find the correct path and word for the smallest possible" in {
      // This test asserts that the grid path matches the word - subsequent tests will just check the words for brevity
      val grid = Grid.fromString(
        """fi
          |sh
          |""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(4)))
      solver.findPossibleSolutions(puzzle) mustEqual Seq(Seq(
        GridPathAndWord(GridPath(Seq(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2))), "fish")
      ))
    }
    "find the possible solutions for a small puzzle, including multiple paths for the same word" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(4, 5)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(
        Seq("tack", "thebe"),
        Seq("tack", "thebe"),
        Seq("bete", "thack"),
        Seq("beet", "thack"),
        Seq("beet", "thack"),
        Seq("beet", "thack"),
        Seq("bete", "thack"),
        Seq("back", "teeth"), // actual solution
        Seq("back", "thete"),
        Seq("back", "thete"),
        Seq("back", "teeth") // actual solution
      )
    }
    "find the possible solutions for a puzzle with only one word" in {
      val grid = Grid.fromString(
        """gnn
          |lti
          |igh""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(9)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(Seq("lightning"))
    }
    "find the possible solutions for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(7, 5)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq(
        Seq("bicycle", "yacht")
      )
    }
    "find the possible solutions matching revealed letters" in {
      val grid = Grid.fromString(
        """ehs
          |sls
          |ufo
          |abm""".stripMargin)
      val puzzle = Puzzle(grid, Seq(WordToFind(4, "bu"), WordToFind(4, "m"), WordToFind(4, "")))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("bush", "moss", "leaf"), // actual solution
        Seq("bush", "moss", "alef"),
        Seq("bush", "moss", "feal"),
        Seq("bush", "moss", "flea")
      ))
    }
    "find the possible solutions when the last word is revealed" in {
      val grid = Grid.fromString(
        """ehs
          |sls
          |ufo
          |abm""".stripMargin)
      val puzzle = Puzzle(grid, Seq(WordToFind(4, ""), WordToFind(4, ""), WordToFind(4, "leaf")))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("bush", "moss", "leaf"), // actual solution
        Seq("mosh", "subs", "leaf"),
        Seq("moss", "bush", "leaf")
      ))
    }
    "find no solutions when a revealed letter matches no words" in {
      val grid = Grid.fromString(
        """ehs
          |sls
          |ufo
          |abm""".stripMargin)
      val puzzle = Puzzle(grid, Seq(WordToFind(4, ""), WordToFind(4, ""), WordToFind(4, "o")))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Nil
    }
    "find the possible solutions for a five-by-four grid with four words and many possible solutions" in {
      val grid = Grid.fromString(
        """agbs
          |toak
          |sdir
          |atoe
          |phts""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(5, 6, 5, 4)))
      // There are a vast number of solutions here which are omitted for brevity
      val solutions = solver.findPossibleSolutions(puzzle).map(_.map(_.word))
      // TODO solutions such as this are not useful; identifying the correct one takes longer than solving manually
      solutions must have size 1049
      solutions must contain(Seq("pasta", "hotdog", "steak", "ribs"))
    }
    "find the possible solutions for a five-by-five grid with five words" in {
      val grid = Grid.fromString(
        """atsoe
          |isusj
          |grrea
          |uapsv
          |mbpca""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(7, 4, 8, 3, 3)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)).distinct must containTheSameElementsAs(Seq(
        Seq("barista", "java", "espresso", "mug", "cup"), // actual solution
        Seq("airgaps", "joss", "precavae", "tum", "sub"),
        Seq("airgaps", "joss", "precavae", "tum", "bus"),
        Seq("airgaps", "joss", "precavae", "sum", "tub"),
        Seq("airgaps", "joss", "precavae", "sum", "but"),
        Seq("airgaps", "joss", "precavae", "sub", "mut"),
        Seq("airgaps", "joss", "precavae", "sub", "tum"),
        Seq("airgaps", "joss", "precavae", "mut", "sub"),
        Seq("airgaps", "joss", "precavae", "mut", "bus"),
        Seq("airgaps", "joss", "precavae", "mus", "tub"),
        Seq("airgaps", "joss", "precavae", "mus", "but"),
        Seq("airgaps", "joss", "precavae", "bum", "uts"),
        Seq("airgaps", "joss", "precavae", "bus", "mut"),
        Seq("airgaps", "joss", "precavae", "bus", "tum"),
        Seq("jaspers", "taig", "sambucas", "evo", "urp"),
        Seq("jaspers", "taig", "sambucas", "urp", "evo"),
        Seq("jaspers", "taig", "sambucas", "urp", "voe"),
        Seq("jaspers", "taig", "sambucas", "voe", "urp"),
        Seq("jaspers", "grum", "copaibas", "uts", "ave"),
        Seq("jaspers", "grum", "copaibas", "ave", "uts"),
        Seq("jaspers", "gaum", "airposts", "cub", "ave"),
        Seq("jaspers", "gaum", "airposts", "ave", "cub"),
        Seq("jaspers", "curs", "gambusia", "opt", "ave"),
        Seq("jaspers", "curs", "gambusia", "ave", "opt"),
        Seq("jaspers", "cour", "gambusia", "pst", "ave"),
        Seq("jaspers", "cour", "gambusia", "ave", "pst"),
        Seq("asperse", "jour", "gambusia", "vac", "pst"),
        Seq("asperse", "jour", "gambusia", "pst", "vac"),
        Seq("barista", "java", "espresso", "gum", "cup")
      ))
    }
    "find the possible solutions for a five-by-five grid with five words and many possible solutions" in {
      val grid = Grid.fromString(
        """kilrc
          |rller
          |asake
          |guica
          |fbmtm""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(5, 5, 4, 5, 6)))
      val solutions = solver.findPossibleSolutions(puzzle).map(_.map(_.word)).distinct
      solutions must have size 315
      solutions must contain(Seq("sugar", "cream", "milk", "black", "filter"))
    }
    "find the possible solutions for a seven-by-seven grid" in {
      // https://wordbrain.info/en/themes/school2/
      val grid = Grid.fromString(
        """tsyuaos
          |skalkrw
          |teoeome
          |axnshle
          |msooslr
          |estsbru
          |ebalcly""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(8, 9, 6, 5, 8, 5, 8)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("homework", "classmate", "lesson", "essay", "textbook", "ruler", "syllabus"),
        Seq("homework", "classmate", "lesson", "essay", "textbook", "ruler", "syllabus"),
        Seq("homework", "classmate", "lesson", "ruler", "syllabus", "essay", "textbook"),
        Seq("homework", "classmate", "lesson", "ruler", "syllabus", "essay", "textbook")
      ))
    }
    "find the possible solutions for an eight-by-eight grid" in {
      // https://wordbrain.info/en/themes/technology3/
      val grid = Grid.fromString(
        """ernngnor
          |vsoiyien
          |rviratdo
          |ieteprac
          |etaenbai
          |cligiplm
          |umrneiur
          |otvanatl""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(11, 11, 11, 10, 6, 6, 9)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("engineering", "application", "ultramodern", "multiverse", "vector", "binary", "variation"),
        Seq("engineering", "application", "ultramodern", "multiverse", "binary", "vector", "variation")
      ))
    }
    "find the possible solutions for an eight-by-eight grid with more words" in {
      // https://wordbrain.info/en/themes/in-the-kitchen3/
      val grid = Grid.fromString(
        """nucetakt
          |kanepoae
          |nevshler
          |isieocnt
          |cetcokse
          |squhtsal
          |asnseitl
          |bauedtmi""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(9, 4, 7, 5, 4, 7, 10, 6, 8, 4)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) must containTheSameElementsAs(Seq(
        Seq("chocolate", "dish", "banquet", "sieve", "sink", "toaster", "sustenance", "kettle", "saucepan", "milk")
      ))
    }
    "find nothing when there are no possible words" in {
      val grid = Grid.fromString(
        """xxx
          |xxx
          |xxx""".stripMargin)
      val puzzle = Puzzle(grid, unrevealedWords(Seq(3, 6)))
      solver.findPossibleSolutions(puzzle).map(_.map(_.word)) mustEqual Seq()
    }
  }

  "Finding possible paths for a word leaving the grid able to contain a word of the next length" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsLeavingNextWordFindable(grid, WordToFind(4, ""), Some(5), dictionary).map { path: GridPath =>
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
        "batt",
        "beet",
        "beet",
        "beet",
        "beta",
        "beta",
        "bete",
        "bete",
        "beth",
        "cate",
        "cate",
        "etch",
        "etch",
        "ethe",
        "hebe",
        "hete",
        "khet",
        "khet",
        "tach",
        "tack",
        "tete",
        "teth",
        "teth",
        "thee",
        "thee"
      ))
    }
  }

  "Finding possible paths for a word starting anywhere" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsStartingAnywhere(grid, WordToFind(4, ""), dictionary).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "abet",
        "abet",
        "abet",
        "ache",
        "bach",
        "back",
        "bate",
        "bate",
        "bath",
        "batt",
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
        "cate",
        "cate",
        "ethe",
        "etch",
        "etch",
        "hebe",
        "hete",
        "khet",
        "khet",
        "tach",
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
      solver.findGridPathsStartingAnywhere(grid, WordToFind(7, ""), dictionary).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "lecythi", "acyclic", "bicycle"
      ))
    }
  }

  "Finding possible paths for a word starting at a given cell" should {
    "return the correct paths" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      solver.findGridPathsStartingAtCell(grid, Cell(3, 2), WordToFind(4, ""), dictionary) must containTheSameElementsAs(Seq(
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
        GridPath(Seq(Cell(3, 2), Cell(3, 3), Cell(2, 2), Cell(1, 1))), // batt
      ))
    }
    "return the correct paths for a non-square grid" in {
      val grid = Grid.fromString(
        """tye
          |hcl
          |aic
          |cyb""".stripMargin)
      solver.findGridPathsStartingAtCell(grid, Cell(4, 3), WordToFind(7, ""), dictionary).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "bicycle"
      ))
    }
    "return the correct paths for a grid with empty letters" in {
      val grid = Grid.fromString(
        """___s
          |__bk
          |__ar
          |__ie
          |__ts""".stripMargin)
      // Starting at the bottom-right 's'
      solver.findGridPathsStartingAtCell(grid, Cell(5, 4), WordToFind(5, ""), dictionary).map(grid.wordAt) must containTheSameElementsAs(Seq(
        "serks", "serai", "stirk", "stire", "steak", "stear"
      ))
    }
  }

  "Finding possible next cells in an unknown word of a given length" should {
    "return the correct cells" in {
      val grid = Grid.fromString(
        """thk
          |etc
          |eba""".stripMargin)
      val gridPath = GridPath(Seq(Cell(3, 2))) // b...
      solver.findPossibleNextCellsForGridPath(grid, gridPath, WordToFind(4, ""), dictionary) must containTheSameElementsAs(Seq(
        Cell(2, 1), // be...
        Cell(3, 1), // be...
        Cell(3, 3) // ba...
      ))
    }
  }

  private def unrevealedWords(lengths: Seq[Int]): Seq[WordToFind] = lengths.map(WordToFind(_, ""))

}
