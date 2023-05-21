package com.wordbrainsolver.application

case class Grid(letters: Seq[Option[Char]], width: Int) {
  require(letters.length % width == 0, s"Number of letters (${letters.length}) must be multiple of width ($width)")

  def letterAt(row: Int, col: Int): Option[Char] = {
    letters((row - 1) * width + col - 1)
  }

  override def toString: String = {
    letters.grouped(width).map((rowLetters: Seq[Option[Char]]) => {
      rowLetters.map(_.getOrElse(Grid.emptyLetterChar)).mkString
    }).mkString("\n")
  }
}

case object Grid {
  private val emptyLetterChar: Char = '_'

  def fromString(string: String): Grid = {
    val width = string.indexOf('\n')
    val letters: Seq[Option[Char]] = string.toCharArray.filterNot(_ == '\n').map {
      case Grid.emptyLetterChar => None
      case char: Char => Some(char)
    }
    Grid(letters, width)
  }
}
