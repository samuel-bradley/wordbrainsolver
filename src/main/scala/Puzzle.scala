package com.wordbrainsolver.application

case class Puzzle(grid: Grid, wordsToFind: Seq[WordToFind]) {
  require(grid.letters.length == wordsToFind.map(_.length).sum, s"Grid has size ${grid.letters.length} but words have total size ${wordsToFind.map(_.length).sum}")
}

case class WordToFind(length: Int, revealedPart: String) {
  require(revealedPart.length <= length, s"Revealed part of word to find ($revealedPart) cannot be longer than word to find ($length)")
}
