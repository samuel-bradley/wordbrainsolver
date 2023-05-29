package com.wordbrainsolver.application

case class Puzzle(grid: Grid, wordLengths: Seq[Int]) {
  require(grid.letters.length == wordLengths.sum, s"Grid has size ${grid.letters.length} but words have total size ${wordLengths.sum}")
}
