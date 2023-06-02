package com.wordbrainsolver.application

case class PossiblePaths(gridPath: GridPath, nextGridPaths: Seq[PossiblePaths]) {
  def getGridPaths: Seq[Seq[GridPath]] = {
    if (nextGridPaths.isEmpty) Seq(Seq(gridPath)) else {
      nextGridPaths.flatMap { possiblePaths: PossiblePaths =>
        possiblePaths.getGridPaths.map(words => Seq(gridPath) ++ words)
      }
    }
  }
}
