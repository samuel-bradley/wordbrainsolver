package com.wordbrainsolver.application

case class PossiblePathsAndWords(gridPathAndWord: GridPathAndWord, nextPathsAndWords: Seq[PossiblePathsAndWords]) {
  def getGridPathsAndWords: Seq[Seq[GridPathAndWord]] = {
    if (nextPathsAndWords.isEmpty) Seq(Seq(gridPathAndWord)) else {
      nextPathsAndWords.flatMap { possiblePaths: PossiblePathsAndWords =>
        possiblePaths.getGridPathsAndWords.map(pathsAndWords => Seq(gridPathAndWord) ++ pathsAndWords)
      }
    }
  }
}
