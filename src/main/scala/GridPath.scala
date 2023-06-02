package com.wordbrainsolver.application

case class GridPath(cells: Seq[Cell]) {
  def add(newCell: Cell): GridPath = copy(cells :+ newCell)
}

case class GridPathAndWord(gridPath: GridPath, word: String)
