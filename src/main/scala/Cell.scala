package com.wordbrainsolver.application

case class Cell(row: Int, col: Int) {
  override def toString: String = s"($row, $col)"
}


