package com.wordbrainsolver.application

case class Coordinates(row: Int, col: Int) {
  override def toString: String = s"($row, $col)"
}


