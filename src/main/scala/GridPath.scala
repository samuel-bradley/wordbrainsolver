package com.wordbrainsolver.application

case class GridPath(coordinates: Seq[Coordinates]) {
  def add(newCoordinates: Coordinates): GridPath = copy(coordinates :+ newCoordinates)
}
