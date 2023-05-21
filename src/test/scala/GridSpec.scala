package com.wordbrainsolver.application

import org.specs2.mutable.Specification

class GridSpec extends Specification {

  "instantiating a Grid" should {
    "throw an exception if the number of letters is not a multiple of the width" in {
      Grid(Seq(Some('a'), Some('b'), Some('c')), 2) must throwAn[IllegalArgumentException]
      Grid(Seq(Some('a'), Some('b'), None), 2) must throwAn[IllegalArgumentException]
    }
    "not throw an exception if the number of letters is a multiple of the width" in {
      Grid(Seq(Some('a'), Some('b'), Some('c'), Some('d')), 2) must not(throwAn[Exception])
      Grid(Seq(Some('a'), Some('b'), Some('c'), None), 2) must not(throwAn[Exception])
    }
  }

}
