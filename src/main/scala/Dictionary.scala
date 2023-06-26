package com.wordbrainsolver.application

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class Dictionary(words: Seq[String]) {
  def wordExists(putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean = {
    words.exists { word =>
      word.startsWith(putativeWordSoFar) && word.startsWith(revealedPart) && word.length == wordLength
    }
  }
}

object Dictionary {
  def fromFile(path: Path, additionalWords: Seq[String]): Dictionary = {
    val words: Seq[String] = (Files.readAllLines(path).asScala.toSeq ++ additionalWords).sorted
    new Dictionary(words)
  }
}
