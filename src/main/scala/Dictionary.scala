package com.wordbrainsolver.application

trait Dictionary {
  def wordExists(putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean
  protected def wordMatches(word: String, putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean = {
    word.startsWith(putativeWordSoFar) && word.startsWith(revealedPart) && word.length == wordLength
  }
}

class ListDictionary(words: Seq[String]) extends Dictionary {
  def wordExists(putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean = {
    words.exists { word: String => wordMatches(word, putativeWordSoFar, revealedPart, wordLength) }
  }
}

class SearchTreeDictionary(searchTree: TernarySearchTree[Boolean]) extends Dictionary {
  def wordExists(putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean = {
    searchTree.keysWithPrefix(putativeWordSoFar).exists { word: String => wordMatches(word, putativeWordSoFar, revealedPart, wordLength) }
  }
}

object SearchTreeDictionary {
  def of(words: Seq[String]): Dictionary = {
    val trie = words.foldLeft(TernarySearchTree[Boolean])((trie, word) => trie.insert(word, true))
    new SearchTreeDictionary(trie)
  }
}

class TrieDictionary(trie: Trie[Boolean]) extends Dictionary {
  def wordExists(putativeWordSoFar: String, revealedPart: String, wordLength: Int): Boolean = {
    trie.search(putativeWordSoFar).getOrElse(false)
  }
}

object TrieDictionary {
  def of(words: Seq[String]): Dictionary = {
    val trie = words.foldLeft(Trie[Boolean])((trie, word) => trie.insert(word, true))
    new TrieDictionary(trie)
  }
}
