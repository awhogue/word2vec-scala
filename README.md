word2vec-scala
==============

A scala reader for binary formats output by the Google word2vec library (https://code.google.com/p/word2vec/)

Usage:
   val vocab = Vocab.loadFromFile(filename, 100000)
   vocab.bestMatches("burger")
