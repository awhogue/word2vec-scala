// Copyright 2013
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package com.foursquare.common.nlp.word2vec

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.ByteOrder._
import java.nio.channels.FileChannel.MapMode._
import scala.collection.mutable.ListBuffer

// Tools to read the binary word models produced by the Google word2vec library (https://code.google.com/p/word2vec/).
//
// Usage:
//   val vocab = Vocab.loadFromFile(filename, 100000)
//   vocab.bestMatches("burger")

case class WordVector(vec: List[Float]) {
  def +(that: WordVector): WordVector = WordVector(this.vec.zip(that.vec).map{case (x, y) => x + y})
  def -(that: WordVector): WordVector = WordVector(this.vec.zip(that.vec).map{case (x, y) => x - y})
  def *(that: WordVector): Double = this.vec.zip(that.vec).map{case (x, y) => x * y}.sum
}

case class Vocab(
    words: Int,
    l1_size: Int,
    vocab: scala.collection.mutable.Map[String, WordVector] = scala.collection.mutable.Map[String, WordVector]()) {

  override def toString(): String = "Vocab with " + vocab.size + " entries"
  def apply(word: String) = vocab.get(word).getOrElse(WordVector(List.fill(l1_size)(0.0f)))
  
  def bestMatches(word: String, num: Int = 50): List[(String, Double)] = {
    val wordVec = this(word)
    vocab.toList.filterNot(_._1 == word).map(w => (w._1, w._2 * wordVec)).sortBy(_._2).reverse.take(num)
  }
                 }

object Vocab {
  // Load a vocabulary from the given file (in binary format), limiting to at most "limit" words (for memory reasons).
  def loadFromFile(filename: String, limit: Int): Vocab = {
    val (fh, buffer) = getFileBuffer(filename)
  
    val words = readInt(buffer)
    val vectorSize = readInt(buffer)
    println(words + " words in vocab")
    println(vectorSize + " word vector size")
  
    val vocab = Vocab(words, vectorSize)
    while (buffer.hasRemaining && vocab.vocab.size <= words && vocab.vocab.size <= limit) {
      vocab.vocab += (readString(buffer) -> readWordVector(buffer, vectorSize))
      if (vocab.vocab.size % 5000 == 0) println("Loaded " + vocab.vocab.size + " words")
    }

    println("Done, loaded " + vocab.vocab.size + " words")
    fh.close()
    vocab
  }

  // Open a file and return the FileInputStream and ByteBuffer associated with it.
  private def getFileBuffer(filename: String): (FileInputStream, ByteBuffer) = {
    val file = new File(filename)
    val fileSize = file.length
    val stream = new FileInputStream(file)
    val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)
    buffer.order(LITTLE_ENDIAN)
    (stream, buffer)
  }

  // Read bytes from the buffer until stopFunc returns false.
  private def readBytesUntil(buffer: ByteBuffer, stopFunc: (Byte) => Boolean): List[Byte] = {
    val lst = ListBuffer[Byte]()
    var c: Byte = 0
    while ({c = buffer.get; stopFunc(c)}) {
      lst += c
    }
    lst.toList
  }

  // Read a single int value from the buffer.
  private def readInt(buffer: ByteBuffer): Int = {
    readBytesUntil(buffer, c => (c >= 48 && c <= 57)).map(_.toChar).mkString.toInt
  }

  // Read a string composed of normal keyboard characters (non-control characters).
  private def readString(buffer: ByteBuffer): String = {
    readBytesUntil(buffer, c => (c >= 33 && c <= 126)).map(_.toChar).mkString
  }

  // Read a single word vector from the buffer.  Word vectors are encoded as binary floats,
  // followed by a "\n" to separate the records.
  private def readWordVector(buffer: ByteBuffer, length: Int): WordVector = {
    val lst = ListBuffer[Float]()
    1 to length foreach { _ => lst += buffer.getFloat }
    buffer.get // Read off the last "\n" character separating the records in the binary format.

    // Normalize the values in the vector.
    val len = math.sqrt(lst.map(x => x*x).sum).toFloat

    WordVector(lst.map(x => x / len).toList)
  }
}
