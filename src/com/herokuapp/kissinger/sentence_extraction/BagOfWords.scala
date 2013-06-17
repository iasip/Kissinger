package com.herokuapp.kissinger.sentence_extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/** Sentences from multiple documents are represented as an unordered collection of words.
  *  
  * @param sentences 
  */
class BagOfWords(sentences: ArrayBuffer[Array[String]]) {
  var tfs = new HashMap[String, Integer]()
  var sfs = new HashMap[String, Double]()
  var tfIsfs = new HashMap[String, Double]()
  val sCount = sentences.length
  for (sentence <- sentences) {
    for (token <- sentence) {
      if (!tfs.contains(token)) {
        tfs.put(token, 1)
        sfs.put(token, 0.0)
      } else {
        tfs.update(token, tfs(token) + 1)
      }
    }
    for (key <- sfs.keys) {
      if (sentence contains key) {
        sfs.update(key, sfs(key) + 1.0)
      }
    }
  }
  
  for (key <- tfs.keys) {
    val tf = tfs(key)
    val isf = math.log(sCount / sfs(key))
    tfIsfs.put(key, tf*isf)
  }
  
  def createTermVector(tokens: Array[String]): HashMap[String, Double] = {
    var termVector = new HashMap[String, Double]
    for (token <- tokens) {
      termVector.put(token, tfIsfs(token))
    }
    return termVector
  }
  
  override def toString(): String = {
    var output = new StringBuilder()
    tfIsfs.foreach(term_weight => output ++= (term_weight._1.toString() + ": " + term_weight._2.toString + "\n"))
    return output.mkString
  }
}