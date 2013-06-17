package com.herokuapp.kissinger.sentence_extraction

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object Driver extends Application{
  override def main(args: Array[String]) {
    val ranker = new ConstrainedCoRank
    val differenceScores = ranker.differenceScore(10, 0.8, 0.2)
    var enScoreSentence = new HashMap[Double, String]
    var cnScoreSentence = new HashMap[Double, String]
    var enSortedScores = new ArrayBuffer[Double]
    var cnSortedScores = new ArrayBuffer[Double]
    var enDiffScores = differenceScores._1
    var cnDiffScores = differenceScores._2
    for (i <- 0 until ranker.m) {
      if (enDiffScores.get(i, 0) != 0.0) {
        enScoreSentence.put(enDiffScores.get(i, 0), Processor.enSents(i).mkString(" ")+".")
        enSortedScores += enDiffScores.get(i,0)
      }
    }
    enSortedScores = enSortedScores.sortBy(-_)
    
    for (i <- 0 until 5) {
      println(enScoreSentence(enSortedScores(i)))
    }
    
    println()
    
    for (i <- 0 until ranker.n) {
      if (cnDiffScores.get(i, 0) != 0.0) {
        cnScoreSentence.put(cnDiffScores.get(i, 0), Processor.cnSents(i).mkString("")+".")
        cnSortedScores += cnDiffScores.get(i, 0)
      }
    }
    cnSortedScores = cnSortedScores.sortBy(-_)
    
    for (i <- 0 until 5) {
      println(cnScoreSentence(cnSortedScores(i)))
    }
    
    
    
  }
}