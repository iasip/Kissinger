package com.herokuapp.kissinger.sentence_extraction

import scala.collection.mutable.HashMap

object VectorSpaceModel {
  def normalizedEuclideanDistance(v1: HashMap[String, Double], v2: HashMap[String, Double]): Double = {
    var sum = 0.0
    for (key <- v1.keys) {
      if (v2 contains key) {
        sum = sum + math.pow((v1(key)-v2(key)), 2) 
      } else {
        sum = sum + math.pow(v1(key), 2)
      }
    }
    for (key <- v2.keys) {
      if (!(v1 contains key)) {
        sum = sum + math.pow(v2(key), 2)
      }
    }
    var norm1 = 0.0
    var norm2 = 0.0
    for (value <- v1.values) {
      norm1 = norm1 + math.pow(value, 2)
    }
    norm1 = math.sqrt(norm1)
    for (value <- v2.values) {
      norm2 = norm2 + math.pow(value, 2)
    }
    norm2 = math.sqrt(norm2)
    return math.sqrt(sum) / (norm1+norm2)
  }
  
  def cosineSimilarity(v1: HashMap[String, Double], v2: HashMap[String, Double]): Double = {
    var sum = 0.0
    var shorter = new HashMap[String, Double]
    var longer = new HashMap[String, Double]
    if (v1.size < v2.size) {
      shorter = v1
      longer = v2
    } else {
      shorter = v2
      longer = v1
    }
    for (key <- shorter.keys) {
      if (longer contains key) {
        sum = sum + (shorter(key) * longer(key))
      }
    }
    var m1 = 0.0
    var m2 = 0.0
    for (value <- v1.values) {
      m1 = m1 + math.pow(value, 2)
    }
    for (value <- v2.values) {
      m2 = m2 + math.pow(value, 2)
    }
    if (m1 == 0.0) m1 = 1.0
    if (m2 == 0.0) m2 = 1.0
    return sum / (m1*m2)
  }
}