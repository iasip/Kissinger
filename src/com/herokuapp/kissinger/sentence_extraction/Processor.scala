package com.herokuapp.kissinger.sentence_extraction

import scala.io.Source.fromFile
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/** Reads in files and provides helper functions **/
object Processor {
  
  val path = "/Where/My/Data/Is/"
  val tokenizedEnFiles = new java.io.File(path+"/Topic1-en").listFiles()
    .filter(_.getName().endsWith(".txt.tokenized"))
  val tokenizedCnFiles = new java.io.File(path+"/Topic1-ch").listFiles()
    .filter(_.getName().endsWith(".txt.tokenized"))
  val tokenizedEnCnFiles = new java.io.File(path+"/Topic1-en").listFiles()
    .filter(_.getName().endsWith(".transcn.tokenized"))
  val tokenizedCnEnFiles = new java.io.File(path+"/Topic1-ch").listFiles()
    .filter(_.getName().endsWith(".transen.tokenized"))

  var enSents = new ArrayBuffer[Array[String]]()
  var cnSents = new ArrayBuffer[Array[String]]()
  var enCnSents = new ArrayBuffer[Array[String]]()
  var cnEnSents = new ArrayBuffer[Array[String]]()
  
  for (file <- tokenizedEnFiles) {
    for(tokenizedSent <- fromFile(file).getLines.toList) {
      enSents += tokenizedSent.split("\\s+")
    }
  }

  for (file <- tokenizedEnCnFiles) {
    for(tokenizedSent <- fromFile(file).getLines.toList) {
      enCnSents += tokenizedSent.split("\\s+")
    }
  }
  
  for (file <- tokenizedCnEnFiles) {
    for(tokenizedSent <- fromFile(file).getLines.toList) {
      cnEnSents += tokenizedSent.split("\\s+")
    }
  }
    
  for (file <- tokenizedCnFiles) {
    for(tokenizedSent <- fromFile(file).getLines.toList) {
      cnSents += tokenizedSent.split("\\s+")
    }
  }
  
  val enBOW = new BagOfWords(enSents)
  val cnBOW = new BagOfWords(cnSents)
  val enCnBOW = new BagOfWords(enCnSents)
  val cnEnBOW = new BagOfWords(cnEnSents)
  
  def getTermVector(tokens: Array[String], lang: String): HashMap[String, Double] = lang match {
    case "en" => enBOW.createTermVector(tokens)
    case "cn" => cnBOW.createTermVector(tokens)
    case "encn" => enCnBOW.createTermVector(tokens)
    case "cnen" => cnEnBOW.createTermVector(tokens)
  } 
}
