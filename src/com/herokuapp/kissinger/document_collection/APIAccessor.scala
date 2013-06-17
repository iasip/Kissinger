package com.heroku.kissinger.document_collection

import scala.io.Source.fromURL
import scala.util.parsing.json._

/** Handles JSON-encoded results from the (now-deprecated-but-still-functional) Google News API **/
object APIAccessor extends Application{
  override def main(args: Array[String]) {
    val result = JSON.parseFull(fromURL("https://ajax.googleapis.com/ajax/services/search/news?v=1.0&q=barack%20obama").mkString)
    result match {
      case Some(e) => println(e)
      case None => println("Failed.")
    }
  }
}