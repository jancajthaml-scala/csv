package com.github.jancajthaml.csv

/**
  * Recursion immutable based line processing
  *
  * @author jan.cajthaml
  */
private[jancajthaml] object x {

  def walk(lines: Array[Array[String]], head: Array[String]): List[Map[String, String]] = {
    if (lines.isEmpty) {
      List.empty
    } else {
      (for ((k, v) <- (head zip lines.head)) yield (k -> v)).toMap :: walk(lines.drop(1), head)
    }
  }

}

/**
  * read CSV data into List (Row) of Maps (colName -> Colvalue)
  *
  * @author jan.cajthaml
  */
object read extends ((String, Char) => List[Map[String, String]]) {

  import x.{walk}

  def apply(source: String, separator: Char): List[Map[String, String]] = {
    val condition: String = "(\\\"[^\\\"]+\\\")|[^\\" + separator + "]+"
    val rows: Array[String] = source.split("[\\r\\n]+") filterNot {_.matches(condition)}
    val lines: Array[Array[String]] = rows.map( e => e.split(separator) )
    walk(lines.drop(1), lines.head)
  }

}