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
      (for ((k, v) <- (head zip lines.head)) yield (k -> v.replaceAll("^[\\\"\\\']+|[\\\"\\\']+$", "").trim)).toMap :: walk(lines.drop(1), head)
    }
  }

}

/**
  * read CSV data into List (Row) of Maps (colName -> Colvalue)
  *
  * @author jan.cajthaml
  */
object read extends ((String, Char, Map[String, String]) => List[Map[String, String]]) {

  import x.{walk}

  def apply(source: String, separator: Char, mapper: Map[String, String]): List[Map[String, String]] = {
    val condition: String = "(\\\"[^\\\"]+\\\")|[^\\" + separator + "]+"
    val rows: Array[String] = source.split("[\\r\\n]+") filterNot {_.matches(condition)}
    val header: Array[String] = rows.head.split(separator)
    val lines: Array[Array[String]] = rows.drop(1).map(_.split(separator))
    val toRemove: Array[Int] = header.zipWithIndex.collect{ case(a, b) if !mapper.isDefinedAt(a) => b}

    walk(
      lines.map(e => e.zipWithIndex.collect { case(a, b) if !toRemove.contains(b) => a }),
      header.filterNot({!mapper.isDefinedAt(_)}).map(mapper)
    )
  }

}