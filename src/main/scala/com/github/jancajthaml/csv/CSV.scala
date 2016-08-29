package com.github.jancajthaml.csv

/**
  * Recursion immutable based line processing
  *
  * @author jan.cajthaml
  */
private[jancajthaml] object x {

  /**
    * Sanify value, deletes trailing characters
    *
    * @param value
    * @return cleaned value
    */
  def cleanUp(value: String): String = {
    //remove double wrapped strings sometime present in csv and trim
    value.replaceAll("^[\\\"\\\']+|[\\\"\\\']+$", "").trim
  }

  /**
    * Walk lines line by line recursively and build Map in each step
    *
    * @param lines - array of separator separated strings (data lines)
    * @param head - separator separated string (header line)
    * @return List of Maps
    */
  def walk(lines: Array[Array[String]], head: Array[String]): List[Map[String, String]] = {
    if (lines.isEmpty) {
      List.empty
    } else {
      //iterate header and data line simultanelously map header col
      //to value col. Then recurse to next line
      (for ((k, v) <- (head zip lines.head)) yield (k -> cleanUp(v))).toMap :: walk(lines.drop(1), head)
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

  /**
    * parse and map csv payload to list of key-value'd pairs based on
    * filter/projection `mapper`
    *
    * @param source - multiline string of csv payload
    * @param separator - col separator character
    * @param mapper - cols filter/projection Map
    * @return List of Maps
    */
  def apply(source: String, separator: Char, mapper: Map[String, String]): List[Map[String, String]] = {
    //csv pattern is (any_until_separator,separator)*
    val csvPattern: String = "(\\\"[^\\\"]+\\\")|[^\\" + separator + "]+"
    //remove non csv lines from source
    val rows: Array[String] = source.split("[\\r\\n]+").filter(!_.isEmpty()).filter(!_.matches(csvPattern))
    //first line is csv header
    val header: Array[String] = rows.head.split(separator)
    //everything else is csv data
    val lines: Array[Array[String]] = rows.drop(1).map(_.split(separator))
    //based on `mapper` filter get cols to keep, drop others
    val keepIndexes: Array[Int] = header.zipWithIndex.collect{
      case(a, b) if mapper.isDefinedAt(a) => b
    }

    walk(
      //drop header values that are not wanted by `mapper` filter
      lines.map(e => e.zipWithIndex.collect { case(a, b) if keepIndexes.contains(b) => a }),
      //drop data cols that are not wanted by `mapper` filter
      header.filterNot({!mapper.isDefinedAt(_)}).map(mapper)
    )
  }

}