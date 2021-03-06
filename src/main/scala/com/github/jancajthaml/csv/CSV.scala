package com.github.jancajthaml.csv

/**
  * read CSV data into List (Row) of Maps (colName -> Colvalue)
  *
  * @author jan.cajthaml
  */
object read extends ((String, Char, Map[Int, String]) => List[Map[String, String]]) {

  /* scoped type for more readable code */
  type Pair = Map[String, String]

  /**
    * parse and map csv payload to list of key-value'd pairs based on
    * filter/projection `mapper`
    *
    * @param source - multiline string of csv payload
    * @param separator - col separator character
    * @param mapper - cols filter/projection Map
    * @return List of Maps
    */
  def apply(source: String, separator: Char, mapper: Map[Int, String]): List[Pair] = {
    val empty: Array[String] = Array.empty[String]
    //csv pattern is (any_until_separator,separator)*
    val pattern: String = "(\\\"[^\\\"]+\\\")|[^\\" + separator + "]+"
    //remove non csv lines from source
    val rows: Array[String] = source.split("[\\r\\n]+").filter(x => (!x.isEmpty() && !x.matches(pattern)))
    //first line is csv header
    //val header: Array[String] = rows.headOption.getOrElse("").split(separator)
    //everything else is csv data
    val lines: Array[Array[String]] = rows.map(_.split(separator))
    //based on `mapper` filter get cols to keep (will drop others)
    val keepIndexes: Array[Int] = mapper.keys.map(_ - 1).toArray //map (_ - 1) //values.toList
    //val keepIndexes: Array[Int] = header.zipWithIndex.collect {case (a, b) if mapper.isDefinedAt(a) => b}
    //Sanify value, deletes trailing/leading quotes and spaces
    def clean(v: String) = v.replaceAll("^[\\\"\\\']+|[\\\"\\\']+$", "").trim
    //Walk row by row recursively and build Map in each step
    @scala.annotation.tailrec def walk(x: Array[Array[String]], head: Array[String], result: List[Pair]): List[Pair] = {
      if (x.isEmpty) result else {
        //iterate header and data line simultanelously map header col to value col. Then recurse to next line
        val step: List[Pair] = result :+ (for {(k, v) <- (head zip x.headOption.getOrElse(empty))} yield (k -> clean(v))).toMap
        walk(x.drop(1), head, step)
      }
    }
    //enter recursion and return aggregated immutable result
    walk(
      //drop header values that are not wanted by `mapper` filter
      lines.map(e => e.zipWithIndex.collect {case(a, b) if keepIndexes.contains(b) => a}),
      //drop data cols that are not wanted by `mapper` filter
      lines.headOption.getOrElse(empty).zipWithIndex.collect {case(a, b) if keepIndexes.contains(b) => mapper(b + 1)},
      //enter recursion with empty List
      List.empty[Pair]
    ).filterNot(_.isEmpty)
  }
}
