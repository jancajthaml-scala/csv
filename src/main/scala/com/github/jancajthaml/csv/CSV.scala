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

object Main extends App {
  
  val data: String = """25.08.2016 #165
země|měna|množství|kód|kurz
Austrálie|dolar|1|AUD|18,214
Brazílie|real|1|BRL|7,435
Bulharsko|lev|1|BGN|13,821
Čína|renminbi|1|CNY|3,597
Dánsko|koruna|1|DKK|3,631
EMU|euro|1|EUR|27,030
Filipíny|peso|100|PHP|51,669
Hongkong|dolar|1|HKD|3,087
Chorvatsko|kuna|1|HRK|3,608
Indie|rupie|100|INR|35,704
Indonesie|rupie|1000|IDR|1,808
Izrael|šekel|1|ILS|6,362
Japonsko|jen|100|JPY|23,825
Jihoafrická rep.|rand|1|ZAR|1,694
Jižní Korea|won|100|KRW|2,144
Kanada|dolar|1|CAD|18,520
Maďarsko|forint|100|HUF|8,725
Malajsie|ringgit|1|MYR|5,962
Mexiko|peso|1|MXN|1,296
MMF|SDR|1|XDR|33,624
Norsko|koruna|1|NOK|2,914
Nový Zéland|dolar|1|NZD|17,489
Polsko|zlotý|1|PLN|6,269
Rumunsko|nové leu|1|RON|6,069
Rusko|rubl|100|RUB|36,883
Singapur|dolar|1|SGD|17,697
Švédsko|koruna|1|SEK|2,846
Švýcarsko|frank|1|CHF|24,786
Thajsko|baht|100|THB|69,246
Turecko|lira|1|TRY|8,136
USA|dolar|1|USD|23,938
Velká Británie|libra|1|GBP|31,587"""

  for (i <- read(data, '|').toSeq) {
    println(i.map(pair => pair._1+"="+pair._2).mkString("",", ",""))
  }

}