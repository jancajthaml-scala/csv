package com.github.jancajthaml.csv

import collection.mutable.Stack
import org.scalatest._

class CSVSpecs extends FlatSpec with Matchers {

  "read" should "deserialize causal csv to map with index projection" in {
    val data: String = """
    25.08.2016 #165
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
    Velká Británie|libra|1|GBP|31,587
    """

    val mapping = Map(
      3 -> "amount",
      4 -> "currency",
      5 -> "rate"
    )

    val parsed:List[Map[String, String]] = read(data, '|', mapping)

    parsed should have size (33)

    parsed(5) should === (Map("amount" -> "1", "currency" -> "DKK", "rate" -> "3,631"))
  }

  it should "deserialize csv with double serialized data" in {
    val data: String = """
    "Platnost od:","12.8.2016" ,""," "," ","Devizy"," "," ","Valuty"," "," ","ÈNB",
    "Zemì","Jednotka","Mìna","Zmìna [%]", ,"Nákup","Prodej","Støed","Nákup","Prodej","Støed","Støed",
    Austrálie,"1","AUD","-0.05","-","18.214","19.148","18.681","18.03","19.33","18.681","18.686",
    Bulharsko,"1","BGN","0.05","+","13.474","14.165","13.82","-","-","-","13.813",
    Kanada,"1","CAD","0.29","+","18.122","19.052","18.587","17.94","19.24","18.587","18.562",
    Švýcarsko,"1","CHF","0.41","+","24.26","25.504","24.882","24.16","25.6","24.882","24.884",
    Dánsko,"1","DKK","0.06","+","3.543","3.724","3.634","3.51","3.76","3.634","3.632",
    EU,"1","EUR","0.04","+","26.354","27.706","27.03","26.3","27.76","27.03","27.02",
    Velká Británie,"1","GBP","-0.35","-","30.614","32.184","31.399","30.46","32.34","31.399","31.403",
    Hong Kong,"1","HKD","0.13","+","3.043","3.199","3.121","-","-","-","3.123",
    Chorvatsko,"1","HRK","-0.06","-","3.518","3.698","3.608","3.52","3.72","3.608","3.604",
    Maïarsko,"100","HUF","0.08","+","8.495","8.93","8.713","8.49","8.97","8.713","8.709",
    Japonsko,"100","JPY","0.1","+","23.309","24.504","23.907","23.07","24.74","23.907","23.903",
    Norsko,"1","NOK","0.44","+","2.866","3.013","2.94","2.84","3.04","2.94","2.927",
    Nový Zéland,"1","NZD","0.34","+","17.12","17.998","17.559","-","-","-","17.559",
    Polsko,"1","PLN","0.03","+","6.183","6.5","6.342","6.16","6.53","6.342","6.342",
    Rumunsko,"1","RON","0.07","+","5.912","6.216","6.064","-","-","-","6.058",
    Rusko,"100","RUB","-0.23","-","35.958","38.954","37.456","-","-","-","37.339",
    Švédsko,"1","SEK","0.49","+","2.8","2.944","2.872","2.77","2.97","2.872","2.865",
    Tunisko,"1","TND","0.17","+","10.7","11.249","10.974","-","-","-","-",
    Turecko,"1","TRY","0.12","+","7.979","8.388","8.184","-","-","-","8.176",
    USA,"1","USD","0.1","+","23.599","24.809","24.204","23.5","24.91","24.204","24.224",
    JAR,"1","ZAR","0.05","+","1.775","1.866","1.821","-","-","-","1.813",
    """

    val mapping = Map(
      2 -> "amount",
      3 -> "currency",
      6 -> "buy_deviza",
      7 -> "sell_deviza",
      9 -> "buy_valuta",
      10 -> "sell_valuta"
    )

    val parsed:List[Map[String, String]] = read(data, ',', mapping)

    parsed should have size (23)

    parsed(6) should === (Map(
      "amount" -> "1",
      "currency" -> "DKK",
      "buy_deviza" -> "3.543",
      "sell_deviza" -> "3.724",
      "buy_valuta" -> "3.51",
      "sell_valuta" -> "3.76"
    ))
  }

}