package aoc2024

import scala.io.Source
import scala.util.Using

object Day22 extends App {

  val input = Using(Source.fromFile("input/2024/22.txt")) {
    _.getLines().toSeq
  }.get

  val buyers = input.map(_.toLong)

  def getSecret(count: Int, secret: Long): Long = {
    if (count == 0) secret
    else {
      val step1 = (secret ^ (secret * 64)) % 16777216
      val step2 = (step1 ^ (step1 / 32)) % 16777216
      val step3 = (step2 ^ (step2 * 2048)) % 16777216
      getSecret(count - 1, step3)
    }
  }

  def getSecrets(count: Int, secret: Long, secrets: Seq[Long]): Seq[Long] = {
    if (count == 0) secrets
    else {
      val newSecret = getSecret(1, secret)
      getSecrets(count - 1, newSecret, secrets :+ newSecret)
    }
  }


  val part1 = buyers.map(b => getSecret(2000, b)).sum
  println(part1)

  var in = 0
  val prices = buyers.map(b => {
    println(buyers.size - in)
    in = in + 1
    b % 10 +: getSecrets(2000, b, Seq.empty).map(_ % 10)
  })


  in = 0
  val priceDiffs = prices.map(bPrices => {
    println(prices.size - in)
    in = in + 1
    bPrices.indices.tail.map(i => bPrices(i) - bPrices(i - 1))
  })

  val priceDiffMap =
  priceDiffs.indices.map(h => {
    println("seqs " + (priceDiffs.length - h))
    val map = collection.mutable.Map.empty[Seq[Long], Long]
    priceDiffs(h).indices.take(2000 - 3).foreach(i => {
      val key = (i to i + 3).map(k => priceDiffs(h)(k))
      if (!map.contains(key))
        map(key) = prices(h)(i + 4)
    })
    map
  })

  val seqs = priceDiffMap.flatMap(_.keys).distinct

  in = 0
  val bananas = seqs.map(seq => {
    println(seqs.size - in)
    in = in + 1
    val banana = priceDiffMap.map(_.getOrElse(seq, 0L))
    (seq, banana.sum)
  })

  // -2, 1, -1, 3
  println(bananas.mkString("\n"))
  println(bananas.maxBy(_._2))

}
