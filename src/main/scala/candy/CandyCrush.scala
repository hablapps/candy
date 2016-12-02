package org.hablapps.candy

import monocle._

case class Game(
  userName: String,
  ups: Int,
  current: Int,
  levels: Map[Int, Level])

case class Level(
  target: Long,
  moves: Int,
  board: Board)

case class Board(
  width: Int,
  height: Int,
  candy: List[Table])

sealed trait Candy
case object Red extends Candy
case object Blue extends Candy
case object Green extends Candy
case object Yellow extends Candy
case object Orange extends Candy
case object Purple extends Candy

object CandyCrush extends App {
  println("Hello Candy!")
}
