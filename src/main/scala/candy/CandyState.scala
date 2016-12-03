package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.macros.Lenses

trait CandyState {

  @Lenses case class Game(
    user: String,
    ups: Int,
    current: Level)

  @Lenses case class Level(
    targetScore: Long,
    moves: Int,
    board: Board,
    currentScore: Long = 0,
    currentMoves: Int = 0)

  @Lenses case class Board(
    width: Int,
    height: Int,
    zero: Pos ==>> Candy,
    matrix: Pos ==>> Candy)

  sealed trait Candy
  case class HorStriped(candy: PrimitiveCandy) extends Candy
  case class VerStriped(candy: PrimitiveCandy) extends Candy
  case object ColourBomb extends Candy
  sealed trait PrimitiveCandy extends Candy
  case object Red extends PrimitiveCandy
  case object Orange extends PrimitiveCandy
  case object Yellow extends PrimitiveCandy
  case object Green extends PrimitiveCandy
  case object Blue extends PrimitiveCandy
  case object Purple extends PrimitiveCandy

  case class Pos(i: Int, j: Int) {
    def down: Pos = Pos(i + 1, j)
    def up: Pos = Pos(i - 1, j)
    def left: Pos = Pos(i, j - 1)
    def right: Pos = Pos(i, j + 1)
  }

  object Pos {
    implicit def orderInstance: Order[Pos] =
      Order.orderBy[Pos, (Int, Int)](p => (p.i, p.j))
  }
}
