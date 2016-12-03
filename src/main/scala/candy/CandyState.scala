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

  sealed trait Dir
  case object Up extends Dir
  case object Down extends Dir
  case object Left extends Dir
  case object Right extends Dir

  case class Pos(i: Int, j: Int) {
    def move(dir: Dir): Pos = dir match {
      case Up => Pos(i - 1, j)
      case Down => Pos(i + 1, j)
      case Left => Pos(i, j - 1)
      case Right => Pos(i, j + 1)
    }
    def down: Pos = move(Down)
    def up: Pos = move(Up)
    def left: Pos = move(Left)
    def right: Pos = move(Right)
  }

  object Pos {
    implicit def orderInstance: Order[Pos] =
      Order.orderBy[Pos, (Int, Int)](p => (p.i, p.j))
  }
}
