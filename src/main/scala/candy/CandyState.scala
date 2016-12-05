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
    targetMoves: Int,
    board: Board,
    currentScore: Long = 0,
    currentMoves: Int = 0)

  @Lenses case class Board(
    width: Int,
    height: Int,
    gen: Stream[RegularCandy],
    matrix: Pos ==>> Candy)

  sealed trait Candy
  sealed trait KindedCandy extends Candy
  sealed trait StripedCandy extends KindedCandy
  case class HorStriped(candy: RegularCandy) extends StripedCandy
  case class VerStriped(candy: RegularCandy) extends StripedCandy
  case object ColourBomb extends Candy
  sealed trait RegularCandy extends KindedCandy
  case object Red extends RegularCandy
  case object Orange extends RegularCandy
  case object Yellow extends RegularCandy
  case object Green extends RegularCandy
  case object Blue extends RegularCandy
  case object Purple extends RegularCandy

  object Candy {
    implicit class CandyAux(candy: Candy) {
      def hasKind(kind: RegularCandy): Boolean = candy match {
        case HorStriped(candy) => kind == candy
        case VerStriped(candy) => kind == candy
        case ColourBomb => false
        case candy => kind == candy
      }
      def morph(f: RegularCandy => StripedCandy): Candy = candy match {
        case _: StripedCandy | ColourBomb => candy
        case c: RegularCandy => f(c)
      }
      def toIcon: String = candy match {
        case Red => " 🍅  "
        case Orange => " 🍌  "
        case Yellow => " 🍋  "
        case Green => " 🍒  "
        case Blue => " 🍍  "
        case Purple => " 🍓  "
        case ColourBomb => " 🍪  "
        case HorStriped(c) => "🢐" + c.toIcon.trim + " 🢒"
        case VerStriped(c) => "🢓" + c.toIcon.trim + " 🢑"
      }
    }
  }

  object RegularCandy {
    def fromInt(i: Int): RegularCandy = (i % 6).abs match {
      case 0 => Red
      case 1 => Orange
      case 2 => Yellow
      case 3 => Green
      case 4 => Blue
      case 5 => Purple
    }
  }

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
