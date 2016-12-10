package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.macros.Lenses

trait CandyState { this: CandyUtils =>

  @Lenses case class Game(
    user: String,
    ups: Int,
    levels: Int => Level,
    current: Level,
    idle: Boolean = true,
    last: Int = 0)

  @Lenses case class Level(
    targetScore: Long,
    targetMoves: Int,
    board: Board,
    currentScore: Long = 0,
    currentMoves: Int = 0)

  @Lenses case class Board(
    height: Int,
    width: Int,
    rng: RNG,
    matrix: Map[Pos, Candy])

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
      def shareKind(other: Candy): Boolean =
        (candy.kind |@| other.kind)(_ == _).getOrElse(false)
      def hasKind(kind: RegularCandy): Boolean =
        candy.kind.fold(false)(_ == kind)
      def morph(f: RegularCandy => StripedCandy): Candy = candy match {
        case _: StripedCandy | ColourBomb => candy
        case c: RegularCandy => f(c)
      }
      import Colour._
      def ansiColour: String = candy match {
        case Red => ANSI_RED
        case Orange => ANSI_YELLOW
        case Yellow => ANSI_GREEN
        case Green => ANSI_CYAN
        case Blue => ANSI_BLUE
        case Purple => ANSI_PURPLE
        case HorStriped(c) => c.ansiColour
        case VerStriped(c) => c.ansiColour
        case _ => ""
      }
      def toIcon: String = candy match {
        case Red => "🍅"
        case Orange => "🍌"
        case Yellow => "🍋"
        case Green => "🍒"
        case Blue => "🍍"
        case Purple => "🍓"
        case ColourBomb => "🍪"
        case HorStriped(c) => s"🢐${c.toIcon} 🢒"
        case VerStriped(c) => s"🢓${c.toIcon} 🢑"
      }
      def kind: Option[RegularCandy] = candy match {
        case HorStriped(candy) => candy.some
        case VerStriped(candy) => candy.some
        case ColourBomb => Option.empty
        case regular: RegularCandy => regular.some
      }
    }
  }

  object KindedCandy {
    implicit class KindedCandyAux(candy: KindedCandy) {
      def kind: RegularCandy = candy match {
        case k: RegularCandy => k
        case HorStriped(k) => k
        case VerStriped(k) => k
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

  sealed trait SwitchOut
  case object NotPlaying extends SwitchOut
  case object InvalidMove extends SwitchOut
  case object YouLose extends SwitchOut
  case object YouWin extends SwitchOut
  case object Ok extends SwitchOut
}
