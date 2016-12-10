package org.hablapps.candy
package comparision

object Problem2 {

  type Pos = (Int, Int)

  sealed trait Candy

  sealed trait RegularCandy extends Candy
  case object Red extends RegularCandy
  case object Orange extends RegularCandy
  case object Green extends RegularCandy

  case class StripedCandy(inner: RegularCandy) extends Candy

  object Mutable {

    import scala.collection.mutable.Map

    /* Data Structures */

    case class Game(
      // ...
      var current: Level)

    case class Level(
      // ...
      var board: Board)

    case class Board(
      // ...
      var matrix: Map[Pos, Candy])

    /* Access By Line */

    def getByLine(i: Int)(game: Game): Int =
      game.current.board.matrix.keys.filter(_._1 == i).size

    def stripeRedByLine(i: Int)(game: Game): Unit =
      game.current.board.matrix.foreach {
        case ((`i`, j), Red) => {
          game.current.board.matrix.update((i, j), StripedCandy(Red))
        }
        case _ => ()
      }
  }

  object Immutable {

    /* Data Structures */

    case class Game(
      // ...
      current: Level)

    case class Level(
      // ...
      board: Board)

    case class Board(
      // ...
      matrix: Map[Pos, Candy])

    /* Access By Line */

    def getByLine(i: Int)(game: Game): Int =
      game.current.board.matrix.count(_._1 == i)

    def stripeRedByLine(i: Int)(game: Game): Game =
      game.copy(current =
        game.current.copy(board =
          game.current.board.copy(matrix =
            game.current.board.matrix.map {
              case ((`i`, j), Red) => (i, j) -> StripedCandy(Red)
              case entry => entry
            })))
  }

  object Optics {

    import monocle._
    import monocle.function.FilterIndex._
    import monocle.macros.Lenses
    import monocle.std.map._

    /* Data Structures */

    @Lenses case class Game(
      // ...
      current: Level)

    @Lenses case class Level(
      // ...
      board: Board)

    @Lenses case class Board(
      // ...
      matrix: Map[Pos, Candy])

    import Game._, Level._, Board._

    /* Access By Line */

    def byLineTr(i: Int): Traversal[Game, Candy] =
      current ^|-> board ^|-> matrix ^|->> filterIndex((p: Pos) => p._1 == i)

    def getByLine(i: Int): Game => Int = byLineTr(i).length

    def stripeRedByLine(i: Int): Game => Game =
      byLineTr(i).modify {
        case Red => StripedCandy(Red)
        case other => other
      }
  }
}
