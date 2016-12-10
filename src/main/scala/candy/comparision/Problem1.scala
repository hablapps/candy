package org.hablapps.candy.comparision

object Problem1 {

  type Pos = (Int, Int)
  class Candy

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

    /* Access Methods (notice `Unit`s) */

    def getMatrix(game: Game): Map[Pos, Candy] =
      game.current.board.matrix

    def setMatrix(matrix2: Map[Pos, Candy])(game: Game): Unit =
      game.current.board.matrix = matrix2

    // XXX: unnatural method in OO
    def modifyMatrix(f: Map[Pos, Candy] => Unit)(game: Game): Unit =
      f(game.current.board.matrix)
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
      matrix: Map[Pos, Candy],
      width: Int)

    /* Access Methods */

    def getMatrix(game: Game): Map[Pos, Candy] =
      game.current.board.matrix

    def setMatrix(matrix2: Map[Pos, Candy])(game: Game): Game =
      game.copy(current =
        game.current.copy(board =
          game.current.board.copy(matrix =
            matrix2)))

    def modifyMatrix(f: Map[Pos, Candy] => Map[Pos, Candy])(game: Game): Game =
      game.copy(current =
        game.current.copy(board =
          game.current.board.copy(matrix =
            f(game.current.board.matrix))))

    def modifyWidth(f: Int => Int)(game: Game): Game =
      game.copy(current =
        game.current.copy(board =
          game.current.board.copy(width =
            f(game.current.board.width))))
  }

  object Lenses {

    import monocle.macros.Lenses

    /* Data Structures */

    @Lenses case class Game(
      // ...
      current: Level)

    // val current: Lens[Game, Level] =
    //   Lens[Game, Level](_.current)(nc => _.copy(current = nc))

    @Lenses case class Level(
      // ...
      board: Board)

    @Lenses case class Board(
      // ...
      matrix: Map[Pos, Candy],
      width: Int)

    import Game._, Level._, Board._

    /* Access Methods */

    def getMatrix(game: Game): Map[Pos, Candy] =
      // game.current.board.matrix
      (current ^|-> board ^|-> matrix).get(game)

    def setMatrix(matrix2: Map[Pos, Candy])(game: Game): Game =
      (current ^|-> board ^|-> matrix).set(matrix2)(game)

    def modifyMatrix(f: Map[Pos, Candy] => Map[Pos, Candy])(game: Game): Game =
      (current ^|-> board ^|-> matrix).modify(f)(game)

    def modifyWidth(f: Int => Int)(game: Game): Game =
      (current ^|-> board ^|-> width).modify(f)(game)
  }
}
