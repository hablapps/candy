package org.hablapps.candy
package comparision

object Problem3 {

  class Game

  object Mutable {

    /* Data Structures */

    case class Game(
      // ...
      var currentScore: Long = 0)

    /* Data Transformations */

    var game: Game = new Game()

    def score(n: Int): Unit =
      game.currentScore += (n * 20)

    def crush(): Int = ???
    def gravity(): Unit = ???
    def populate(): Unit = ???
    def notStabilized(): Boolean = ???

    /* Composing Transformations... */

    def stabilize: Unit = {
      score(crush)
      gravity
      populate
      if (notStabilized) stabilize
    }
  }

  object Immutable {

    import monocle.macros.Lenses

    /* Data Structures */

    @Lenses case class Game(
      // ...
      currentScore: Long)

    import Game.currentScore

    /* Data Transformations */

    def crush(game: Game): (Game, Int) =
      ???

    def score(n: Int)(game: Game): Game =
      currentScore.modify(_ + (n * 20))(game)

    def gravity(game: Game): Game =
      ???

    def populate(game: Game): Game =
      ???

    def notStabilized(game: Game): Boolean =
      ???

    /* Composing Transformations... */

    def stabilize(game: Game): Game = {
      val (g1, n) = crush(game)
      val g2 = score(n)(g1)
      val g3 = gravity(g2)
      val g4 = populate(g3)
      if (notStabilized(g4)) stabilize(g4) else g4
    }
  }

  object StateMonad {

    import scalaz._, Scalaz._
    import monocle.macros.Lenses
    import monocle.state.all._

    /* Data Structures */

    @Lenses case class Game(
      // ...
      currentScore: Long)

    import Game.currentScore

    /* Data Transformations */

    def crush: State[Game, Int] =
      ???

    def score0(n: Int): State[Game, Unit] =
      for {
        s <- get[Game]
        ns = currentScore.modify(_ + (n * 20))(s)
        _ <- put(ns)
      } yield ()

    def score1(n: Int): State[Game, Unit] =
      for {
        _ <- modify(currentScore.modify(_ + (n * 20)))
      } yield ()

    def score2(n: Int): State[Game, Unit] =
      modify(currentScore.modify(_ + (n * 20)))

    def score(n: Int): State[Game, Unit] =
      currentScore.mod_(_ + (n * 20))

    def gravity: State[Game, Unit] =
      ???

    def populate: State[Game, Unit] =
      ???

    def notStabilized: State[Game, Boolean] =
      ???

    /* Composing Transformations... */

    def stabilize: State[Game, Unit] =
      for {
        _  <- crush >>= score
        _  <- gravity
        _  <- populate
        ns <- notStabilized
        _  <- stabilize.whenM(ns)
      } yield ()
  }
}
