package org.hablapps.candy
package comparision

object Problem3 {

  class Game

  object Mutable {

    /* Data Transformations */

    var game: Game = new Game

    def crush(): Int = {
      // ...
      var n = 3
      game = ???
      n
    }

    def score(n: Int): Unit = {
      // ...
      game = ???
    }

    def gravity(): Unit = {
      // ...
      game = ???
    }

    def populate(): Unit = {
      // ...
      game = ???
    }

    def notStabilized(): Boolean = {
      // ...
      var b = ???
      b
    }

    /* Composing Transformations... */

    def stabilize: Unit = {
      score(crush)
      gravity
      populate
      if (notStabilized) stabilize
    }
  }

  object Immutable {

    /* Data Transformations */

    def crush(game: Game): (Game, Int) =
      ???

    def score(n: Int)(game: Game): Game =
      ???

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

    /* Data Transformations */

    def crush: State[Game, Int] =
      for {
        game <- get[Game]
        // ...
        n = 3
        _ <- put(new Game)
      } yield n

    def score(n: Int): State[Game, Unit] =
      ???

    def gravity: State[Game, Unit] =
      ???

    def populate: State[Game, Unit] =
      ???

    def notStabilized(g: Game): Boolean =
      ???

    /* Composing Transformations... */

    def stabilize: State[Game, Unit] =
      for {
        _ <- crush >>= score
        _ <- gravity
        _ <- populate
        g <- get
        _ <- stabilize.whenM(notStabilized(g))
      } yield ()
  }
}
