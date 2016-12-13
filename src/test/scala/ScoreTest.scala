package org.hablapps.candy
package test

import scala.util.Random

import org.scalatest._

import scalaz._, Scalaz._

import monocle.Lens
import monocle.state.all._
import monocle.macros.Lenses

class ScoreTest extends FlatSpec with Matchers {

  /* Data Structures */

  @Lenses case class Game(
    // ...
    current: Level)

  @Lenses case class Level(
    // ...
    currentScore: Long)

  /* Optics */

  val currentScoreLn: Lens[Game, Long] =
    Game.current ^|-> Level.currentScore

  /* Logic */

  def score(n: Int): State[Game, Unit] =
    currentScoreLn.mod_(_ + (n * 20))

  "Candy Crush" should "increase score" in {
    val n = 3
    val s = n * 20
    val zero: Game = new Game(new Level(0))

    val p: State[Game, Unit] = score(n)

    p.exec(zero) shouldBe currentScoreLn.modify(_ + s)(zero)
  }
}
