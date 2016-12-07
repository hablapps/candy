package org.hablapps.candy
package test

import scala.util.Random

import org.scalatest._

import scalaz._, Scalaz._

import monocle._

class CandyTest extends FlatSpec with Matchers {

  val system: CandyCrush = new CandyCrush
  import system._

  val gen = unfold(RNG.simple(0))(rng => rng.nextInt.some)
  val board = Board(9, 9, gen.map(RegularCandy.fromInt), ==>>.empty)
  val level = Level(2000, 50, board)
  val zero: Game = Game("jesus", 3, _ => level, level)

  "Candy Crush" should "start playing when idle" in {
    val p: State[Game, Boolean] = play
    val (one, ok) = p(zero)
    ok shouldBe true
    Game.idle.get(one) shouldBe false
  }

  it should "ignore play order when already playing" in {
    val p: State[Game, Boolean] = play
    val (one, ok) = p(Game.idle.set(false)(zero))
    ok shouldBe false
    Game.idle.get(one) shouldBe false
  }
}
