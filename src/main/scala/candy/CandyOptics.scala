package org.hablapps.candy

import scalaz._, Scalaz._

import monocle.{ Iso, Lens, Traversal }
import monocle.function.At._
import monocle.std.map._

trait CandyOptics { this: CandyState with CandyUtils =>

  val boardLn: Lens[Game, Board] =
    Game.current ^|-> Level.board

  val targetScoreLn: Lens[Game, Long] =
    Game.current ^|-> Level.targetScore

  val currentScoreLn: Lens[Game, Long] =
    Game.current ^|-> Level.currentScore

  val targetMovesLn: Lens[Game, Int] =
    Game.current ^|-> Level.targetMoves

  val currentMovesLn: Lens[Game, Int] =
    Game.current ^|-> Level.currentMoves

  val heightLn: Lens[Game, Int] =
    boardLn ^|-> Board.height

  val widthLn: Lens[Game, Int] =
    boardLn ^|-> Board.width

  val matrixLn: Lens[Game, Map[Pos, Candy]] =
    boardLn ^|-> Board.matrix

  def rngLn: Lens[Game, RNG] =
    boardLn ^|-> Board.rng

  def candyLn(pos: Pos): Lens[Game, Option[Candy]] =
    matrixLn ^|-> at(pos)

  def kindTr(kind: RegularCandy)(h: Int, w: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilter(allPos(h, w): _*)((_, oc) => oc.fold(false)(_.hasKind(kind)))

  def lineTr(i: Int)(h: Int, w: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilter(allPos(h, w): _*)((p, _) => p.i == i)

  def columnTr(j: Int)(h: Int, w: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilter(allPos(h, w): _*)((p, _) => p.j == j)

  def gravityTr(h: Int, w: Int): Traversal [Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilterCtx(allPos(h, w): _*)(mx => {
      case (p, oc) => oc.isDefined && p.i < h &&
        ((p.i + 1) to h).exists(i => ! mx.isDefinedAt(Pos(i, p.j)))
    })

  def inarowTr(n: Int)(h: Int, w: Int): Traversal [Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilterCtx(allPos(h, w): _*) { mx => (p, oc) =>
      def check(f: Pos => Pos): Int =
        iterateWhile(p)(f, pos => oc.fold(false)(c => mx.get(pos).fold(false)(_.shareKind(c)))).size
      (check(_.left) + check(_.right) > n) || (check(_.up) + check(_.down) > n)
    }

  def allTr(h: Int, w: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> multiAtFilter(allPos(h, w): _*)((_, _) => true)
}
