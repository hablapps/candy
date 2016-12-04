package org.hablapps.candy

import scalaz._, Scalaz._

import monocle.{ Iso, Lens, Traversal }
import monocle.function.At._
import monocle.std.map._

trait CandyOptics { this: CandyState with CandyUtils =>

  /* candy-specific */

  val boardLn: Lens[Game, Board] =
    Game.current ^|-> Level.board

  val heightLn: Lens[Game, Int] =
    boardLn ^|-> Board.height

  val matrixLn: Lens[Game, Pos ==>> Candy] =
    boardLn ^|-> Board.matrix

  def candyLn(pos: Pos): Lens[Game, Option[Candy]] =
    matrixLn ^<-> map2mapzIso[Pos, Candy].reverse ^|-> at(pos)

  def kindTr(kind: RegularCandy): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectTr((_, c) => c.hasKind(kind))

  def lineTr(i: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectTr((p, _) => p.i == i)

  def columnTr(j: Int): Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectTr((p, _) => p.j == j)

  def gravityTr(height: Int): Traversal [Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectCtxTr(mx => {
      case (p, _) => p.i < height &&
        (p.i to height).exists(i => mx.notMember(Pos(i, p.j)))
    })

  def inarowTr(n: Int): Traversal [Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectCtxTr { mx => (p, c) =>
      def check(f: Pos => Pos): Int =
        iterateWhile(p)(f, mx.lookup(_).fold(false)(_ == c)).size
      (check(_.left) + check(_.right) > n) || (check(_.up) + check(_.down) > n)
    }

  val allTr: Traversal[Game, (Pos, Option[Candy])] =
    matrixLn ^|->> selectTr((_, _) => true)
}
