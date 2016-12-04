package org.hablapps.candy

import scalaz._, Scalaz._

import monocle.{ Iso, Lens, Traversal }
import monocle.function.At._
import monocle.std.map._

trait CandyOptics { this: CandyState =>

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

  /* generic */

  def map2mapzIso[K: Order, V]: Iso[Map[K, V], K ==>> V] =
    Iso[Map[K, V], K ==>> V](xs => ==>>.fromList(xs.toList))(_.toList.toMap)

  // XXX: check traversal laws!
  def selectCtxTr[K: Order, V](p: K ==>> V => (K, V) => Boolean) =
    new Traversal[K ==>> V, (K, Option[V])] {
      def modifyF[F[_]: Applicative](
          f: ((K, Option[V])) => F[(K, Option[V])])(
          s: K ==>> V): F[K ==>> V] =
        s.fold[F[K ==>> V]](==>>.empty.pure[F]) { (k, v, acc) =>
          val fv = if (p(s)(k, v)) f(k, v.some) else (k, v.some).pure[F]
          (acc |@| fv) {
            case (s2, (k2, Some(v))) => s2 + (k2 -> v)
            case (s2, (k2, None)) => s2 - k2
          }
        }
    }

  def selectTr[K: Order, V](p: (K, V) => Boolean) =
    selectCtxTr[K, V](_ => (k, v) => p(k, v))

  // XXX: not in scalaz?
  private def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil
}
