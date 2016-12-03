package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyOptics { this: CandyState =>

  // candy-specific

  val boardLn: Lens[Game, Board] =
    Game.current ^|-> Level.board

  val matrixLn: Lens[Game, Pos ==>> Candy] =
    boardLn ^|-> Board.matrix

  def kindTr(kind: Candy): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTr((_, c) => c == kind)

  def lineTr(i: Int): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTr((p, _) => p.i == i)

  def columnTr(j: Int): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTr((p, _) => p.j == j)

  def gravityTr(height: Int): Traversal [Pos ==>> Candy, (Pos, Option[Candy])] =
    predCtxTr(mx => {
      case (p, _) => p.i < height &&
        (p.i to height).exists(i => mx.notMember(Pos(i, p.j)))
    })

  // XXX: not in scalaz?
  private def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil

  def inarowTr(
      n: Int): Traversal [Pos ==>> Candy, (Pos, Option[Candy])] =
    predCtxTr { mx => (p, c) =>
      def check(f: Pos => Pos): Int =
        iterateWhile(p)(f, mx.lookup(_).fold(false)(_ == c)).size
      (check(_.left) + check(_.right) > n) || (check(_.up) + check(_.down) > n)
    }

  // generic

  // XXX: check traversal laws!
  def predCtxTr[K: Order, V](p: K ==>> V => (K, V) => Boolean) =
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

  def predTr[K: Order, V](p: (K, V) => Boolean) =
    predCtxTr[K, V](_ => (k, v) => p(k, v))
}
