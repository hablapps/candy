package org.hablapps.candy

import scalaz._, Scalaz._
import monocle._

trait CandyUtils { this: CandyState =>

  /* scalaz */

  implicit class IfMHelper(mb: State[Game, Boolean]) {
    def ifM_(mu: State[Game, Unit]): State[Game, Unit] =
      mb.ifM(mu, ().point[State[Game, ?]])
  }

  // XXX: not in scalaz?
  def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil

  /* monocle */

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
}
