package org.hablapps.candy

import scalaz._, Scalaz._
import monocle._

trait CandyUtils { this: CandyState =>

  def allPos(h: Int, w: Int): List[Pos] =
    cartesian(h, w).map(ia => Pos(ia._1, ia._2))

  /* scala */

  object Colour {
    val ANSI_RESET = "\u001B[0m"
    val ANSI_BLACK = "\u001B[30m"
    val ANSI_RED = "\u001B[31m"
    val ANSI_GREEN = "\u001B[32m"
    val ANSI_YELLOW = "\u001B[33m"
    val ANSI_BLUE = "\u001B[34m"
    val ANSI_PURPLE = "\u001B[35m"
    val ANSI_CYAN = "\u001B[36m"
    val ANSI_WHITE = "\u001B[37m"
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                    ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
         simple(seed2))
      }
    }
  }

  def cartesian(h: Int, w: Int): List[(Int, Int)] =
    (for {
      i <- 1 to h
      j <- 1 to w
    } yield (i, j)).toList

  /* scalaz */

  // XXX: inference problems when generalizing to `M[_]`
  implicit class IfMHelper(mb: State[Game, Boolean]) {
    def ifM_(mu: State[Game, Unit]): State[Game, Unit] =
      mb.ifM(mu, ().point[State[Game, ?]])
  }

  // XXX: not in scalaz?
  def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil

  def iterateN[M[_]: Monad, A](m: M[A], n: Int): M[List[A]] =
    if (n > 0) m >>= (a => iterateN(m, n-1).map(a :: _)) else List.empty.point[M]

  /* monocle */

  import monocle.function.At, At._

  def multiAtFilterCtx[I: Order, A](
      is: I*)(
      p: Map[I, A] => (I, Option[A]) => Boolean): Traversal[Map[I, A], (I, Option[A])] =
    new Traversal[Map[I, A], (I, Option[A])] {
      def modifyF[F[_]: Applicative](
          f: ((I, Option[A])) => F[(I, Option[A])])(
          s: Map[I, A]): F[Map[I, A]] =
        is.toList.foldLeft[F[Map[I, A]]](Map.empty.pure[F]) { (acc, i) =>
          val fv = if (p(s)(i, s.get(i))) f((i, s.get(i))) else (i, s.get(i)).pure[F]
          (acc |@| fv) {
            case (s2, (i2, Some(a2))) => s2 + (i2 -> a2)
            case (s2, _) => s2
          }
        }
    }

  def multiAtFilter[I: Order, A](
      is: I*)(
      p: (I, Option[A]) => Boolean): Traversal[Map[I, A], (I, Option[A])] =
    multiAtFilterCtx[I, A](is: _*)(_ => (i, oa) => p(i, oa))
}
