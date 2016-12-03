package org.hablapps.candy

import scalaz._, Scalaz._

import monocle._
import monocle.macros.Lenses

@Lenses case class Game(
  userName: String,
  ups: Int,
  current: Int,
  levels: Map[Int, Level])

@Lenses case class Level(
  target: Long,
  moves: Int,
  board: Board)

@Lenses case class Board(
  width: Int,
  height: Int,
  matrix: Pos ==>> Candy)

sealed trait Candy
case class HorStriped(candy: PrimitiveCandy) extends Candy
case class VerStriped(candy: PrimitiveCandy) extends Candy
case object ColourBomb extends Candy
sealed trait PrimitiveCandy extends Candy
case object Red extends PrimitiveCandy
case object Orange extends PrimitiveCandy
case object Yellow extends PrimitiveCandy
case object Green extends PrimitiveCandy
case object Blue extends PrimitiveCandy
case object Purple extends PrimitiveCandy

object CandyCrush extends App {

  // optic utils

  // XXX: check traversal laws!
  def predCtxTraversal[K: Order, V](p: K ==>> V => (K, V) => Boolean) =
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

  def predTraversal[K: Order, V](p: (K, V) => Boolean) =
    predCtxTraversal[K, V](_ => (k, v) => p(k, v))

  // candy-specific optics

  def kindTraversal(
      kind: Candy): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTraversal((_, c) => c == kind)

  def lineTraversal(i: Int): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTraversal((p, _) => p._1 == i)

  def columnTraversal(j: Int): Traversal[Pos ==>> Candy, (Pos, Option[Candy])] =
    predTraversal((p, _) => p._2 == j)

  def gravityTraversal(
      height: Int): Traversal [Pos ==>> Candy, (Pos, Option[Candy])] =
    predCtxTraversal(mx => {
      case ((i, j), _) => i < height && (i to height).exists(mx.notMember(_, j))
    })

  // XXX: not in scalaz?
  private def iterateWhile[A](a: A)(f: A => A, p: A => Boolean): List[A] =
    if (p(a)) a :: iterateWhile(f(a))(f, p) else Nil

  def inarowTraversal(
      n: Int): Traversal [Pos ==>> Candy, (Pos, Option[Candy])] =
    predCtxTraversal { mx => (p, c) =>
      def check(f: Pos => Pos): Int =
        iterateWhile(p)(f, mx.lookup(_).fold(false)(_ == c)).size
      (check(left) + check(right) > n) || (check(up) + check(down) > n)
    }

  // logic (without `State` capabilities)

  def down(pos: Pos): Pos = (pos._1 + 1, pos._2)
  def up(pos: Pos): Pos = (pos._1 - 1, pos._2)
  def left(pos: Pos): Pos = (pos._1, pos._2 - 1)
  def right(pos: Pos): Pos = (pos._1, pos._2 + 1)

  def fall(board: Board): Board =
    (Board.matrix ^|->> gravityTraversal(board.height))
      .modify(kv => (down(kv._1), kv._2))(board)

  def crushWith(
      tr: Traversal[Pos ==>> Candy, (Pos, Option[Candy])])(
      mx: Pos ==>> Candy): Pos ==>> Candy =
    tr.modify(kv => (kv._1, None))(mx)

  def crushKind(kind: Candy)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(kindTraversal(kind))(mx)

  def crushLine(i: Int)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(lineTraversal(i))(mx)

  def crushColumn(j: Int)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(columnTraversal(j))(mx)

  def crushThree(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(inarowTraversal(3))(mx)

  // sandbox

  val matrix: Pos ==>> Candy =
    ==>>((1, 1) -> Red, (1, 2) -> Red, (1, 3) -> Red, (1, 4) -> Blue)

  val matrix2 = crushThree(matrix)
  println(matrix2.toList.toMap)

  println("Hello Candy 🍌 🍇 !")
}
