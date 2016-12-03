package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyLogic { this: CandyOptics with CandyState =>

  def fall: State[Game, Unit] =
    for {
      h <- gets(heightLn.get)
      _ <- modify(gravityTr(h).modify(kv => (kv._1.down, kv._2)))
    } yield ()

  def crushWith(tr: Traversal[Game, (Pos, Option[Candy])]): State[Game, Unit] =
    modify(tr.modify(kv => (kv._1, None)))

  def crushKind(kind: Candy): State[Game, Unit] = crushWith(kindTr(kind))

  def crushLine(i: Int): State[Game, Unit] = crushWith(lineTr(i))

  def crushColumn(j: Int): State[Game, Unit] = crushWith(columnTr(j))

  def crushThree: State[Game, Unit] = crushWith(inarowTr(3))
}
