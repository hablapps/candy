package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyLogic { this: CandyOptics with CandyState with CandyUtils =>

  def react(spark: (Pos, Dir)): State[Game, Unit] =
    for {
      // morph (bomb) + crushWith + score (can't be determined by the state)
      _ <- stabilize
      // are we finish?
    } yield ()

  def stabilize: State[Game, Unit] =
    for {
      // crush + score
      _ <- gravity
      // populate
      _ <- nonStabilized.ifM_(stabilize)
    } yield ()

  def nonStabilized: State[Game, Boolean] =
    gets(inarowTr(3).length(_) != 0)

  def swap(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      mx <- gets(matrixLn.get)
      _  <- modify(candyLn(from).set(mx.lookup(from.move(dir))))
      _  <- modify(candyLn(from.move(dir)).set(mx.lookup(from)))
    } yield ()

  def switch(from: Pos, dir: Dir): State[Game, Boolean] =
    for {
      mx <- gets(matrixLn.get)
      _  <- swap(from, dir)
      b1 = ((mx.lookup(from) |@| mx.lookup(from.move(dir))) {
        case (ColourBomb, _) | (_, ColourBomb) => true
        case _ => false
      }).getOrElse(false)
      b2 <- nonStabilized
      _  <- swap(from, dir).whenM(! (b1 || b2))
    } yield b1 || b2

  def gravity: State[Game, Unit] =
    for {
      h <- gets(heightLn.get)
      _ <- modify(gravityTr(h).modify(kv => (kv._1.down, kv._2)))
             .whileM_(gets(gravityTr(h).length(_) != 0))
    } yield ()

  def stripeKind(
      kind: RegularCandy)(
      f: RegularCandy => StripedCandy): State[Game, Unit] =
    modify(kindTr(kind).modify(kv => (kv._1, kv._2.map(_.morph(f)))))

  // TODO: it's not only about putting Nones, think of crushing striped candy
  def crushWith(tr: Traversal[Game, (Pos, Option[Candy])]): State[Game, Unit] =
    modify(tr.modify(kv => (kv._1, None)))

  def crushKind(kind: RegularCandy): State[Game, Unit] = crushWith(kindTr(kind))

  def crushLine(i: Int): State[Game, Unit] = crushWith(lineTr(i))

  def crushColumn(j: Int): State[Game, Unit] = crushWith(columnTr(j))

  def crushMin(n: Int): State[Game, Unit] = crushWith(inarowTr(n))

  def crushAll: State[Game, Unit] = crushWith(allTr)
}
