package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyLogic { this: CandyOptics with CandyState =>

  def isReactionTrigger: State[Game, Boolean] =
    gets(inarowTr(3).length(_) != 0)

  def swap(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      mx <- gets(matrixLn.get)
      _  <- modify(candyLn(from).set(mx.lookup(from.move(dir))))
      _  <- modify(candyLn(from.move(dir)).set(mx.lookup(from)))
    } yield ()

  def slide(from: Pos, dir: Dir): State[Game, Boolean] =
    for {
      mx <- gets(matrixLn.get)
      _  <- swap(from, dir)
      b1 = ((mx.lookup(from) |@| mx.lookup(from.move(dir))) {
        case (ColourBomb, _) | (_, ColourBomb) => true
        case _ => false
      }).getOrElse(false)
      b2 <- isReactionTrigger
      _  <- swap(from, dir).whenM(! (b1 || b2))
    } yield b1 || b2

  def fall: State[Game, Unit] =
    for {
      h <- gets(heightLn.get)
      _ <- modify(gravityTr(h).modify(kv => (kv._1.down, kv._2)))
    } yield ()

  // TODO: it's not only about putting Nones, think of crushing striped candy
  def crushWith(tr: Traversal[Game, (Pos, Option[Candy])]): State[Game, Unit] =
    modify(tr.modify(kv => (kv._1, None)))

  def crushKind(kind: SimpleCandy): State[Game, Unit] = crushWith(kindTr(kind))

  def crushLine(i: Int): State[Game, Unit] = crushWith(lineTr(i))

  def crushColumn(j: Int): State[Game, Unit] = crushWith(columnTr(j))

  def crushThree: State[Game, Unit] = crushWith(inarowTr(3))
}
