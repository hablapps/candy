package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyLogic { this: CandyOptics with CandyState with CandyUtils =>

  def play: State[Game, Boolean] =
    for {
      ok <- (isIdle |@| nonZeroUps)(_ && _)
      _  <- loadCurrent.whenM(ok)
      _  <- setPlaying
    } yield ok

  def leave: State[Game, Boolean] =
    for {
      ok <- isPlaying
      _  <- modifyUps(_ - 1).whenM(ok)
      _  <- setIdle
    } yield ok

  def switch(from: Pos, dir: Dir): State[Game, SwitchOut] =
    isPlaying.ifM(
      for {
        _   <- swap(from, dir)
        vld <- (isBombInvolved(from, dir) |@| nonStabilized)(_ || _)
        _   <- if (vld) newMove >> stabilize else undo(from, dir)
        win <- checkWinningCondition
        _   <- unlockNextLevel.whenM(win)
        los <- checkLosingCondition
        _   <- modifyUps(_ - 1).whenM(!win && los)
        _   <- setIdle.whenM(win || los)
        ok = if (win) YouWin
             else if (los) YouLose
             else if (vld) Ok
             else InvalidMove
      } yield ok,
      (NotPlaying: SwitchOut).pure[State[Game, ?]])

  private def newMove: State[Game, Unit] =
    modify(currentMovesLn.modify(_ + 1))

  private def unlockNextLevel: State[Game, Unit] =
    modify(Game.last.modify(_ + 1))

  private def checkWinningCondition: State[Game, Boolean] =
    (gets(currentScoreLn.get) |@| gets(targetScoreLn.get))(_ >= _)

  private def checkLosingCondition: State[Game, Boolean] =
    (gets(currentMovesLn.get) |@| gets(targetMovesLn.get))(_ >= _)

  private def modifyUps(f: Int => Int): State[Game, Unit] =
    modify(Game.ups.modify(f))

  private def setIdle: State[Game, Unit] =
    modify(Game.idle.set(true))

  private def setPlaying: State[Game, Unit] =
    modify(Game.idle.set(false))

  private def isIdle: State[Game, Boolean] =
    gets(Game.idle.get)

  private def isPlaying: State[Game, Boolean] =
    isIdle.map(!_)

  private def nonZeroUps: State[Game, Boolean] =
    gets(Game.ups.get).map(_ > 0)

  private def loadCurrent: State[Game, Unit] =
    for {
      _ <- gets(Game.last.get) <*> gets(Game.levels.get)
      _ <- stabilize
      _ <- modify(currentScoreLn.set(0))
    } yield ()

  private def stabilize: State[Game, Unit] =
    for {
      _ <- crush >>= score
      _ <- gravity
      _ <- populate
      _ <- nonStabilized.ifM_(stabilize)
    } yield ()

  private def isBombInvolved(from: Pos, dir: Dir): State[Game, Boolean] =
    for {
      oc1 <- gets(candyLn(from).get)
      oc2 <- gets(candyLn(from.move(dir)).get)
      res = (oc1 |@| oc2) {
        case (ColourBomb, _) | (_, ColourBomb) => true
        case _ => false
      }
    } yield res.getOrElse(false)

  private def nonStabilized: State[Game, Boolean] =
    gets(inarowTr(3).length(_) > 0)

  private def swap(from: Pos, dir: Dir): State[Game, Unit] =
    for {
      mx <- gets(matrixLn.get)
      _  <- modify(candyLn(from).set(mx.lookup(from.move(dir))))
      _  <- modify(candyLn(from.move(dir)).set(mx.lookup(from)))
    } yield ()

  private def undo(from: Pos, dir: Dir): State[Game, Unit] =
    swap(from, dir)

  private def gravity: State[Game, Unit] =
    for {
      h <- gets(heightLn.get)
      _ <- modify(gravityTr(h).modify(kv => (kv._1.down, kv._2)))
             .whileM_(gets(gravityTr(h).length(_) > 0))
    } yield ()

  private def generateCandy(n: Int): State[Game, List[RegularCandy]] =
    for {
      xs <- gets(genLn.get).map(_.take(n).toList)
      _  <- modify(genLn.modify(_.drop(n)))
    } yield xs

  private def populate: State[Game, Unit] =
    for {
      mx <- gets(matrixLn.get)
      h  <- gets(heightLn.get)
      w  <- gets(widthLn.get)
      gaps = cartesian(h, w).map(k => Pos(k._1, k._2)).filter(mx.notMember)
      xs <- generateCandy(gaps.size).map(_.zip(gaps))
      _  <- xs.traverse_[State[Game, ?]](x => modify(candyLn(x._2).set(x._1.some)))
    } yield ()

  private def stripeKind(
      kind: RegularCandy,
      f: RegularCandy => StripedCandy): State[Game, Unit] =
    modify(kindTr(kind).modify(kv => (kv._1, kv._2.map(_.morph(f)))))

  private def score(crushed: Int): State[Game, Unit] =
    modify(currentScoreLn.modify(_ + (crushed * 10)))

  private def crush: State[Game, Int] =
    gets(inarowTr(3).length) >>! (_ => crushMin(3))

  // TODO: it's not only about putting Nones, think of crushing striped candy
  private def crushWith(tr: Traversal[Game, (Pos, Option[Candy])]): State[Game, Unit] =
    modify(tr.modify(kv => (kv._1, None)))

  private def crushKind(kind: RegularCandy): State[Game, Unit] =
    crushWith(kindTr(kind))

  private def crushLine(i: Int): State[Game, Unit] =
    crushWith(lineTr(i))

  private def crushColumn(j: Int): State[Game, Unit] =
    crushWith(columnTr(j))

  private def crushMin(n: Int): State[Game, Unit] =
    crushWith(inarowTr(n))

  private def crushAll: State[Game, Unit] =
    crushWith(allTr)
}
