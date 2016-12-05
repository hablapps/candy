package org.hablapps.candy

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import monocle.Getter

import CandyCrush._

object Main extends App {

  val switchPat =
    """\s*switch\s+\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)\s+(up|down|left|right)\s*""".r

  def loop: State[Game, Unit] = {
    print("candy> ")
    readLine match {
      case "exit" => ().point[State[Game, ?]]
      case "" => loop
      case switchPat(i, j, s) => {
        val pos = Pos(i.toInt, j.toInt)
        val dir = s match {
          case "up" => Up
          case "down" => Down
          case "left" => Left
          case "right" => Right
        }
        switch(pos, dir).ifM(
          stabilize >> (showGame >> loop),
          { println(s"invalid switch: '$pos -> $dir'"); loop })
      }
      case wrong => println(s"unknown order: '$wrong'"); loop
    }
  }

  def showGame: State[Game, Unit] =
    for {
      h  <- gets(heightLn.get)
      w  <- gets(widthLn.get)
      mx <- gets(matrixLn.get)
      ts <- gets(targetScoreLn.get)
      cs <- gets(currentScoreLn.get)
      tm <- gets(targetMovesLn.get)
      cm <- gets(currentMovesLn.get)
      _ = {
        println()
        print("   ")
        println((1 to h).mkString("    "))
        println()
        (1 to h) foreach { i =>
          print(s"$i ")
          print(((1 to w) map { j =>
            mx.lookup(Pos(i, j)).fold(" - ")(_.toIcon)
          }).mkString(" "))
          println(); println()
        }
        println(s"# Score: ($cs / $ts)")
        println(s"# Moves: ($cm / $tm)")
        println()
      }
    } yield ()

  import scala.util.Random

  val gen = unfold(new Random())(rnd => (rnd.nextInt, rnd).some)
  val board = Board(8, 8, gen.map(RegularCandy.fromInt), ==>>.empty)
  val level = Level(300, 30, board)
  val game = Game("jesus", 3, level)

  println("""
      |   _____                _          _____                _
      |  / ____|              | |        / ____|              | |
      | | |     __ _ _ __   __| |_   _  | |     _ __ _   _ ___| |__
      | | |    / _` | '_ \ / _` | | | | | |    | '__| | | / __| '_ \
      | | |___| (_| | | | | (_| | |_| | | |____| |  | |_| \__ \ | | |
      |  \_____\__,_|_| |_|\__,_|\__, |  \_____|_|   \__,_|___/_| |_|
      |                           __/ |
      |                          |___/
      |""".stripMargin)

  (stabilize >> (showGame >> loop)).run(game)
}
