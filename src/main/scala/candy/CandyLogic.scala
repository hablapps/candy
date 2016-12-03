package org.hablapps.candy

import scalaz._, Scalaz._
import monocle.{ Lens, Traversal }

trait CandyLogic { this: CandyOptics with CandyState =>

  def fall(board: Board): Board =
    (Board.matrix ^|->> gravityTr(board.height))
      .modify(kv => (kv._1.down, kv._2))(board)

  def crushWith(
      tr: Traversal[Pos ==>> Candy, (Pos, Option[Candy])])(
      mx: Pos ==>> Candy): Pos ==>> Candy =
    tr.modify(kv => (kv._1, None))(mx)

  def crushKind(kind: Candy)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(kindTr(kind))(mx)

  def crushLine(i: Int)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(lineTr(i))(mx)

  def crushColumn(j: Int)(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(columnTr(j))(mx)

  def crushThree(mx: Pos ==>> Candy): Pos ==>> Candy =
    crushWith(inarowTr(3))(mx)
}
