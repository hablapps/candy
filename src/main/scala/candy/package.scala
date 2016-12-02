package org.hablapps

package object `candy` {

  type Position = (Int, Int)

  type Table = List[(Option[Candy], Position)]
}
