package tetris.logic

// you can alter this file!

case class Point(x : Int, y : Int) {
  def +(rhs : Point) : Point = Point(x + rhs.x, y + rhs.y)
  def ==(rhs : Point) : Boolean = x == rhs.x && y == rhs.y
}
