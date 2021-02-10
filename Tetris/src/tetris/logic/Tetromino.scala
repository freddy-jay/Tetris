package tetris.logic

case class Tetromino(
                 tetroType: CellType,
                 center: Point,
                 points: Seq[Point]
               ) {

  def this(tetroType: CellType, center: Point) = this(tetroType, center, {
    def addTetrominoPoints(x: Int = 0, y: Int = 0): Point = Point(center.x + x, center.y + y)

    tetroType match {
      case ICell => Seq(center, addTetrominoPoints(-1), addTetrominoPoints(1), addTetrominoPoints(2))
      case JCell => Seq(center, addTetrominoPoints(-1), addTetrominoPoints(-1, -1), addTetrominoPoints(1))
      case LCell => Seq(center, addTetrominoPoints(-1), addTetrominoPoints(1, -1), addTetrominoPoints(1))
      case OCell => Seq(center, addTetrominoPoints(0, -1), addTetrominoPoints(1, -1), addTetrominoPoints(1))
      case SCell => Seq(center, addTetrominoPoints(-1), addTetrominoPoints(0, -1), addTetrominoPoints(1, -1))
      case TCell => Seq(center, addTetrominoPoints(-1), addTetrominoPoints(1), addTetrominoPoints(0, -1))
      case ZCell => Seq(center, addTetrominoPoints(0, -1), addTetrominoPoints(-1, -1), addTetrominoPoints(1))
    }
  })

  def getTetrominoPoints() = points

  def getType() = tetroType

  def getCenter() = center

  def rotateRight(): Seq[Point] = {
    println("-" * 3 + "rotateRight()")
        println(center)
    var newPointSet: Seq[Point] = Seq()
    if (tetroType == ICell)
      for (point <- points) {
        val distanceFromCenter = Point(point.x - center.x, point.y - center.y)
        newPointSet = newPointSet.appended(Point(center.x - distanceFromCenter.y + 1, center.y + distanceFromCenter.x))
      }
    else if (tetroType == OCell) newPointSet = points
    else
      for (point <- points) {
        val distanceFromCenter = Point(point.x - center.x, point.y - center.y)
        newPointSet = newPointSet.appended(Point(center.x - distanceFromCenter.y, center.y + distanceFromCenter.x))
      }
    newPointSet
  }

  def rotateLeft(): Seq[Point] = {
    println("-" * 3 + "rotateRight()")
    var newPointSet: Seq[Point] = Seq()
    if (tetroType == ICell)
      for (point <- points) {
        val distanceFromCenter = Point(point.x - center.x, point.y - center.y)
        newPointSet = newPointSet.appended(Point(center.x + distanceFromCenter.y + 1, center.y - distanceFromCenter.x))
      }
    else if (tetroType == OCell) newPointSet = points
    else
      for (point <- points) {
        val distanceFromCenter = Point(point.x - center.x, point.y - center.y)
        newPointSet = newPointSet.appended(Point(center.x + distanceFromCenter.y, center.y - distanceFromCenter.x))
      }
    newPointSet
  }

  def moveRight(): Seq[Point] = {
    println("-" * 3 + "moveRight()")
    var newPointSet: Seq[Point] = Seq()
    for (point <- points)
      newPointSet = newPointSet.appended(Point(point.x + 1, point.y))
    newPointSet
  }

  def moveLeft(): Seq[Point] = {
    println("-" * 3 + "moveLeft()")
    var newPointSet: Seq[Point] = Seq()
    for (point <- points)
      newPointSet = newPointSet.appended(Point(point.x - 1, point.y))
    newPointSet
  }

  def moveDown(): Seq[Point] = {
    println("-" * 3 + "moveDown()")
    var newPointSet: Seq[Point] = Seq()
    for (point <- points)
      newPointSet = newPointSet.appended(Point(point.x, point.y + 1))
    newPointSet
  }
}

object Tetromino {
  def apply(tetroType: CellType, center: Point): Tetromino = new Tetromino(tetroType, center)
}
