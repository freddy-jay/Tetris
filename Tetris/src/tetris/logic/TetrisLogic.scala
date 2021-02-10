package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

import scala.collection.mutable.Stack

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims: Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  val centerForTetrominos: Point = if (gridDims.width % 2 == 0) Point((gridDims.width - 1) / 2, 1) else Point((gridDims.width / 2), 1)
  var gameStack: Stack[GameFrame] = Stack(GameFrame(initialBoard,
    Tetromino(intToCellType(randomGen.randomInt(7)), centerForTetrominos)))
  var currentGameFrame: GameFrame = gameStack.top
  var currentTetro: Tetromino = currentGameFrame.currentTetromino
  var gameOver: Boolean = false

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  def rotateLeft(): Unit = {
    val rotatedTetro =  currentTetro.copy(points = currentTetro.rotateLeft())
    rotateTetro(rotatedTetro)
  }

  def rotateRight(): Unit = {
    val rotatedTetro = currentTetro.copy(points = currentTetro.rotateRight())
    println(currentTetro)
    println(rotatedTetro)
    rotateTetro(rotatedTetro)
  }

  def rotateTetro(rotatedTetro: Tetromino) = {
    if (!checkTetroCollision(rotatedTetro))
      (gameStack.push(currentGameFrame.copy(
        currentTetromino = rotatedTetro)))
    updateGameFrame()
  }

  def moveLeft(): Unit = {
    if (checkMovementCollision("left")) gameStack.push(currentGameFrame.copy())
    else
      gameStack.push(
        currentGameFrame.copy(
          currentTetromino = currentTetro.copy(center= Point(currentTetro.center.x - 1, currentTetro.center.y),points = currentTetro.moveLeft())))
    updateGameFrame()
  }

  def moveRight(): Unit = {
      if(checkMovementCollision("right")) gameStack.push(currentGameFrame.copy())
      else
        gameStack.push(
          currentGameFrame.copy(
            currentTetromino = currentTetro.copy(center= Point(currentTetro.center.x + 1, currentTetro.center.y), points = currentTetro.moveRight())))
    updateGameFrame()
  }

  def moveDown(): Unit = {
      if(checkMovementCollision("down")) {
        val updatedGF = currentGameFrame.copy(currentGameFrame.addCurrentToBoard())
        gameStack.push(currentGameFrame.copy(
          board = updatedGF.board, currentTetromino = randomTetromino()))
      }
      else
        gameStack.push(currentGameFrame.copy(
          currentTetromino = currentTetro.copy(center= Point(currentTetro.center.x, currentTetro.center.y + 1), points = currentTetro.moveDown
          ())))
    updateGameFrame()
  }

  def updateGameFrame(): Unit = {
    currentGameFrame = gameStack.top
    currentTetro = currentGameFrame.currentTetromino
    checkGameOver()
  }

  def randomTetromino() = Tetromino(intToCellType(randomGen.randomInt(7)), centerForTetrominos)

  def intToCellType(num: Int): CellType = {
    num match {
      case 0 => ICell
      case 1 => JCell
      case 2 => LCell
      case 3 => OCell
      case 4 => SCell
      case 5 => TCell
      case 6 => ZCell
      case _ => Empty
    }
  }

  def doHardDrop(): Unit = {
    while (!checkMovementCollision("down"))
      moveDown()
    val updatedGF = currentGameFrame.copy(currentGameFrame.addCurrentToBoard())
    gameStack.push(currentGameFrame.copy(
      board = updatedGF.board, currentTetromino = randomTetromino()))
    updateGameFrame()
  }

  def checkMovementCollision(action: String): Boolean = {
    action match {
      case "right" => {
        val right = currentTetro.copy(points = currentTetro.moveRight())
        checkTetroCollision(right)
      }
      case "left" => {
        {
          val left = currentTetro.copy(points = currentTetro.moveLeft())
          checkTetroCollision(left)
        }
      }
      case "down" => {
        val down = currentTetro.copy(points = currentTetro.moveDown())
        checkTetroCollision(down)
      }
    }
  }

  def checkTetroCollision(tetro: Tetromino): Boolean = {
    tetro.getTetrominoPoints().exists(p => gridDims.width <= p.x) ||    // move right collision
    tetro.getTetrominoPoints().exists(p => 0 > p.x)               ||   // move left collision
    tetro.getTetrominoPoints().exists(p => p.y >= gridDims.height)||  // move down collision
    tetro.getTetrominoPoints().exists(p => {                         // tetromino collision
      val testGF = currentGameFrame.copy(currentTetromino = tetro)
      val row = currentGameFrame.board(p.y)
//      println("*" * 5 + "test" + "*" * 5)
//      println(row)
//      println(currentGameFrame.checkCollision(tetro.getTetrominoPoints()))
      row(p.x) != Empty
    })
  }

  def checkGameOver() = {
    val tetroPoints = currentTetro.getTetrominoPoints()
    tetroPoints.foreach(p => {
      val row = currentGameFrame.board(p.y)
      if(row(p.x) != Empty)
        gameOver = true
    })
  }

  // TODO implement me
  def isGameOver: Boolean = gameOver

  // TODO implement me
  def getCellType(p: Point): CellType = currentGameFrame.tetrominoType(p)

}

case class GameFrame(
                      board: Seq[Seq[CellType]],
                      currentTetromino: Tetromino
                    ) {

  def addCurrentToBoard() = addTetromino(currentTetromino)

  def addTetromino(tetroToAdd: Tetromino): Seq[Seq[CellType]] = {
    var newBoard = board
    tetroToAdd.getTetrominoPoints().foreach(p => {
      var row = newBoard(p.y).toList
      row = row.updated(p.x, tetroToAdd.getType())
      newBoard = newBoard.updated(p.y, row)
    })
    while (hasRowToRemove(newBoard))
      newBoard = removeRow(newBoard)
    newBoard
  }

  def tetrominoType(p: Point): CellType = {
    if (currentTetromino.getTetrominoPoints().contains(p)) currentTetromino.getType()
    else {
      val row = board(p.y)
      row(p.x)
    }
  }

  def checkCollision(pointsToCheck: Seq[Point]): Boolean = {
    var returnValue = false
    board.foreach(row => if (row.toSeq.intersect(pointsToCheck).isEmpty) returnValue = false else return true)
    returnValue
  }

  def hasRowToRemove(board: Seq[Seq[CellType]]): Boolean = {
    for(row <- board)
      if (!row.contains(Empty))
        return true
    false
  }

  def removeRow(board: Seq[Seq[CellType]]): Seq[Seq[CellType]] = {
    var newBoard = board
    for (row <- newBoard.size - 1 to 1 by -1) {
      if (!newBoard(row).contains(Empty)) {
        for (rowToChange <- row to 1 by -1) {
          newBoard = newBoard.updated(rowToChange, newBoard(rowToChange - 1))
        }
      }
    }
    newBoard
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller
  val DefaultWidth: Int = 10


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

  def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }
}
