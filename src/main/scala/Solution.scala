// https://www.youtube.com/watch?v=zBLCbqycVzw
object Solution {

  private type Board = Array[Array[Int]]
  private def printBoard(sudoku: Board): String =
    sudoku.grouped(3).map { bigGroup =>
      bigGroup.map { row =>
        row.grouped(3).map { smallGroup =>
          smallGroup.mkString(" ", " ", " ")
        }.mkString("|", "|", "|")
      }.mkString("\n")
    }.mkString("-------------------------\n", "\n-------------------------\n", "\n-------------------------\n")

  private def validate(sudoku: Board, x: Int, y: Int, value: Int): Boolean = {
    val row = sudoku(y)
    val column = sudoku.map(r => r.apply(x))
    val boxX = x / 3
    val boxY = y / 3
    val box = for {
      yb <- (boxY * 3) until (boxY * 3 + 3) // indicies for rows in THIS box
      xb <- (boxX * 3) until (boxX * 3 + 3) // indicies for cols in THIS box
    } yield sudoku(yb)(xb)

    row.count(_ == value) == 0 && column.count(_ == value) == 0 && box.count(_ == value) == 0
  }

  private def solve(sudoku: Board, x: Int = 0, y: Int = 0): Unit = {
    if (y >= 9) println(printBoard(sudoku))  // completed all columns, final solution
    else if (x >= 9) solve(sudoku, 0, y+1)  // completed a row, move to next row
    else if (sudoku(y)(x) > 0) solve(sudoku, x+1, y)  // if the board already contains a non-zero value (part of the original puzzle), then you need to solve for the next cell to the right
    else (1 to 9).filter(value => validate(sudoku, x, y, value)).foreach { value =>
      // fill the sudoku board with a value
      sudoku(y)(x) = value
      // try the next cell
      solve(sudoku,x + 1, y)
      // if we get here, then value is not a solution at (x,y); then remove the value
      sudoku(y)(x) = 0;
    }
  }

  def main(args: Array[String]): Unit = {
    println("============================================================= problem 1")
    val problem1 =
      Array(
        Array(5,3,0, 0,7,0, 0,0,0),
        Array(6,0,0, 1,9,5, 0,0,0),
        Array(0,9,8, 0,0,0, 0,6,0),

        Array(8,0,0, 0,6,0, 0,0,3),
        Array(4,0,0, 8,0,3, 0,0,1),
        Array(7,0,0, 0,2,0, 0,0,6),

        Array(0,6,0, 0,0,0, 2,8,0),
        Array(0,0,0, 4,1,9, 0,0,5),
        Array(0,0,0, 0,8,0, 0,7,9),
      )

    println(printBoard(problem1))
    solve(problem1)

    println("============================================================= problem 2")
    val problem2 =
      Array(
        Array(5, 3, 0, 0, 7, 0, 0, 0, 0),
        Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
        Array(0, 9, 8, 0, 0, 0, 0, 6, 0),

        Array(8, 0, 0, 0, 0, 0, 0, 0, 3),
        Array(4, 0, 0, 8, 0, 3, 0, 0, 1),
        Array(7, 0, 0, 0, 2, 0, 0, 0, 6),

        Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
        Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
        Array(0, 0, 0, 0, 8, 0, 0, 7, 0),
      )

    println(printBoard(problem2))
    solve(problem2)

    println("============================================================= problem 3")
    val problem3 =
      Array(
        Array(0, 3, 0, 0, 7, 0, 0, 0, 0),
        Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
        Array(0, 9, 8, 0, 0, 0, 0, 6, 0),

        Array(8, 0, 0, 0, 6, 0, 0, 0, 3),
        Array(4, 0, 0, 8, 0, 3, 0, 0, 0),
        Array(7, 0, 0, 0, 2, 0, 0, 0, 6),

        Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
        Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
        Array(0, 0, 0, 0, 8, 0, 0, 7, 9),
      )

    println(printBoard(problem3))
    solve(problem3)

  }
}
