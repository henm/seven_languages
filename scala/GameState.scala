/**
 * Seven Languages in Seven Weeks
 * Scala, day 1
 */

import scala.util.Random

// First define enumeration-objects to represent the different states of a box
// in the game
sealed trait SelectState {
    def name: String
}
case object X extends SelectState { val name = "X" }
case object O extends SelectState { val name = "O" }
case object Empty extends SelectState { val name = "Empty" }

// Now define a class representing the state of a game
// Some kind of array with immutable fields would be great!
class GameState(val fields: Array[SelectState]) {

    val row1 = (fields(0), fields(1), fields(2))
    val row2 = (fields(3), fields(4), fields(5))
    val row3 = (fields(6), fields(7), fields(8))

    val col1 = (fields(0), fields(3), fields(6))
    val col2 = (fields(1), fields(4), fields(5))
    val col3 = (fields(2), fields(5), fields(8))

    val diag1 = (fields(0), fields(4), fields(8))
    val diag2 = (fields(2), fields(4), fields(6))

    def xWinning():Boolean = {
        isWinning(X)
    }

    def oWinning(): Boolean = {
        isWinning(O)
    }

    def isWinning(state: SelectState) = {
        (isComplete(row1, state) || isComplete(row2, state) || isComplete(row3, state)
            || isComplete(col1, state) || isComplete(col2, state) || isComplete(col3, state)
            || isComplete(diag1, state) || isComplete(diag2, state))
    }

    def isComplete(row: Tuple3[SelectState, SelectState, SelectState], state: SelectState) = {
        row.productIterator.forall(box => box == state)
    }

    def print() = {
        println(row1)
        println(row2)
        println(row3)
    }

    def checkAlreadySet(state: SelectState) = {
        if (state != Empty) {
            throw new IllegalStateException("Field is already set");
        }
    }

    def setField(field: Int, state: SelectState) = {
        if (field < 0 || field > 8) {
            throw new IllegalArgumentException("Invalid field!")
        }
        fields(field) = state
        new GameState(fields)
    }

    def getRandomEmptyField() = {
        val emptyFields = getEmptyFields()
        val randomIndex = Random.nextInt(emptyFields.length)
        emptyFields(randomIndex)
    }

    def getEmptyFields() = {
        fields.zipWithIndex
            .partition(x => (x._1 == Empty))
            ._1
            .unzip
            ._2
    }
}

// Representing a whole game
class Game() {

    var state = new GameState(Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty))
    var player: SelectState = X // X is starting

    def play(): Unit = {
        val field = state.getRandomEmptyField()

        state = state.setField(field, player)

        state.print()
        println("----------------------------------")

        if (state.xWinning) {
            println("X wins!")
            return
        }
        if (state.oWinning) {
            println("Y wins!")
            return
        }

        swapPlayer()

        play()
    }

    def swapPlayer() = {
        if (player == X) {
            player = O
        } else if (player == O) {
            player = X
        } else {
            throw new IllegalStateException("Invalid player")
        }
    }
}

// Play!
val game = new Game()
game.play()