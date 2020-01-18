type placement = White | Black | Empty;

exception OutOfBound(string);

type board = list(list(string));

type position = (int, int);
type player = White | Black;

type play = (player, position)

type game = {
  currentPlayer: player,
  history: list(play),
  board: board,
  won: bool
};

let displayPlayer = (player: player) => {
  switch player {
  | White => "O"
  | Black => "X"
  };
}

let updateBoard = (board, move) => {
  let (player, (x, y)) = move;
  board |> List.mapi((rowIndex, row) => {
    if (rowIndex == x) {
      row |> List.mapi((colIndex, value) => {
        if (colIndex == y) {
          displayPlayer(player)
        } else {
          value
        }
      })
    } else {
      row
    }
  })
}

let proceedGame = (gameState: game, newMove: position) => {
  let newBoard = updateBoard(gameState.board, (gameState.currentPlayer, newMove));
  {
    ...gameState,
    history: List.append(gameState.history,[ (gameState.currentPlayer, newMove) ]),
    board: newBoard,
    currentPlayer: gameState.currentPlayer == Black ? White : Black
  }
}

let renderBoard = (board: board) => {
  let combineColumns = String.concat("")
  let combineRows = String.concat("\n")

  board |>  List.map(combineColumns) |> combineRows
}

let renderGame = (gameState: game) => {
  renderBoard(gameState.board)
}


let startGame = () => {
  let initialBoard: board = [
    ["_", "_", "_"],
    ["_", "_", "_"],
    ["_", "_", "_"]
  ]

  let sample: game = {
    currentPlayer: White,
    history: [],
    board: initialBoard,
    won: false
  }

  Js.Console.log("Starting Game")
  Js.Console.log(renderGame(sample))
}

startGame()