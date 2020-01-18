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

let getPos = (board: board, pos: position) => {
  let (x, y) = pos;

  List.nth(List.nth(board, x), y)
}

let isWin = (board: board, player: player) => {
  let conditions: list(list(position)) = [
    //Rows
    [(0, 0), (0, 1), (0, 2)],
    [(1, 0), (1, 1), (1, 2)],
    [(2, 0), (2, 1), (2, 2)],
    //Columns
    [(0, 0), (1, 0), (2, 0)],
    [(0, 1), (1, 1), (2, 1)],
    [(0, 2), (1, 2), (2, 2)],
    //Diagonals
    [(0, 0), (1, 1), (2, 2)],
    [(0, 2), (1, 1), (2, 0)]
  ]
  let checkCondition = (condition) => condition |> List.for_all(pos => getPos(board, pos) == displayPlayer(player))

  conditions |> List.exists(checkCondition)
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
    history: List.append(gameState.history,[ (gameState.currentPlayer, newMove) ]),
    board: newBoard,
    currentPlayer: gameState.currentPlayer == Black ? White : Black,
    won: isWin(newBoard, gameState.currentPlayer)
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

  let initialGameState: game = {
    currentPlayer: White,
    history: [],
    board: initialBoard,
    won: false
  }

  let moves = [
    (White, (1,1)),
    (Black, (0, 1)),
    (White, (1,2)),
    (Black, (2, 2)),
    (White, (1,0))
  ]

  let index = ref(0);
  let break = ref(false);
  let gameState = ref(initialGameState);
  Js.Console.log("Starting Game")

  while (List.length(gameState^.history) > 9 || index^ < List.length(moves)) {
    let (player, (x, y)) = List.nth(moves, index^);
    gameState := proceedGame(gameState^, (x, y));
    Js.Console.log("Player<" ++ displayPlayer(player) ++ "> placed position<" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ ">.")
    Js.Console.log(renderGame(gameState^));
    Js.Console.log("========================");
    if (gameState^.won) {
      break := true;
    }
    index := index^ + 1;
  }

  let previousPlayer = (player) => {
    switch(player) {
      | Black => White
      | White => Black
    }
  }

  if (gameState^.won) {
    Js.Console.log("Winner is: " ++ displayPlayer(previousPlayer(gameState^.currentPlayer)) ++ "!");
  } else {
    Js.Console.log("No one won!")
  }
}

startGame()