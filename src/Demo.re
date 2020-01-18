type placement = White | Black | Empty;

exception OutOfBound(string);

type renderedBoard = list(list(placement));

type position = (int, int);
type player = White | Black;

type play = (player, position)

type game = {
  currentPlayer: player,
  history: list(play),
  won: bool
};

let displayPlayer = (player: player) => {
  switch player {
  | White => "O"
  | Black => "X"
  };
}


let proceedGame = (gameState: game, newMove: position) => {
  {...gameState, history: List.append(gameState.history, [ (gameState.currentPlayer, newMove) ]) }
}

let renderBoard = (moves: list(play), initialBoard: renderedBoard) => {
  List.mapi((row, cols) => {
    List.mapi((col, item) => {
      switch(List.find(play => {
        let (player, (x, y)) = play;
        x == row && y == col;
      })(moves)) {
        | (player, (x, y)) => {
          if (y == 2) {
            displayPlayer(player) ++ "\n"
          } else {
            displayPlayer(player)
          }
        }
        | exception Not_found => {
          if (col == 2) {
            "E" ++ "\n"
          } else {
            "E"
          }
        }
      }
    })(cols)
  })(initialBoard)
}

let renderGame = (gameState: game) => {
  let initialBoard: renderedBoard = [
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty]
  ]

  renderBoard(gameState.history, initialBoard) |> List.flatten |> String.concat("")
}

let sample: game = {
  currentPlayer: Black,
  history: [(Black, (1, 0)), (White, (0, 1))],
  won: false
}

Js.Console.log("Starting Game")
Js.Console.log(renderGame(sample))



