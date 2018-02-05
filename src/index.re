open Reprocessing;

type pieceTypeT =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King;

type colorT =
  | White
  | Black;

type pieceT = (colorT, pieceTypeT);

type squareT =
  | None
  | Some(pieceT);

type stateT = array(array(squareT));

let startingBackRank = color => [|
  Some((color, Rook)),
  Some((color, Knight)),
  Some((color, Bishop)),
  Some((color, Queen)),
  Some((color, King)),
  Some((color, Bishop)),
  Some((color, Knight)),
  Some((color, Rook))
|];

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  Array.mapi(
    (rank, row) =>
      switch (rank + 1) {
      | 1 => startingBackRank(White)
      | 2 => Array.map(_square => Some((White, Pawn)), row)
      | 7 => Array.map(_square => Some((Black, Pawn)), row)
      | 8 => startingBackRank(Black)
      | _n => row
      },
    Array.make_matrix(8, 8, None)
  );
};

let drawSquare = (rank, file, _piece, env) => {
  let squareSize = 75;
  let lightSquare = Utils.color(~r=255, ~g=255, ~b=240, ~a=255);
  let darkSquare = Utils.color(~r=101, ~g=0, ~b=11, ~a=255);
  let squareColors = [|lightSquare, darkSquare|];
  Draw.fill(squareColors[(rank + file) mod 2], env);
  Draw.rect(
    ~pos=(rank * squareSize, file * squareSize),
    ~width=squareSize,
    ~height=squareSize,
    env
  );
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=100, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env);
  Array.iteri(
    (rank, row) =>
      Array.iteri((file, square) => drawSquare(rank, file, square, env), row),
    state
  );
  state;
};

run(~setup, ~draw, ());