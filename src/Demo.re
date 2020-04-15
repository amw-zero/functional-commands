module L = Relude.List;
module IO = Relude.IO;
module Opt = Relude.Option;

module AppErrorType: BsBastet.Interface.TYPE with type t = string = {
  type t = string;
};
module IOAppError = IO.WithError(AppErrorType);
let (>>=) = IOAppError.Infix.(>>=);


// Monad version
 // let moveIfLegalM = legal => IO.pure(legal ? moveNums(state) : state);

 // let cmdM = 
 //  isMoveLegalUseCase(() => 5, 2, 3)
 //  >>= (moveIfLegalM)


let makeNetworkRequest = (path, networkBridge, ()) => IO.async(networkBridge(path));

// If move is legal, move cards between cascades
let isMoveLegalRequest = (networkBridge, c1, c2) => IO.suspendIO(makeNetworkRequest("api/move_legality", networkBridge));

type state = {
  cards: array(list(int)),
  other: int
};

let cards = [|[1], [2]|];

let state = { cards, other: 5};

type networkBridge = (string, (result(bool, string) => unit)) => unit;

let attemptMoveCommand: (networkBridge) => IO.t(state, string) = networkBridge => {
  let moveNums = s => {
    let num = L.head(s[0]) |> Opt.getOrElse(5);
    s[1] = L.append(num, s[1]);
    s[0] = [];
    s
  };

  let moveIfLegal = legal => legal ? moveNums(state.cards) : state.cards;

  isMoveLegalRequest(networkBridge, 2, 3)     // make network request
  |> IO.map(moveIfLegal)                      // operate on state subtree
  |> IO.map(c => {...state, cards: c});       // replace state subtree, resolve to full state
};

let test = () => {
  let passthroughNetworkBridge = (_, onDone) => onDone(Ok(true));
  
  attemptMoveCommand(passthroughNetworkBridge)
  |> IO.unsafeRunAsync(_ => ());

  Js.log(state.cards[0] == []);
  Js.log(state.cards[1] == [2, 1]);
};

test();
