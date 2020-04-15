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

type networkBridge = (string, (result(bool, string) => unit)) => unit;

let attemptMoveCommand: (state, networkBridge) => IO.t(state, string) = (state, networkBridge) => {  
  let moveNums = c => {
    let num = L.head(c[0]) |> Opt.getOrElse(5);
    let newCards = Array.copy(c);
    newCards[1] = L.append(num, c[1]);
    newCards[0] = [];
    newCards
  };

  let moveIfLegal = (state, legal) => legal ? moveNums(state.cards) : state.cards;

  isMoveLegalRequest(networkBridge, 2, 3)     // make network request
  |> IO.map(moveIfLegal(state))               // operate on state subtree
  |> IO.map(c => {...state, cards: c});       // replace state subtree, resolve to full state
};

let testRequestSuccessLegalMove = () => {
  let passthroughNetworkBridge = (_, onDone) => onDone(Ok(true));
  let cards = [|[1], [2]|];
  let state = { cards, other: 5};
  
  attemptMoveCommand(state, passthroughNetworkBridge)
  |> IO.unsafeRunAsync(r => switch (r) {
    | Ok(s) => {
      Js.log(s.cards[0] == []);
      Js.log(s.cards[1] == [2, 1]);
    }
    | Error(e) => Js.log(e)
  });
};

let testRequestSuccessIllegalMove = () => {
  let cards = [|[1], [2]|];
  let state = { cards, other: 5};
  let passthroughNetworkBridge = (_, onDone) => onDone(Error("error"));
  
  attemptMoveCommand(state, passthroughNetworkBridge)
  |> IO.unsafeRunAsync(r => switch (r) {
    | Ok(_) => Js.log("false")
    | Error(_) => Js.log("true")
  });
};

testRequestSuccessLegalMove();
testRequestSuccessIllegalMove();
