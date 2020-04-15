module L = Relude.List;
module IO = Relude.IO;
module Opt = Relude.Option;

module AppErrorType: BsBastet.Interface.TYPE with type t = string = {
  type t = string;
};
module IOAppError = IO.WithError(AppErrorType);
let (>>=) = IOAppError.Infix.(>>=);

let l = L.cons(1, [2,3]);

L.showBy(string_of_int, l) |> Js.log;

let asyncFunc = onDone => {
  Js.log("asyncFunc");
  onDone(Ok(7));
//  onDone(Error("bad"));
};

Js.log("Making IO");
let io: IO.t(int, string) = IO.async(asyncFunc);

let io2: IO.t(int, string) = IO.suspendIO(() => IO.async(onDone => onDone(Ok(7))));

Js.log("unsafeRunAsync");

io |> IO.unsafeRunAsync(r => switch (r) {
  | Ok(a) => Js.log(a)
  | Error(msg) => Js.log(msg)
});


type effect =
  | NetworkRequest(string);

let run = effect => 
  switch (effect) {
  | NetworkRequest(s) => Js.log(s)
  };

let isMoveLegal2 = (c1, c2) => NetworkRequest("move_legality");


let otherNetworkRequest = () => IO.async(onDone => onDone(Ok(5)));



let otherUseCase = (arg) => IO.suspendIO(otherNetworkRequest)


// Monad version
 // let moveIfLegalM = legal => IO.pure(legal ? moveNums(state) : state);

 // let cmdM = 
 //  isMoveLegalUseCase(() => 5, 2, 3)
 //  >>= (moveIfLegalM)

let makeNetworkRequest = () => IO.async(onDone => onDone(Ok(true)));

// If move is legal, move cards between cascades
let isMoveLegalRequest = (s, c1, c2) => IO.suspendIO(makeNetworkRequest);

type state = {
  cards: array(list(int)),
  other: int
};

let cards = [|[1], [2]|];

let state = { cards, other: 5};

let attemptMoveCommand: IO.t(state, string) = {
  let moveNums = s => {
    let num = L.head(s[0]) |> Opt.getOrElse(5);
    s[1] = L.append(num, s[1]);
    s[0] = [];
    s
  };

  let moveIfLegal = legal => legal ? moveNums(state.cards) : state.cards;

  isMoveLegalRequest(() => 5, 2, 3)      // make network request
  |> IO.map(moveIfLegal)                 // operate on state subtree
  |> IO.map(c => {...state, cards: c});  // replace state subtree, resolve to full state
};

let test = () => {
  // Command
  attemptMoveCommand
  |> IO.unsafeRunAsync(_ => ());

  // Result
  Js.log(state.cards[0] == []);
  Js.log(state.cards[1] == [2, 1]);
};

test();
