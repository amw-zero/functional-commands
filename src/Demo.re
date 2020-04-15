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


let makeNetworkRequest = () => IO.async(onDone => onDone(Ok(true)));
let otherNetworkRequest = () => IO.async(onDone => onDone(Ok(5)));

// If move is legal, move cards between cascades
let isMoveLegalUseCase = (s, c1, c2) => IO.suspendIO(makeNetworkRequest);
let otherUseCase = (arg) => IO.suspendIO(otherNetworkRequest)

let state = [|[1], [2]|];

let moveNums = s => {
  let num = L.head(s[0]) |> Opt.getOrElse(5);
  s[1] = L.append(num, s[1]);
  s[0] = [];
  s
};

// legal => IO.pure()
 let moveIfLegalM = legal => IO.pure(legal ? moveNums(state) : state);
let moveIfLegal = legal => legal ? moveNums(state) : state;

// moveNums(state);
Js.log(state[0]);
Js.log(state[1]);


// |> IO.unsafeRunAsync(_ => ());



let cmdM = 
  isMoveLegalUseCase(() => 5, 2, 3)
  >>= (moveIfLegalM)

let cmdMap: IO.t(array(list(int)), string) = 
  isMoveLegalUseCase(() => 5, 2, 3)
  |> IO.map(moveIfLegal);

let test = () => {
  // Command
  cmdMap
  |> IO.unsafeRunAsync(_ => ());

  // Result
  Js.log(state[0] == []);
  Js.log(state[1] == [2, 1]);
};

test();
