// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Relude_IO = require("relude/src/Relude_IO.bs.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");

var AppErrorType = { };

var IOAppError = Relude_IO.WithError(AppErrorType);

var $great$great$eq = IOAppError.Infix.$great$great$eq;

var l = Relude_List.cons(1, /* :: */[
      2,
      /* :: */[
        3,
        /* [] */0
      ]
    ]);

console.log(Relude_List.showBy((function (prim) {
            return String(prim);
          }), l));

function asyncFunc(onDone) {
  console.log("asyncFunc");
  return Curry._1(onDone, /* Ok */Block.__(0, [7]));
}

console.log("Making IO");

var io = Relude_IO.async(asyncFunc);

var io2 = Relude_IO.suspendIO((function (param) {
        return Relude_IO.async((function (onDone) {
                      return Curry._1(onDone, /* Ok */Block.__(0, [7]));
                    }));
      }));

console.log("unsafeRunAsync");

Relude_IO.unsafeRunAsync((function (r) {
        console.log(r[0]);
        return /* () */0;
      }), io);

function run(effect) {
  console.log(effect[0]);
  return /* () */0;
}

function isMoveLegal2(c1, c2) {
  return /* NetworkRequest */["move_legality"];
}

function makeNetworkRequest(param) {
  return Relude_IO.async((function (onDone) {
                return Curry._1(onDone, /* Ok */Block.__(0, [true]));
              }));
}

function otherNetworkRequest(param) {
  return Relude_IO.async((function (onDone) {
                return Curry._1(onDone, /* Ok */Block.__(0, [5]));
              }));
}

function isMoveLegalUseCase(s, c1, c2) {
  return Relude_IO.suspendIO(makeNetworkRequest);
}

function otherUseCase(arg) {
  return Relude_IO.suspendIO(otherNetworkRequest);
}

var state = [
  /* :: */[
    1,
    /* [] */0
  ],
  /* :: */[
    2,
    /* [] */0
  ]
];

function moveNums(s) {
  var num = Relude_Option.getOrElse(5, Relude_List.head(Caml_array.caml_array_get(s, 0)));
  Caml_array.caml_array_set(s, 1, Relude_List.append(num, Caml_array.caml_array_get(s, 1)));
  Caml_array.caml_array_set(s, 0, /* [] */0);
  return s;
}

function moveIfLegalM(legal) {
  return Relude_IO.pure(legal ? moveNums(state) : state);
}

function moveIfLegal(legal) {
  if (legal) {
    return moveNums(state);
  } else {
    return state;
  }
}

console.log(Caml_array.caml_array_get(state, 0));

console.log(Caml_array.caml_array_get(state, 1));

var cmdM = Curry._2($great$great$eq, Relude_IO.suspendIO(makeNetworkRequest), moveIfLegalM);

var cmdMap = Relude_IO.map(moveIfLegal, Relude_IO.suspendIO(makeNetworkRequest));

function test(param) {
  Relude_IO.unsafeRunAsync((function (param) {
          return /* () */0;
        }), cmdMap);
  console.log(Caml_array.caml_array_get(state, 0) === /* [] */0);
  console.log(Caml_obj.caml_equal(Caml_array.caml_array_get(state, 1), /* :: */[
            2,
            /* :: */[
              1,
              /* [] */0
            ]
          ]));
  return /* () */0;
}

test(/* () */0);

var L = /* alias */0;

var IO = /* alias */0;

var Opt = /* alias */0;

exports.L = L;
exports.IO = IO;
exports.Opt = Opt;
exports.AppErrorType = AppErrorType;
exports.IOAppError = IOAppError;
exports.$great$great$eq = $great$great$eq;
exports.l = l;
exports.asyncFunc = asyncFunc;
exports.io = io;
exports.io2 = io2;
exports.run = run;
exports.isMoveLegal2 = isMoveLegal2;
exports.makeNetworkRequest = makeNetworkRequest;
exports.otherNetworkRequest = otherNetworkRequest;
exports.isMoveLegalUseCase = isMoveLegalUseCase;
exports.otherUseCase = otherUseCase;
exports.state = state;
exports.moveNums = moveNums;
exports.moveIfLegalM = moveIfLegalM;
exports.moveIfLegal = moveIfLegal;
exports.cmdM = cmdM;
exports.cmdMap = cmdMap;
exports.test = test;
/* IOAppError Not a pure module */
