// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var input = Fs.readFileSync("input/day01.in", "utf8");

function list_of_digits(input) {
  return Belt_List.fromArray(Array.from(input, Caml_format.caml_int_of_string));
}

function rotate(xs, step) {
  return Belt_Option.getWithDefault(Belt_Option.flatMap(Belt_List.drop(Belt_List.concat(xs, xs), step), (function (__x) {
                    return Belt_List.take(__x, Belt_List.length(xs));
                  })), /* [] */0);
}

function captcha(xs, ys) {
  return Belt_List.reduce(Belt_List.map(Belt_List.zip(xs, ys), (function (param) {
                    var x = param[0];
                    if (x === param[1]) {
                      return x;
                    } else {
                      return 0;
                    }
                  })), 0, (function (prim, prim$1) {
                return prim + prim$1 | 0;
              }));
}

var xs = list_of_digits(input);

var ys = rotate(xs, 1);

console.log(captcha(xs, ys));

var xs$1 = list_of_digits(input);

var ys$1 = rotate(xs$1, Belt_List.length(xs$1) / 2 | 0);

console.log(captcha(xs$1, ys$1));

var part1;

var part2;

exports.input = input;
exports.list_of_digits = list_of_digits;
exports.rotate = rotate;
exports.captcha = captcha;
exports.part1 = part1;
exports.part2 = part2;
/* input Not a pure module */
