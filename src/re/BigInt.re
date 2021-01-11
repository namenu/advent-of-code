type t;

[@bs.val] external fromInt: int => t = "BigInt";
[@bs.val] external fromFloat: float => t = "BigInt";
[@bs.val] external fromString: string => t = "BigInt";

[@bs.val] external toInt: t => int = "Number";
[@bs.val] external toString: t => string = "String";

external (+): (t, t) => t = "%addfloat";
external (-): (t, t) => t = "%subfloat";
external ( * ): (t, t) => t = "%mulfloat";
external (/): (t, t) => t = "%divfloat";
let (mod): (t, t) => t = [%raw {|function (a, b) { return (a % b); }|}];
let ( ** ): (t, t) => t = [%raw {|function (a, b) { return (a ** b); }|}];

let (lsl): (t, t) => t = [%raw {|function (a, b) { return (a << b); }|}];
let (lsr): (t, t) => t = [%raw {|function (a, b) { return (a >> b); }|}];
let (lor): (t, t) => t = [%raw {|function (a, b) { return (a | b); }|}];
let (land): (t, t) => t = [%raw {|function (a, b) { return (a & b); }|}];

let zero = fromInt(0);
let one = fromInt(1);
