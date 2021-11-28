open Belt;

let benchmark = f => {
  let start = Js.Date.now();
  f();
  let elapsed = Js.Date.now() -. start;
  Js.log("elapsed: " ++ elapsed->Float.toString);
};

[@bs.module "console"] external consoledir: ('obj, 'option) => unit = "dir";
let clog = o => consoledir(o, {"depth": "null"});

let splitLines = s => s->Js.String2.trim->Js.String2.split("\n");

let splitParagraphs = s => s->Js.String2.trim->Js.String2.split("\n\n");

[@bs.val]
external dirname: string = "__dirname";

let readInput = (~year, ~day) => {
  let path = Printf.sprintf({j|resources/year%04d/day%02d.in|j}, year, day);
  let dir = dirname ++ "/../../"
  Node_fs.readFileAsUtf8Sync(dir ++ path);
};
