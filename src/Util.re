open Belt;

let benchmark = f => {
  let start = Js.Date.now();
  f();
  let elapsed = Js.Date.now() -. start;
  Js.log("elapsed: " ++ elapsed->Float.toString);
};

[@bs.module "console"] external consoledir: ('obj, 'option) => unit = "dir";
let clog = o => consoledir(o, {"depth": "null"});
