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

let splitParagraphs = input => {
  splitLines(input)
  ->List.fromArray
  ->Garter.List.partitionBy(x => Js.String2.length(x) == 0)
  ->List.keepMap(xs =>
      switch (xs) {
      | [""] => None
      | l => Some(l->List.toArray)
      }
    )
  ->List.toArray;
};
