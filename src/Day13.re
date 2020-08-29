open Belt;

module Walls = {
  type t = {layers: Map.Int.t(int)};

  let fromString = text => {
    let parse = s => {
      let kv = s->Js.String2.split(": ")->Array.map(int_of_string);
      (Array.getUnsafe(kv, 0), Array.getUnsafe(kv, 1));
    };

    let layers =
      text->Js.String2.trim->Js.String2.split("\n")->Array.map(parse)->Map.Int.fromArray;

    {layers: layers};
  };

  let severity = (w: t) => {
    w.layers
    ->Map.Int.keep((k, v) => {
        let period = (v - 1) * 2;
        k mod period == 0;
      })
      ->Map.Int.reduce(0, (acc, k, v) => {
        acc + k * v
      });
  };
};

let input = "0: 3\n1: 2\n4: 4\n6: 4";
let input = Node.Fs.readFileAsUtf8Sync("input/day13.in")

let w = Walls.fromString(input);

Walls.severity(w)->Js.log;
