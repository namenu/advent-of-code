open Belt;

let sampleInput = "FBFBBFFRLR";

let input = Node.Fs.readFileAsUtf8Sync("resources/year2020/day05.in");

let toBinary = s => {
  s
  ->Garter.String.toArray
  ->Array.map(c =>
      switch (c) {
      | "B"
      | "R" => "1"
      | _ => "0"
      }
    )
  ->Js.Array2.joinWith("")
  ->Garter.Int.fromStringWithRadix(~radix=2);
};

sampleInput->toBinary->Js.log;
assert(toBinary("BFFFBBFRRR") == 567);
assert(toBinary("FFFBBBFRRR") == 119);
assert(toBinary("BBFFBBFRLL") == 820);

input->Util.splitLines->Array.map(toBinary)->Garter.Array.max->Js.log;
