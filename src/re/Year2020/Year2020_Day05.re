open Belt;

let sampleInput = "FBFBBFFRLR";

let input = Util.readInput(~year=2020, ~day=5)

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

assert(sampleInput->toBinary == 357);
assert(toBinary("BFFFBBFRRR") == 567);
assert(toBinary("FFFBBBFRRR") == 119);
assert(toBinary("BBFFBBFRLL") == 820);

let seats = input->Util.splitLines->Array.map(toBinary);

// part1
seats->Garter.Array.max->Js.log;

// part2
seats
->Garter.Array.Int.groupBy(~keyFn=x => x lsr 3)
->Map.Int.keep((_, v) => Array.length(v) < 8)
->Js.log;
