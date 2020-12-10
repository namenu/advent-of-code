open Belt

let sampleInput = [1721, 979, 366, 299, 675, 1456]

let input = Util.readInput(~year=2020, ~day=1)->Util.splitLines->Array.map(int_of_string)

let findPairs = (input, ~sum) => {
  let nums = input->Set.fromArray(~id=module(Garter.Id.FloatComparable))
  let lut = Set.has(nums)
  nums->Set.keep(x => lut(sum -. x))->Set.toArray->Array.map(x => (x, sum -. x))
}

let part1 = input => {
  findPairs(input, ~sum=2020.0)->Array.map(((x, y)) => x *. y)->Garter.Array.max->Js.log
}

let part2 = input => {
  let numPairs = input->List.fromArray->Garter.List.orderedPairs
  let lut = numPairs->List.reduce(Map.Int.empty, (res, (x, y)) => {
    let (sum, mult) = (x + y, x * y)
    switch res->Map.Int.get(sum) {
    | Some(m) => mult > m ? res->Map.Int.set(sum, mult) : res
    | None => res->Map.Int.set(sum, mult)
    }
  })

  Array.keep(input, x => {
    Map.Int.has(lut, 2020 - x)
  })->Array.map(x => x * Map.Int.getExn(lut, 2020 - x))->Garter.Array.max->Js.log
}

// input->part1->Js.log
// sampleInput->part1->Js.log

// input->part2->Js.log
// sampleInput->part2->Js.log
