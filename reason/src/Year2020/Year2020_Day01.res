open Belt

let sampleInput = [1721, 979, 366, 299, 675, 1456]

let readInput = () => {
  let input = Node_fs.readFileAsUtf8Sync("input/year2020/day01.in")
  input->Js.String2.split("\n")->Array.map(int_of_string)
}

let part1 = input => {
  let nums = Set.Int.fromArray(input)
  Array.keep(input, x => {
    Set.Int.has(nums, 2020 - x)
  })->Array.map(x => x * (2020 - x))->Garter.Array.max->Js.log
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

readInput()->part1->Js.log
sampleInput->part1->Js.log

readInput()->part2->Js.log
sampleInput->part2->Js.log
