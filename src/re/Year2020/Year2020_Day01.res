open Garter

let sampleInput = [1721, 979, 366, 299, 675, 1456]->Vector.fromArray

let input =
  Util.readInput(~year=2020, ~day=1)->Util.splitLines->Array.map(int_of_string)->Vector.fromArray

let findPairs = (input, ~sum) => {
  let nums = input->Set.fromVector(~id=module(Garter.Id.FloatComparable))
  let lut = Set.has(nums)
  nums->Set.keep(x => lut(sum -. x))->Set.toVector->Vector.map(x => (x, sum -. x))
}

let part1 = (input: Vector.t<int>) => {
  let input = input->Vector.map(float_of_int)
  let answer = findPairs(input, ~sum=2020.0)->Vector.map(((x, y)) => x *. y)->Vector.max
  answer->int_of_float
}

let part2 = (input: Vector.t<int>) => {
  let numPairs = input->List.fromVector->Garter.List.orderedPairs
  let lut = numPairs->List.reduce(Belt.Map.Int.empty, (res, (x, y)) => {
    let (sum, mult) = (x + y, x * y)
    switch res->Belt.Map.Int.get(sum) {
    | Some(m) => mult > m ? res->Belt.Map.Int.set(sum, mult) : res
    | None => res->Belt.Map.Int.set(sum, mult)
    }
  })

  input
  ->Vector.keep(x => {
    Belt.Map.Int.has(lut, 2020 - x)
  })
  ->Vector.map(x => x * Belt.Map.Int.getExn(lut, 2020 - x))
  ->Vector.max
}

assert (input->part1 == 436404)
assert (sampleInput->part1 == 514579)

assert (input->part2 == 274879808)
assert (sampleInput->part2 == 241861950)
