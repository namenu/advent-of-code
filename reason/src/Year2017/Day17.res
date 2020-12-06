open Belt

let step = 371

module SpinLock = {
  type t = {
    buffer: array<int>,
    pos: int,
  }

  let make = () => {
    buffer: [0],
    pos: 0,
  }

  let spin = ({buffer, pos}) => {
    let sz = Array.size(buffer)
    let step = mod(step, sz)
    let pos = mod(pos + step, sz)

    let buffer =
      Array.slice(buffer, ~offset=0, ~len=pos + 1)
      ->Array.concat([sz])
      ->Array.concat(Array.sliceToEnd(buffer, pos + 1))

    {buffer: buffer, pos: pos + 1}
  }

  let rec spinWithCount = (t, count) => {
    if mod(count, 100000) == 0 {
      Js.log(count)
    }
    count == 0 ? t : spinWithCount(spin(t), count - 1)
  }

  let identify = ({buffer, pos}) => {
    let sz = Array.size(buffer)
    Array.getUnsafe(buffer, mod(pos + 1, sz))
  }
}

let part1 = () =>
  SpinLock.make()->SpinLock.spinWithCount(2017)->SpinLock.identify->Js.log

let rec part2 = (~size, ~pos, ~identified) =>
  switch size {
  | 50000000 => identified
  | _ =>
    let step = mod(step, size)
    let pos = mod(pos + step, size)

    let identified = pos == 0 ? size : identified
    part2(~size=size + 1, ~pos=pos + 1, ~identified)
  }

part2(~size=1, ~pos=0, ~identified=-1)->Js.log
