open Jest;

describe("Expect", () => {
  Expect.(
    test("Year2020_Day01", () => {
      open! Year2020_Day01;

      assert(input->part1 == 436404);
      assert(sampleInput->part1 == 514579);

      assert(input->part2 == 274879808);
      assert(sampleInput->part2 == 241861950);

      expect(true) |> toBe(true);
    })
  )

  Expect.(
    test("Year2020_Day01", () => {
      open! Year2020_Day01;

      assert(input->part1 == 436404);
      assert(sampleInput->part1 == 514579);

      assert(input->part2 == 274879808);
      assert(sampleInput->part2 == 241861950);

      expect(true) |> toBe(true);
    })
  )

  Expect.(
    test("Year2020_Day02", () => {
      open! Year2020_Day02;

      assert(part2(sampleInput) == 1);
      assert(part2(input) == 588);

      expect(true) |> toBe(true);
    })
  )

  Expect.(
    test("Year2020_Day09", () => {
      open! Year2020_Day09;

      assert(part2(sampleInput, 127.0) == 62.0);
      assert(part2(input, 144381670.0) == 20532569.0);

      expect(true) |> toBe(true);
    })
  )
  Expect.(
    test("Year2020_Day14", () => {
      open! Year2020_Day14;

      assert(part1(sampleInput) == 165.0);
      assert(part1(input) == 17765746710228.0);

      expect(true) |> toBe(true);
    })
  )
});
