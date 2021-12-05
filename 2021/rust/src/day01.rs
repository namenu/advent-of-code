#![feature(array_windows)]
#![feature(iter_zip)]

use std::iter::zip;

#[allow(dead_code)]
const SAMPLE: &str = "199
200
208
210
200
207
240
269
260
263";

let parse_input() -> &Vec<i32> {
    // let nums = SAMPLE
    let nums = include_str!("../inputs/01.in")
        .lines()
        .map(|n| n.parse().unwrap())
        .collect::<Vec<i32>>();
}

pub fn part1() -> usize {
    nums.array_windows::<2>().filter(|[a, b]| a < b).count()
}

pub fn part2() -> usize {
    zip(nums.iter(), nums.iter().skip(3))
        .filter(|(&a, &b)| a < b)
        .count()
}
