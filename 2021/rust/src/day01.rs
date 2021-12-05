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

fn part1(nums: &Vec<i32>) -> usize {
    nums.array_windows::<2>().filter(|[a, b]| a < b).count()
}

fn part2(nums: &Vec<i32>) -> usize {
    zip(nums.iter(), nums.iter().skip(3))
        .filter(|(&a, &b)| a < b)
        .count()
}

fn main() {
    // let nums = SAMPLE
    let nums = include_str!("../inputs/1.in")
        .lines()
        .map(|n| n.parse().unwrap())
        .collect::<Vec<i32>>();

    println!("{}", part1(&nums));
    println!("{}", part2(&nums));
}
