use itertools::Itertools;
use std::convert::TryInto;

type Board = [u8; 25];

struct Bingo {
    draws: Vec<u8>,
    boards: Vec<Board>,
}

impl Bingo {
    fn from_str(input: &str) -> Self {
        let mut lines = input.lines();
        let draws = lines
            .next()
            .unwrap()
            .split(',')
            .map(|n| n.parse().unwrap())
            .collect();

        let mut boards: Vec<Board> = vec![];

        for chunk in &lines.chunks(6) {
            let board: Board = chunk
                .skip(1)
                .join(" ")
                .split_whitespace()
                .map(|n| n.parse().unwrap())
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap();
            println!("board: {:?}", board);
            boards.push(board);
        }

        Self { draws, boards }
    }
}

#[allow(dead_code)]
pub fn part1(input: &str) -> usize {
    let _bingo = Bingo::from_str(input);
    0
}

#[allow(dead_code)]
pub fn part2(_input: &str) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use std::fs;

    static INPUT: &str = include_str!("../inputs/05.in");
    static SAMPLE: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";

    #[test]
    fn part1() {
        assert_eq!(super::part1(SAMPLE), 150);
        // assert_eq!(super::part1(INPUT), 7438);
    }

    // #[test]
    // fn part2() {
    //     assert_eq!(super::part2(SAMPLE), 21406);
    //     assert_eq!(
    //         super::part2(&fs::read_to_string("inputs/05.in").unwrap()),
    //         21406
    //     )
    // }
}
