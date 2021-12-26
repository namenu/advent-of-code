type Board = [[i8; 5]; 5];

fn has_won(board: &Board) -> bool {
    for i in 0..5 {
        // check rows
        let mut line = true;
        for c in 0..5 {
            if board[i][c] != -1 {
                line = false;
                break;
            }
        }
        if line {
            return true;
        }

        // check cols
        let mut line = true;
        for r in 0..5 {
            if board[r][i] != -1 {
                line = false;
                break;
            }
        }
        if line {
            return true;
        }
    }
    return false;
}

fn score(board: &Board) -> i32 {
    let mut sum: i32 = 0;
    for r in 0..5 {
        for c in 0..5 {
            if board[r][c] != -1 {
                sum += board[r][c] as i32
            }
        }
    }
    sum
}

struct Bingo {
    draws: Vec<i8>,
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

        let mut boards: Vec<Board> = Vec::new();

        while let Some(_) = lines.next() {
            let mut board: Board = [[0; 5]; 5];

            for r in 0..5 {
                for (c, num) in lines.next().unwrap().split_whitespace().enumerate() {
                    let num = num.parse().unwrap();
                    board[r][c] = num;
                }
            }
            boards.push(board);
        }

        Self { draws, boards }
    }

    fn play(&mut self) -> i32 {
        for d in &self.draws {
            for b in self.boards.iter_mut() {
                for r in 0..5 {
                    for c in 0..5 {
                        if b[r][c] == *d {
                            b[r][c] = -1;
                        }
                    }
                }
            }

            for b in &self.boards {
                if has_won(b) {
                    return score(b) * *d as i32;
                }
            }
        }
        panic!()
    }

    fn play2(&mut self) -> i32 {
        let mut result = Vec::new();
        for b in self.boards.iter_mut() {
            for (cnt, d) in self.draws.iter().enumerate() {
                for r in 0..5 {
                    for c in 0..5 {
                        if b[r][c] == *d {
                            b[r][c] = -1;
                        }
                    }
                }
                if has_won(b) {
                    let score = score(b) * *d as i32;
                    result.push((cnt, score));
                    break;
                }
            }
        }

        result.sort_by(|(x, _), (y, _)| y.cmp(x));
        result[0].1
    }
}

pub static INPUT: &str = include_str!("../inputs/04.in");
pub static SAMPLE: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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

pub fn part1(input: &str) -> i32 {
    let mut _bingo = Bingo::from_str(input);

    _bingo.play()
}

pub fn part2(input: &str) -> i32 {
    let mut _bingo = Bingo::from_str(input);

    _bingo.play2()
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        assert_eq!(super::part1(super::SAMPLE), 4512);
        assert_eq!(super::part1(super::INPUT), 54275);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(super::SAMPLE), 1924);
        assert_eq!(super::part2(super::INPUT), 13158);
    }
}
