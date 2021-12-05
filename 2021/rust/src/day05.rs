use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use std::fs;

#[derive(Debug)]
struct HLine {
    y: i32,
    x1: i32,
    x2: i32,
}

#[derive(Debug)]
struct VLine {
    x: i32,
    y1: i32,
    y2: i32,
}

#[derive(Debug)]
struct DLine {
    x1: i32,
    x2: i32,
    y1: i32,
    y2: i32,
}

#[derive(Debug)]
enum Line {
    Horizontal(HLine),
    Vertical(VLine),
    Diagonal(DLine),
}

impl Line {
    fn parse(line: &str) -> Line {
        // "0,9 -> 5,9" -> H(0,5)
        let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();

        for caps in re.captures(line).iter() {
            let x1 = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
            let y1 = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
            let x2 = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
            let y2 = caps.get(4).unwrap().as_str().parse::<i32>().unwrap();
            if x1 == x2 {
                return Line::Vertical(VLine { x: x1, y1, y2 });
            } else if y1 == y2 {
                return Line::Horizontal(HLine { y: y1, x1, x2 });
            } else {
                return Line::Diagonal(DLine { x1, y1, x2, y2 });
            }
        }
        panic!()
    }
}

struct Field {
    points: HashMap<(i32, i32), usize>,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let max_x = self.points.keys().map(|p| p.0).max().unwrap();
        let max_y = self.points.keys().map(|p| p.1).max().unwrap();
        for y in 0..(max_y + 1) {
            for x in 0..(max_x + 1) {
                let z = self.points.get(&(x, y));
                write!(f, "{}", z.unwrap_or(&0))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn interval(a: i32, b: i32) -> std::ops::Range<i32> {
    if a < b {
        a..b + 1
    } else {
        b..a + 1
    }
}

impl Field {
    fn new() -> Self {
        Self {
            points: HashMap::new(),
        }
    }

    fn draw_lines(&mut self, lines: &Vec<Line>, use_diagonal: bool) {
        lines
            .iter()
            .filter(|l| match l {
                Line::Diagonal(_) => use_diagonal,
                _ => true,
            })
            .for_each(|l| self.draw_line(l));
    }

    fn draw_line(&mut self, line: &Line) {
        match line {
            Line::Horizontal(HLine { y, x1, x2 }) => {
                for x in interval(*x1, *x2) {
                    let e = self.points.entry((x, *y)).or_insert(0);
                    *e += 1;
                }
            }
            Line::Vertical(VLine { x, y1, y2 }) => {
                for y in interval(*y1, *y2) {
                    let e = self.points.entry((*x, y)).or_insert(0);
                    *e += 1;
                }
            }
            Line::Diagonal(DLine { x1, y1, x2, y2 }) => {
                let dx = if x1 < x2 { 1 } else { -1 };
                let dy = if y1 < y2 { 1 } else { -1 };
                let (mut x, mut y) = (*x1, *y1);
                loop {
                    let e = self.points.entry((x, y)).or_insert(0);
                    *e += 1;
                    if x == *x2 {
                        assert_eq!(y, *y2);
                        break;
                    }
                    x += dx;
                    y += dy;
                }
            }
        }
    }

    fn count_overlaps(&self) -> usize {
        self.points.iter().filter(|(_, v)| **v > 1).count()
    }
}

static SAMPLE: &str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";

fn parse_input() -> Vec<Line> {
    let s = fs::read_to_string("inputs/05.in").unwrap();
    s.lines().map(Line::parse).collect()
}

pub fn part1() -> usize {
    let lines = parse_input();

    let mut f = Field::new();
    f.draw_lines(&lines, false);
    f.count_overlaps()
}

pub fn part2() -> usize {
    let lines = parse_input();

    let mut f = Field::new();
    f.draw_lines(&lines, true);
    f.count_overlaps()
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        assert_eq!(super::part1(), 7438)
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(), 21406)
    }
}
