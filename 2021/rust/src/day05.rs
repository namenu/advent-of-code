use regex::Regex;
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
    ///
    /// "0,9 -> 5,9" -> H(0,5)
    ///
    fn parse(line: &str) -> Line {
        let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();

        for caps in re.captures(line).iter() {
            let x1 = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
            let y1 = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
            let x2 = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
            let y2 = caps.get(4).unwrap().as_str().parse::<i32>().unwrap();
            if x1 == x2 {
                let (y1, y2) = if y1 < y2 { (y1, y2) } else { (y2, y1) };
                return Line::Vertical(VLine { x: x1, y1, y2 });
            } else if y1 == y2 {
                let (x1, x2) = if x1 < x2 { (x1, x2) } else { (x2, x1) };
                return Line::Horizontal(HLine { y: y1, x1, x2 });
            } else {
                return Line::Diagonal(DLine { x1, y1, x2, y2 });
            }
        }
        panic!()
    }
}

struct Field {
    field: Vec<Vec<i32>>,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in self.field.iter() {
            writeln!(f, "{:?}", row).unwrap();
        }
        Ok(())
    }
}

impl Field {
    fn new(max_x: usize, max_y: usize) -> Self {
        Self {
            field: vec![vec![0 as i32; max_x]; max_y],
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
                for x in *x1..(*x2 + 1) {
                    self.field[*y as usize][x as usize] += 1;
                }
            }
            Line::Vertical(VLine { x, y1, y2 }) => {
                for y in *y1..(*y2 + 1) {
                    self.field[y as usize][*x as usize] += 1;
                }
            }
            Line::Diagonal(DLine { x1, y1, x2, y2 }) => {
                let dx = if x1 < x2 { 1 } else { -1 };
                let dy = if y1 < y2 { 1 } else { -1 };
                let (mut x, mut y) = (*x1, *y1);
                loop {
                    self.field[y as usize][x as usize] += 1;
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
        self.field
            .iter()
            .map(|row| row.iter().filter(|&v| *v > 1).count())
            .sum()
    }
}

fn parse_input() -> Vec<Line> {
    let s = fs::read_to_string("inputs/05.in").unwrap();
    s.lines().map(Line::parse).collect()
}

pub fn part1() -> usize {
    let lines = parse_input();

    let mut f = Field::new(1000, 1000);

    f.draw_lines(&lines, false);

    f.count_overlaps()
}

pub fn part2() -> usize {
    let lines = parse_input();

    let mut f = Field::new(1000, 1000);

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
