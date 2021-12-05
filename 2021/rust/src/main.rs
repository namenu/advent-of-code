use regex::Regex;
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

///
/// "0,9 -> 5,9" -> H(0,5)
///
fn parse_line(line: &str) -> Line {
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

fn draw_axial(field: &mut Vec<Vec<i32>>, line: &Line) {
    match line {
        Line::Horizontal(HLine { y, x1, x2 }) => {
            for x in *x1..(*x2 + 1) {
                field[*y as usize][x as usize] += 1;
            }
        }
        Line::Vertical(VLine { x, y1, y2 }) => {
            for y in *y1..(*y2 + 1) {
                field[y as usize][*x as usize] += 1;
            }
        }
        _ => {}
    }
}

fn print_field(field: &Vec<Vec<i32>>) {
    for row in field.iter() {
        println!("{:?}", row);
    }
}

fn part1(lines: &Vec<Line>) -> usize {
    let mut field = vec![vec![0 as i32; 1000]; 1000];

    for l in lines.iter() {
        draw_axial(&mut field, l)
    }

    let mut sum = 0;
    for row in field.iter() {
        let c = row.iter().filter(|&v| *v > 1).count();
        sum += c;
    }

    sum
}

fn draw(field: &mut Vec<Vec<i32>>, line: &Line) {
    match line {
        Line::Horizontal(HLine { y, x1, x2 }) => {
            for x in *x1..(*x2 + 1) {
                field[*y as usize][x as usize] += 1;
            }
        }
        Line::Vertical(VLine { x, y1, y2 }) => {
            for y in *y1..(*y2 + 1) {
                field[y as usize][*x as usize] += 1;
            }
        }
        Line::Diagonal(DLine { x1, y1, x2, y2 }) => {
            let dx = if x1 < x2 { 1 } else { -1 };
            let dy = if y1 < y2 { 1 } else { -1 };
            let (mut x, mut y) = (*x1, *y1);
            loop {
                field[y as usize][x as usize] += 1;
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

fn part2(lines: &Vec<Line>) -> usize {
    let mut field = vec![vec![0 as i32; 1000]; 1000];

    for l in lines.iter() {
        draw(&mut field, l)
    }

    // print_field(&field);

    field
        .iter()
        .map(|row| row.iter().filter(|&v| *v > 1).count())
        .sum()
}

fn main() {
    let s = fs::read_to_string("inputs/05.in").unwrap();

    // TODO: ?
    let lines: Vec<Line> = s.lines().map(parse_line).collect();

    println!("{}", part1(&lines));

    println!("{}", part2(&lines));
}
