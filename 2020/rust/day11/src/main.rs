use std::fmt;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Cell {
    Floor,
    EmptySeat,
    OccupiedSeat,
}

impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = match self {
            Cell::Floor => '.',
            Cell::EmptySeat => 'L',
            Cell::OccupiedSeat => '#',
        };
        write!(f, "{}", c)
    }
}

const DIRS: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

const THRESHOLD: usize = 5;

#[derive(Debug, Clone, PartialEq)]
struct Grid {
    cells: Vec<Vec<Cell>>,
    size: usize,
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in &self.cells {
            for c in line {
                write!(f, "{}", c).unwrap()
            }
            writeln!(f, "").unwrap()
        }
        Ok(())
    }
}

impl Grid {
    fn new(input: &String) -> Grid {
        let mut cells = vec![];
        for line in input.lines() {
            let mut row = vec![];
            for c in line.chars() {
                let cell = match c {
                    'L' => Cell::EmptySeat,
                    '#' => Cell::OccupiedSeat,
                    '.' | _ => Cell::Floor,
                };
                row.push(cell);
            }
            cells.push(row);
        }
        let size = cells.len();
        Grid { cells, size }
    }

    fn in_bound(&self, (x, y): (i32, i32)) -> bool {
        y >= 0 && y < self.size as i32 && x >= 0 && x < self.size as i32
    }

    // If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    // If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    // Otherwise, the seat's state does not change.
    fn num_occupied_adjacents(&self, (x, y): (usize, usize)) -> usize {
        let mut num_occupied = 0;
        for (dy, dx) in DIRS {
            let (y2, x2) = (y as i32 + dy, x as i32 + dx);
            if self.in_bound((x2, y2)) && self.cells[y2 as usize][x2 as usize] == Cell::OccupiedSeat
            {
                num_occupied += 1
            }
        }
        num_occupied
    }

    // Now, instead of considering just the eight immediately adjacent seats,
    // consider the first seat in each of those eight directions.
    fn num_occupied_directions(&self, (x, y): (usize, usize)) -> usize {
        let nearest = |(dy, dx): (i32, i32)| -> Cell {
            let (mut y2, mut x2) = (y as i32, x as i32);
            loop {
                y2 += dy;
                x2 += dx;
                if self.in_bound((x2, y2)) {
                    match self.cells[y2 as usize][x2 as usize] {
                        Cell::Floor => continue,
                        seat => break seat,
                    }
                } else {
                    break Cell::Floor;
                }
            }
        };

        let mut cnt = 0;
        for dir in DIRS {
            if nearest(dir) == Cell::OccupiedSeat {
                cnt += 1
            }
        }

        cnt
    }

    fn next_state_for_cell(&self, (x, y): (usize, usize)) -> Cell {
        let cnt = self.num_occupied_directions((x, y));
        let current = self.cells[y][x];
        let next = match current {
            Cell::EmptySeat if cnt == 0 => Cell::OccupiedSeat,
            Cell::OccupiedSeat if cnt >= THRESHOLD => Cell::EmptySeat,
            otherwise => otherwise,
        };
        next
    }

    fn next_state(&self) -> Grid {
        let mut next = self.clone();

        for (y, line) in self.cells.iter().enumerate() {
            for (x, _c) in line.iter().enumerate() {
                next.cells[y][x] = self.next_state_for_cell((x, y));
            }
        }

        next
    }

    fn num_occupied(&self) -> usize {
        let mut cnt = 0;
        for line in self.cells.iter() {
            for c in line.iter() {
                if *c == Cell::OccupiedSeat {
                    cnt += 1;
                }
            }
        }
        cnt
    }

    fn find_stable(&self) -> Grid {
        let mut history = vec![self.clone()];

        loop {
            let current = history.last().unwrap();
            let next = current.next_state();
            if let Some(_) = history.iter().find(|&s| *s == next) {
                break next;
            } else {
                history.push(next);
            }
        }
    }
}

fn main() {
    // let sample_input = fs::read_to_string("sample_input.txt").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();

    let g = Grid::new(&input);

    println!("{}", g.find_stable().num_occupied());
}
