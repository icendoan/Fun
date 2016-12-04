use std::collections::HashSet;
use std::fmt::{Formatter, Display};

mod statics;
use statics::*;

fn main() {
    print!("Day 1: ");
    day1();
    print!("Day 2: ");
    day2();
    print!("Day 3: ");
    day3();
}

#[derive(Copy,Clone)]
enum D {
    N,
    E,
    S,
    W,
}
impl D {
    fn left(&mut self) {
        *self = match *self {
            D::N => D::W,
            D::W => D::S,
            D::S => D::E,
            D::E => D::N,
        }
    }
    fn right(&mut self) {
        *self = match *self {
            D::N => D::E,
            D::E => D::S,
            D::S => D::W,
            D::W => D::N,
        }
    }
}

impl Display for D {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f,
               "{}",
               match *self {
                   D::N => "North",
                   D::W => "West",
                   D::S => "South",
                   D::E => "East",
               })
    }
}

struct Loc {
    x: i64,
    y: i64,
    dir: D,
}

impl Loc {
    fn movn(&mut self, instr: &str, visited: &mut HashSet<(i64, i64)>) -> Option<(i64, i64)> {
        let len = if let Ok(n) = i64::from_str_radix(&instr[1..], 10) {
            n
        } else {
            return None;
        };

        if instr.starts_with('R') {
            self.dir.right()
        } else if instr.starts_with('L') {
            self.dir.left()
        }

        let mut first_revisited = None;

        for _ in 0..len {
            match self.dir {
                D::N => self.y += 1,
                D::S => self.y -= 1,
                D::E => self.x += 1,
                D::W => self.x -= 1,
            }

            if !visited.insert((self.x, self.y)) && first_revisited.is_none() {
                first_revisited = Some((self.x, self.y));
            }
        }

        first_revisited
    }
}



fn day1() {
    let mut visited = HashSet::new();
    visited.insert((0, 0));

    let mut loc = Loc {
        x: 0,
        y: 0,
        dir: D::N,
    };

    let mut revisited = false;

    for instr in day1::DIRECTIONS.split(' ') {
        match loc.movn(instr, &mut visited) {
            Some((x, y)) if !revisited => {
                print!("First revisited at: {}; ", i64::abs(x) + i64::abs(y));
                revisited = true;
            }
            _ => {}
        }
    }

    println!("Final Distance: {}", i64::abs(loc.x) + i64::abs(loc.y));
}

fn day2() {
    let mut code: String = String::with_capacity(5);
    let mut code_simple: String = String::with_capacity(5);
    let mut loc: Loc = Loc {
        x: 1,
        y: 1,
        dir: D::N,
    };

    let mut x = 0;
    let mut y = 2;


    for line in day2::DIRECTIONS.lines() {
        for c in line.chars() {
            match c {
                'U' if loc.y > 0 => {
                    loc.y -= 1;
                }
                'D' if loc.y < 2 => {
                    loc.y += 1;
                }
                'L' if loc.x > 0 => {
                    loc.x -= 1;
                }
                'R' if loc.x < 2 => {
                    loc.x += 1;
                }
                _ => {}
            }

            match c {
                'U' => {
                    if y == 0 || day2::PAD[y - 1][x].is_none() {
                        continue;
                    }
                    y -= 1;
                }
                'R' => {
                    if x == 4 || day2::PAD[y][x + 1].is_none() {
                        continue;
                    }
                    x += 1;
                }
                'D' => {
                    if y == 4 || day2::PAD[y + 1][x].is_none() {
                        continue;
                    }
                    y += 1;
                }
                'L' => {
                    if x == 0 || day2::PAD[y][x - 1].is_none() {
                        continue;
                    };
                    x -= 1;
                }
                _ => {}
            }
        }

        if let Some(c) = day2::PAD[y][x] {
            code.push(c);
        }

        code_simple.push((49 + (3 * loc.y) + loc.x) as u8 as char);
    }

    println!("{}, {}", code_simple, code);
}

fn day3() {

    let mut t: [u64; 3] = [0; 3];
    let mut valid_triangles: u64 = 0;

    for triangle in &day3::TRIANGLES[..] {
        t.copy_from_slice(triangle);
        t.sort();
        if t[2] < t[0] + t[1] {
            valid_triangles += 1;
        }
    }

    let mut bonus_valid_triangles: u64 = 0;

    for triangles in day3::TRIANGLES[..].chunks(3) {
        for col in 0..3 {
            t[0] = triangles[0][col];
            t[1] = triangles[1][col];
            t[2] = triangles[2][col];

            t.sort();

            if t[2] < t[0] + t[1] {
                bonus_valid_triangles += 1;
            }
        }
    }

    println!("Valid triangles: {}; Bonus triangles: {} ",
             valid_triangles,
             bonus_valid_triangles);
}
