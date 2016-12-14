#![allow(dead_code)]
#![feature(slice_patterns)]

extern crate crypto;
use crypto::digest::Digest;
use crypto::md5;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{self, Display, Formatter};
use std::cmp;

mod statics;
use statics::*;

fn main()
{
    print!("Day 1: ");
    day1();
    print!("Day 2: ");
    day2();
    print!("Day 3: ");
    day3();
    print!("Day 4: ");
    day4();
    print!("Day 5: ");
    day5();
    print!("Day 6: ");
    day6();
    print!("Day 7: ");
    day7();
    print!("Day 8: ");
    day8();
    print!("Day 9: ");
    day9();
    print!("Day 10: ");
    day10();
    print!("Day 11: ");
    day11();
    print!("Day 12: ");
    day12();
    print!("Day 13: ");
    day13();
    print!("Day 14: ");
    day14();
    print!("Day 15: ");
    day15();
    print!("Day 16: ");
    day16();
    print!("Day 17: ");
    day17();
    print!("Day 18: ");
    day18();
    print!("Day 19: ");
    day19();
    print!("Day 20: ");
    day20();
    print!("Day 21: ");
    day21();
    print!("Day 22: ");
    day22();
    print!("Day 23: ");
    day23();
    print!("Day 24: ");
    day24();
    print!("Day 25: ");
    day25();
    println!("");
}

#[derive(Copy,Clone)]
enum D
{
    N,
    E,
    S,
    W,
}
impl D
{
    fn left(&mut self)
    {
        *self = match *self {
            D::N => D::W,
            D::W => D::S,
            D::S => D::E,
            D::E => D::N,
        }
    }
    fn right(&mut self)
    {
        *self = match *self {
            D::N => D::E,
            D::E => D::S,
            D::S => D::W,
            D::W => D::N,
        }
    }
}

impl Display for D
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result
    {
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

struct Loc
{
    x: i64,
    y: i64,
    dir: D,
}

impl Loc
{
    fn movn(&mut self, instr: &str, visited: &mut HashSet<(i64, i64)>) -> Option<(i64, i64)>
    {
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



fn day1()
{
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
                print!("First revisited at: {}; ",
                       i64::abs(x) + i64::abs(y));
                revisited = true;
            },
            _ => {},
        }
    }

    println!("Final Distance: {}",
             i64::abs(loc.x) + i64::abs(loc.y));
}

fn day2()
{
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
                },
                'D' if loc.y < 2 => {
                    loc.y += 1;
                },
                'L' if loc.x > 0 => {
                    loc.x -= 1;
                },
                'R' if loc.x < 2 => {
                    loc.x += 1;
                },
                _ => {},
            }

            match c {
                'U' => {
                    if y == 0 || day2::PAD[y - 1][x].is_none() {
                        continue;
                    }
                    y -= 1;
                },
                'R' => {
                    if x == 4 || day2::PAD[y][x + 1].is_none() {
                        continue;
                    }
                    x += 1;
                },
                'D' => {
                    if y == 4 || day2::PAD[y + 1][x].is_none() {
                        continue;
                    }
                    y += 1;
                },
                'L' => {
                    if x == 0 || day2::PAD[y][x - 1].is_none() {
                        continue;
                    };
                    x -= 1;
                },
                _ => {},
            }
        }

        if let Some(c) = day2::PAD[y][x] {
            code.push(c);
        }

        code_simple.push((49 + (3 * loc.y) + loc.x) as u8 as char);
    }

    println!("Square pad code: {}, Diamond pad code: {}",
             code_simple,
             code);
}

fn day3()
{

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

fn day4()
{
    let mut letters: HashMap<char, i32> = HashMap::new();
    let mut letter_freqs: Vec<(i32, char)> = Vec::new();
    let mut room_sum: u32 = 0;
    let mut decryped_name: String = String::new();

    for room_spec in day4::ROOMS.lines() {
        letter_freqs.clear();
        letters.clear();
        decryped_name.clear();
        let room_id = day4_inner(room_spec,
                                 &mut letters,
                                 &mut letter_freqs,
                                 &mut decryped_name);
        // if room_id != 0
        // {
        //    println!("Found real room #{} with name {}", room_id, decryped_name);
        // }
        if decryped_name == "northpole object storage" {
            print!("North Pole objects: {}; ", room_id)
        }

        room_sum += room_id;
    }


    println!("Room number sums: {}", room_sum);
}

#[test]
fn test_day4()
{
    let mut letters = HashMap::new();
    let mut letter_freqs = Vec::new();
    let mut decryped_name = String::new();
    let result = day4_inner("aaaaa-bbb-z-y-x-123[abxyz]",
                            &mut letters,
                            &mut letter_freqs,
                            &mut decryped_name);

    assert!(result == 123);
    letters.clear();
    letter_freqs.clear();
    decryped_name.clear();
    assert!(987 ==
            day4_inner("a-b-c-d-e-f-g-h-987[abcde]",
                       &mut letters,
                       &mut letter_freqs,
                       &mut decryped_name));
    letters.clear();
    letter_freqs.clear();
    decryped_name.clear();
    assert!(day4_inner("not-a-real-room-404[oarel]",
                       &mut letters,
                       &mut letter_freqs,
                       &mut decryped_name) == 404);
    letters.clear();
    letter_freqs.clear();
    decryped_name.clear();
    assert!(day4_inner("totally-real-room-200[decoy]",
                       &mut letters,
                       &mut letter_freqs,
                       &mut decryped_name) == 0);
    letters.clear();
    letter_freqs.clear();
    decryped_name.clear();

    caesar_shift("qzmt-zixmtkozy-ivhz".as_bytes(),
                 343,
                 &mut decryped_name);
    assert!("very encrypted name" == decryped_name);
}

fn day4_inner(room_spec: &str,
              letters: &mut HashMap<char, i32>,
              letter_freqs: &mut Vec<(i32, char)>,
              decryped_name: &mut String)
              -> u32
{

    let name_end_idx = if let Some(ix) = room_spec.rfind('-') {
        ix
    } else {
        return 0;
    };

    let name_str = &room_spec[0..name_end_idx];

    for letter in name_str.chars() {
        if letter == '-' {
            continue;
        }

        if letters.contains_key(&letter) {
            letters.get_mut(&letter).map(|x| *x -= 1);
        } else {
            letters.insert(letter, -1);
        }
    }


    for (key, value) in letters.drain() {
        letter_freqs.push((value, key));
    }

    letter_freqs.sort();

    let code_idx = if let Some(idx) = room_spec.find('[') {
        idx
    } else {
        return 0;
    };

    let room_number = if let Ok(x) = u32::from_str_radix(&room_spec[name_end_idx + 1..code_idx],
                                                         10) {
        x
    } else {
        println!("Parse error!");
        return 0;
    };

    for (&(_, x), y) in letter_freqs.iter().zip(room_spec[1 + code_idx..].chars()).take(5) {
        if x != y {
            return 0;
        }
    }

    caesar_shift(&room_spec[0..name_end_idx].as_bytes(),
                 room_number,
                 decryped_name);
    return room_number;
}

fn caesar_shift(txt: &[u8], shift: u32, buf: &mut String)
{
    let normalised_shift: u8 = (shift % 26) as u8;

    for &c in txt {
        if c == ('-' as u8) {
            buf.push(' ');
            continue;
        }

        buf.push((97 + ((c - 97 + normalised_shift) % 26)) as char);
    }
}

fn day5()
{
    fn first_five_zero(buf: &[u8]) -> bool
    {
        if buf.len() < 3 {
            return false;
        }

        return buf[0] == 0 && buf[1] == 0 && (buf[2] & 0xf0) == 0;
    }

    let mut plaintext = String::new();
    let mut password = String::new();
    let mut password2: [char; 8] = [' '; 8];
    let mut p2 = String::new();
    let mut pass2_num_chars = 0;
    let mut out_buffer: [u8; 16] = [0; 16];

    let hex_chars: [char; 16] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

    let mut md5: md5::Md5 = md5::Md5::new();
    let mut index = 0;

    while (password.len() < 8) || pass2_num_chars < 8 {
        for x in out_buffer.iter_mut() {
            *x = 0;
        }

        plaintext.clear();
        md5.reset();
        plaintext.push_str(day5::ROOM_NUMBER);
        plaintext.push_str(&index.to_string());
        md5.input(plaintext.as_bytes());
        md5.result(&mut out_buffer);

        if first_five_zero(&out_buffer[..]) {
            if password.len() < 8 {
                let c: char = hex_chars[(out_buffer[2] & 0x0f) as usize];
                password.push(c);
            }

            let index = (out_buffer[2] & 0x0f) as usize;
            let c2: char = hex_chars[((out_buffer[3] & 0xf0) >> 4) as usize];
            if (index < 8) && password2[index] == ' ' {
                password2[index] = c2;
                pass2_num_chars += 1;
            }
        }

        index += 1;
    }

    for &c in &password2[..] {
        p2.push(c);
    }

    println!("Sequential password: {}; Positional password: {}",
             password,
             p2);
}


fn day6()
{
    let mut hashmaps = vec![HashMap::new(); 8];
    let mut message = String::new();
    let mut min_msg = String::new();

    for msg in &day6::MESSAGES[..] {
        for (i, c) in msg.chars().enumerate() {
            assert!(i < 8);
            if hashmaps[i].contains_key(&c) {
                hashmaps[i].get_mut(&c).map(|x| *x += 1);
            } else {
                hashmaps[i].insert(c, 1);
            }
        }
    }
    for map in &mut hashmaps[..] {
        let mut max_count = 0;
        let mut max_char = ' ';
        let mut min_count = 1000;
        let mut min_char = ' ';

        for (c, n) in map.drain() {
            if n > max_count {
                max_char = c;
                max_count = n;
            }

            if n < min_count {
                min_char = c;
                min_count = n;
            }
        }
        message.push(max_char);
        min_msg.push(min_char);

    }
    println!("Message: {}; Min Message: {}",
             message,
             min_msg);
}

fn day7()
{

    let mut tls_count: u32 = 0;
    let mut ssl_count: u32 = 0;

    for &ip in &day7::IPs[..] {
        if supports_tls(ip) {
            tls_count += 1;
        }

        if supports_ssl(ip) {
            ssl_count += 1;
        }
    }

    println!("TLS addresses: {}, SSL addresses: {}",
             tls_count,
             ssl_count)
}

fn supports_tls(ip: &str) -> bool
{
    let mut outer_palindrome = false;
    let mut inner_palindrome = false;
    let mut is_inner = false;

    for w in windows(ip, 4) {
        // filter out transitionary windows
        if w.contains('[') {
            is_inner = true;
            continue;
        }

        if w.contains(']') {
            is_inner = false;
            continue;
        }

        if is_inner {
            inner_palindrome |= palindrome(w) &&
                                w.chars()
                                 .take(2)
                                 .fold((true, ' '),
                                       |(x, y), c| (x && (y != c), c))
                                 .0;
        } else {
            outer_palindrome |= palindrome(w) &&
                                w.chars()
                                 .take(2)
                                 .fold((true, ' '),
                                       |(x, y), c| (x && (y != c), c))
                                 .0;
        }
    }

    outer_palindrome && !inner_palindrome
}

fn supports_ssl(ip: &str) -> bool
{
    fn aba(w: &str) -> bool
    {
        (w.len() == 3) && (w.chars().next()) == (w.chars().rev().next()) &&
        w.chars()
         .fold((true, ' '),
               |(x, y), z| (x && (y != z), z))
         .0
    }

    fn invert(w: &str, buf: &mut String)
    {
        if w.len() != 3 {
            return;
        }
        let mut iter = w.chars();
        buf.clear();
        let inner = iter.next().unwrap();
        let outer = iter.next().unwrap();
        buf.push(outer);
        buf.push(inner);
        buf.push(outer);
    }


    let mut inner_sections = Vec::new();
    let mut outer_sections = Vec::new();
    let mut bab: String = String::with_capacity(3);
    let mut inner = false;
    for w in windows(ip, 3) {
        if w.contains('[') {
            inner = true;
            continue;
        }

        if w.contains(']') {
            inner = false;
            continue;
        }
        if aba(w) {
            if inner {
                inner_sections.push(w);
            } else {
                outer_sections.push(w);
            }
        }
    }

    for section in outer_sections {
        for inner_section in &inner_sections {
            invert(inner_section, &mut bab);
            if section == bab {
                return true;

            }

        }

    }

    false
}

#[test]
fn day7_test()
{
    assert!(palindrome("abba"));
    assert!(!palindrome("mnop"));
    assert!(supports_tls("abba[mnop]qrst"),
            "abba[mnop]qrst");
    assert!(!supports_tls("abcd[bddb]xyyx"),
            "abcd[bddb]xyyx");
    assert!(!supports_tls("aaaa[qwer]tyui"),
            "aaaa[qwer]tyui");
    assert!(supports_tls("ioxxoj[asdfgh]zxcvbn"),
            "ioxxoj[asdfgh]zxcvbn");

    assert!(supports_ssl("aba[bab]xyz"),
            "aba[bab]xyz");
    assert!(!supports_ssl("xyx[xyx]xyx"),
            "xyx[xyx]xyx");
    assert!(supports_ssl("aaa[kek]eke"),
            "aaa[kek]eke");
    assert!(supports_ssl("zazbz[bzb]cdb"),
            "zazbz[bzb]cdb");
}

fn day8()
{
    // use u64 = [bool;64]
    let mut screen: Screen = Screen::new(50, 6);
    for instr in &day8::INSTRUCTIONS[..] {
        screen.exec(instr);
    }

    println!("Cells lit: {}", screen.ct());
    screen.pr();
}

struct Screen
{
    data: [[bool; 50]; 6],
    buf: [bool; 50],
    width: usize,
    height: usize,
}

impl Screen
{
    fn rr(&mut self, r: usize, s: usize)
    {
        if r >= self.height {
            return;
        }

        let row: &mut [bool; 50] = &mut self.data[r];
        let buf: &mut [bool; 50] = &mut self.buf;
        let w: usize = self.width;

        for i in 0..w {
            buf[(i + s) % w] = row[i];
        }

        for i in 0..w {
            row[i] = buf[i];
        }
    }

    fn rc(&mut self, c: usize, s: usize)
    {
        if c >= self.width {
            return;
        }
        let h: usize = self.height;

        for i in 0..h {
            self.buf[(i + s) % h] = self.data[i][c];
        }

        for i in 0..h {
            self.data[i][c] = self.buf[i];
        }
    }

    fn sq(&mut self, a: usize, b: usize)
    {
        let h = self.height;
        let w = self.width;
        if w < a || h < b {
            return;
        }
        for row in &mut self.data[0..b] {
            for val in &mut row[0..a] {
                *val = true;
            }
        }
    }

    #[allow(dead_code)]
    fn cl(&mut self)
    {
        for row in &mut self.data[..] {
            for val in &mut row[..] {
                *val = false;
            }
        }

        for val in &mut self.buf[..] {
            *val = false;
        }
    }

    fn pr(&self)
    {
        let mut s = String::with_capacity((self.width + 1) * self.height);

        for row in &self.data[0..self.height] {
            for val in &row[0..self.width] {
                s.push(if *val {
                    '#'
                } else {
                    ' '
                });
            }
            s.push('\n');
        }

        println!("{}", s);
    }

    fn new(w: usize, h: usize) -> Screen
    {
        Screen {
            data: [[false; 50]; 6],
            buf: [false; 50],
            width: w,
            height: h,
        }
    }

    fn exec(&mut self, i: &day8::Instr)
    {
        match *i {
            day8::Instr::RRow(row, shift) => self.rr(row, shift),
            day8::Instr::RCol(col, shift) => self.rc(col, shift),
            day8::Instr::Rect(w, h) => self.sq(w, h),
        }
    }

    fn ct(&self) -> u64
    {
        let mut x = 0;
        for row in &self.data[0..self.height] {
            for val in &row[0..self.width] {
                if *val {
                    x += 1;
                }
            }
        }
        x
    }
}
#[test]
fn test_screen()
{
    use statics::day8::Instr;
    let mut s = Screen::new(7, 3);
    s.exec(&Instr::Rect(3, 2));
    s.exec(&Instr::RCol(1, 1));
    s.exec(&Instr::RRow(0, 4));
    s.exec(&Instr::RCol(1, 1));
    assert!(s.data[0][0..7] == [false, true, false, false, true, false, true]);
    assert!(s.data[1][0..7] == [true, false, true, false, false, false, false]);
    assert!(s.data[2][0..7] == [false, true, false, false, false, false, false]);
}

fn day9()
{
    let mut size: u64 = 0;
    let d = Decompressor::new(day9::FILE_TEXT);
    for block in d {
        size += block.len() as u64;
    }

    println!("Decompressed size: {}, full size: {}",
             size,
             decompressed_size(day9::FILE_TEXT));
}

#[derive(Debug)]
enum DecompressionBlock
{
    Mul(u64, u64, u64),
    Raw(u64, u64),
}

impl DecompressionBlock
{
    fn position(&self) -> u64
    {
        match *self {
            DecompressionBlock::Mul(p, _, _) => p,
            DecompressionBlock::Raw(p, _) => p,
        }
    }

    fn length(&self) -> u64
    {
        match *self {
            DecompressionBlock::Mul(_, l, _) |
            DecompressionBlock::Raw(_, l) => l,
        }
    }
}

fn eval_chain(chain: &[DecompressionBlock], position: u64, length: u64) -> u64
{
    let mut size: u64 = 0;
    // handle each character of the raw base separately, to account for disjoint
    // spans
    for p in 0..length {
        let mut multiplier = 1;
        for block in chain {
            if let &DecompressionBlock::Mul(mul_start, l, m) = block {
                if mul_start + l > position + p - length {
                    multiplier *= m;
                }
            }
        }
        size += multiplier;
    }

    size
}

fn decompressed_size(txt: &str) -> u64
{
    let mut blocks = Vec::new();
    let mut position = 0;
    for block in txt.split(|x| x == '(' || x == ')') {
        position += 1 + block.len() as u64;
        if let Some((len, count)) = parse_marker(block) {
            blocks.push(DecompressionBlock::Mul(position + 1, len as u64, count as u64));
        } else {
            blocks.push(DecompressionBlock::Raw(position, block.len() as u64))
        }
    }

    let mut size: u64 = 0;
    let mut active = Vec::new();

    for block in blocks {
        position = block.position();
        if let DecompressionBlock::Raw(p, l) = block {
            size += eval_chain(&active, p, l);
        } else {
            active.push(block);
        }

        active.retain(|x| x.length() + x.position() > position)
    }

    size
}

struct Decompressor<'a>
{
    posn: usize,
    txt: &'a str,
    len: usize,
    ct: usize,
}

impl<'a> Iterator for Decompressor<'a>
{
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str>
    {
        if self.posn == self.txt.len() {
            return None;
        }
        if self.ct > 0 {
            let block = &self.txt[self.posn..self.posn + self.len];
            self.ct -= 1;
            if self.ct == 0 {
                self.posn += self.len;
                self.len = 0;
            }
            return Some(block);
        }

        // find the start of the next marker
        // and then set the marker
        // and then yield preceding block

        let next_marker_pos = match (&self.txt[self.posn..]).find('(') {
            Some(n) => n,
            None => {
                let rest = &self.txt[self.posn..];
                self.posn = self.txt.len();
                if rest.is_empty() {
                    return None;
                } else {
                    return Some(rest);
                }
            },

        };

        let marker_len = (&self.txt[self.posn + next_marker_pos..]).find(')').unwrap();
        let (len, count) = parse_marker(&self.txt[self.posn + next_marker_pos..self.posn + next_marker_pos +
                                                                               marker_len])
                               .unwrap();
        self.ct = count;
        self.len = len;
        let pre_block = &self.txt[self.posn..self.posn + next_marker_pos];
        self.posn += next_marker_pos + marker_len + 1; // skip ending ')'

        // for example, if we start with a marker
        if pre_block.is_empty() {
            self.ct -= 1;
            Some(&self.txt[self.posn..self.posn + self.len])
        } else {
            Some(pre_block)
        }
    }
}

impl<'a> Decompressor<'a>
{
    fn new(src: &'a str) -> Decompressor<'a>
    {
        Decompressor {
            txt: src,
            len: 0,
            ct: 0,
            posn: 0,
        }

    }
}

fn parse_marker(txt: &str) -> Option<(usize, usize)>
{
    let src = if txt.starts_with('(') {
        &txt[1..]
    } else {
        &txt[..]
    };

    let split = if let Some(n) = src.find('x') {
        n
    } else {
        return None;
    };
    let len = if let Ok(n) = usize::from_str_radix(&src[0..split], 10) {
        n
    } else {
        return None;
    };
    let count = if let Ok(n) = usize::from_str_radix(&src[split + 1..], 10) {
        n
    } else {
        return None;
    };
    Some((len, count))
}

#[test]
fn test_decompressor()
{
    let test1 = "ADVENT";
    let d1 = Decompressor::new(test1);
    let o1: Vec<_> = d1.collect();
    assert!(o1 == vec!["ADVENT"]);
    let test2 = "A(1x5)BC";
    let d2 = Decompressor::new(test2);
    let o2: Vec<_> = d2.collect();
    assert!(o2 == vec!["A", "B", "B", "B", "B", "B", "C"]);
    let test3 = "(3x3)XYZ";
    let d3 = Decompressor::new(test3);
    let o3: Vec<_> = d3.collect();
    assert!(o3 == vec!["XYZ", "XYZ", "XYZ"]);
    let d4 = Decompressor::new("A(2x2)BCD(2x2)EFG");
    let o4: Vec<_> = d4.collect();
    assert!(o4 == vec!["A", "BC", "BC", "D", "EF", "EF", "G"]);
    let o5 = decompressed_size("(3x3)XYZ");
    assert!(o5 == 9);
    let o6 = decompressed_size("X(8x2)(3x3)ABCY");
    assert!(o6 == 20);
    assert!(decompressed_size("(27x12)(20x12)(13x14)(7x10)(1x12)A") == 241920);
    let o7 = decompressed_size("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN");
    assert!(o7 == 445);
}

struct Bot
{
    id: u32,
    chip1: Option<u32>,
    chip2: Option<u32>,
    high: BotTarget,
    low: BotTarget,
}

#[derive(Clone, Copy, Debug)]
enum BotTarget
{
    Bot(u32),
    Output(u32),
    None,
}

#[derive(Clone, Copy, Debug)]
struct BotComparison
{
    bot: u32,
    low: u32,
    high: u32,
}

impl Bot
{
    fn new(n: u32) -> Bot
    {
        Bot {
            id: n,
            chip1: None,
            chip2: None,
            high: BotTarget::None,
            low: BotTarget::None,
        }
    }
    fn chip(&mut self, c: u32) -> &mut Bot
    {
        if self.chip1.is_none() {
            self.chip1 = Some(c);
        } else if self.chip2.is_none() {
            self.chip2 = Some(c);
        } else {
            println!("Warning: trying to pass a chip to a bot that already has two chips!");
        }

        self

    }
    fn high(&mut self, c: BotTarget) -> &mut Bot
    {
        self.high = c;
        self
    }
    fn low(&mut self, c: BotTarget) -> &mut Bot
    {
        self.low = c;
        self
    }
}

fn set_up_bots(bot_map: &mut BTreeMap<u32, Bot>, logic: &[day10::Logic])
{
    for l in logic {
        match *l {
            day10::Logic::Give(bot_id, chip) => {
                let bot = get_or_insert_mut(bot_map, &bot_id, Bot::new(bot_id));
                bot.chip(chip);
            },
            day10::Logic::High(src, dest) => {
                let bot = get_or_insert_mut(bot_map, &src, Bot::new(src));
                bot.high(BotTarget::Bot(dest));
            },
            day10::Logic::Low(src, dest) => {
                let bot = get_or_insert_mut(bot_map, &src, Bot::new(src));
                bot.low(BotTarget::Bot(dest));
            },
            day10::Logic::OutputHigh(src, dest) => {
                let bot = get_or_insert_mut(bot_map, &src, Bot::new(src));
                bot.high(BotTarget::Output(dest));
            },
            day10::Logic::OutputLow(src, dest) => {
                let bot = get_or_insert_mut(bot_map, &src, Bot::new(src));
                bot.low(BotTarget::Output(dest));
            },
        }
    }
}

fn eval_bots(bot_map: &mut BTreeMap<u32, Bot>,
             outputs: &mut BTreeMap<u32, Vec<u32>>,
             comparisons: &mut Vec<BotComparison>)
{

    struct Movement
    {
        src: u32,
        dest: BotTarget,
        chip: u32,
    }

    let mut movements = Vec::new();

    let mut complete = false;
    while !complete {
        complete = true;
        movements.clear();

        for bot in bot_map.values_mut() {
            if bot.chip1.is_some() && bot.chip2.is_some() {
                // we have carried out some comparison in this iteration
                complete = false;

                let (high, low) = match (bot.chip1.take(), bot.chip2.take()) {
                    (Some(x), Some(y)) if x > y => (x, y),
                    (Some(x), Some(y)) => (y, x),
                    _ => unreachable!(),
                };

                comparisons.push(BotComparison {
                    bot: bot.id,
                    high: high,
                    low: low,
                });

                movements.push(Movement {
                    src: bot.id,
                    dest: bot.high,
                    chip: high,
                });
                movements.push(Movement {
                    src: bot.id,
                    dest: bot.low,
                    chip: low,
                });

            }
        }

        for &Movement { ref src, ref dest, ref chip } in &movements {
            match *dest {
                BotTarget::Bot(b) => {
                    if let Some(bot) = bot_map.get_mut(&b) {
                        bot.chip(*chip);
                    } else {
                        println!("Error: Bot #{} wants to pass high chip to Bot#{}, but it does not exist!",
                                 src,
                                 b);
                    }
                },

                BotTarget::Output(o) => {
                    let output = get_or_insert_mut(outputs, &o, Vec::new());
                    output.push(*chip);
                },

                BotTarget::None => {
                    return println!("Error: Bot#{} is moving chip #{}, but has no destination!",
                                    src,
                                    chip);
                },
            }
        }
    }
}

#[test]
fn test_bots()
{
    let logic = [day10::Logic::Give(2, 5),
                 day10::Logic::Low(2, 1),
                 day10::Logic::High(2, 0),
                 day10::Logic::Give(1, 3),
                 day10::Logic::OutputLow(1, 1),
                 day10::Logic::High(1, 0),
                 day10::Logic::OutputLow(0, 2),
                 day10::Logic::OutputHigh(0, 0),
                 day10::Logic::Give(2, 2)];
    let mut bots = BTreeMap::new();
    let mut outputs = BTreeMap::new();
    let mut comparisons = Vec::new();

    set_up_bots(&mut bots, &logic[..]);
    eval_bots(&mut bots,
              &mut outputs,
              &mut comparisons);
    println!("{:?}", outputs);
    println!("{:?}", comparisons);
    let mut expected_outputs = BTreeMap::new();
    expected_outputs.insert(0, vec![5]);
    expected_outputs.insert(1, vec![2]);
    expected_outputs.insert(2, vec![3]);
    assert!(outputs == expected_outputs)
}

fn day10()
{
    let mut bots = BTreeMap::new();
    let mut outputs = BTreeMap::new();
    let mut comparisons = Vec::new();

    set_up_bots(&mut bots, &day10::LOGIC[..]);
    eval_bots(&mut bots,
              &mut outputs,
              &mut comparisons);

    for cmp in comparisons {
        if cmp.high == 61 && cmp.low == 17 {
            print!("Bot comparing 61 to 17: #{} ", cmp.bot);
        }
    }

    let mut x: u32 = *outputs.get(&0).unwrap().get(0).unwrap();
    x *= *outputs.get(&1).unwrap().get(0).unwrap();
    x *= *outputs.get(&2).unwrap().get(0).unwrap();

    println!("Product of first three bins: {}", x);
}

#[derive(Clone, Debug)]
struct FloorPlan
{
    po: [u8; 2],
    th: [u8; 2],
    pr: [u8; 2],
    ru: [u8; 2],
    co: [u8; 2],
    el: [u8; 2],
    di: [u8; 2],
    fl: u8,
    steps: u32,
}

impl PartialEq for FloorPlan
{
    fn eq(&self, other: &FloorPlan) -> bool
    {
        cmp::Ordering::Equal == self.cmp(other)
    }
}

impl PartialOrd for FloorPlan
{
    fn partial_cmp(&self, other: &FloorPlan) -> Option<cmp::Ordering>
    {
        Some(self.cmp(other))
    }
}

impl Ord for FloorPlan
{
    fn cmp(&self, other: &FloorPlan) -> cmp::Ordering
    {
        let mut s_psn = [(self.po[0], self.po[1]),
                         (self.th[0], self.th[1]),
                         (self.pr[0], self.pr[1]),
                         (self.ru[0], self.ru[1]),
                         (self.co[0], self.co[1]),
                         (self.el[0], self.el[1]),
                         (self.di[0], self.di[1])];
        let mut o_psn = [(other.po[0], other.po[1]),
                         (other.th[0], other.th[1]),
                         (other.pr[0], other.pr[1]),
                         (other.ru[0], other.ru[1]),
                         (other.co[0], other.co[1]),
                         (other.el[0], other.el[1]),
                         (other.di[0], other.di[1])];

        s_psn.sort();
        o_psn.sort();

        (s_psn, self.fl).cmp(&(o_psn, other.fl))
    }
}

impl Eq for FloorPlan {}

impl FloorPlan
{
    fn to_string(&self) -> String
    {
        let mut s = String::new();
        s.push('\n');
        for i in (1..5).rev() {
            s.push_str(if self.fl == i {
                "E : "
            } else {
                "  : "
            });
            s.push_str(if self.po[0] == i {
                "PoC "
            } else {
                "    "
            });
            s.push_str(if self.th[0] == i {
                "ThC "
            } else {
                "    "
            });
            s.push_str(if self.pr[0] == i {
                "PrC "
            } else {
                "    "
            });
            s.push_str(if self.ru[0] == i {
                "RuC "
            } else {
                "    "
            });
            s.push_str(if self.co[0] == i {
                "CoC "
            } else {
                "    "
            });
            s.push_str(if self.el[0] == i {
                "ElC "
            } else {
                "    "
            });
            s.push_str(if self.di[0] == i {
                "DiC "
            } else {
                "    "
            });
            s.push_str(if self.po[1] == i {
                "PoG "
            } else {
                "    "
            });
            s.push_str(if self.th[1] == i {
                "ThG "
            } else {
                "    "
            });
            s.push_str(if self.pr[1] == i {
                "PrG "
            } else {
                "    "
            });
            s.push_str(if self.ru[1] == i {
                "RuG "
            } else {
                "    "
            });
            s.push_str(if self.co[1] == i {
                "CoG "
            } else {
                "    "
            });
            s.push_str(if self.el[1] == i {
                "ElG "
            } else {
                "    "
            });
            s.push_str(if self.di[1] == i {
                "DiG "
            } else {
                "    "
            });

            s.push('\n');
        }
        s
    }

    fn is_solved(&self) -> bool
    {
        self.po[0] >= 4 && self.po[1] >= 4 && self.th[0] >= 4 && self.th[1] >= 4 && self.pr[0] >= 4 &&
        self.pr[1] >= 4 && self.ru[0] >= 4 && self.ru[1] >= 4 &&
        self.co[0] >= 4 && self.co[1] >= 4 && self.el[0] >= 4 && self.el[1] >= 4 && self.di[0] >= 4 &&
        self.di[1] >= 4
    }

    fn get_floor(&self, floor: u8) -> BitField16
    {
        let mut field = BitField16::new();
        field.set(0, self.po[0] == floor);
        field.set(1, self.th[0] == floor);
        field.set(2, self.pr[0] == floor);
        field.set(3, self.ru[0] == floor);
        field.set(4, self.co[0] == floor);
        field.set(5, self.el[0] == floor);
        field.set(6, self.di[0] == floor);
        field.set(7, self.po[1] == floor);
        field.set(8, self.th[1] == floor);
        field.set(9, self.pr[1] == floor);
        field.set(10, self.ru[1] == floor);
        field.set(11, self.co[1] == floor);
        field.set(12, self.el[1] == floor);
        field.set(13, self.di[1] == floor);

        field
    }

    fn has_fried_chips(&self) -> bool
    {
        for floor in 1..5 {
            let mut vuln = false;
            let mut has_rtg = false;
            let floor = self.get_floor(floor);
            for component in 0..7 {
                vuln |= floor.get(component) && !floor.get(component + 7);
                has_rtg |= floor.get(component + 5);
            }

            if vuln && has_rtg {
                return true;
            }
        }
        false
    }

    fn from_floor_index_mut(&mut self, index: u8) -> &mut u8
    {
        let i = if index > 6 {
            1
        } else {
            0
        };

        match index % 7 {
            0 => &mut self.po[i],
            1 => &mut self.th[i],
            2 => &mut self.pr[i],
            3 => &mut self.ru[i],
            4 => &mut self.co[i],
            5 => &mut self.el[i],
            6 => &mut self.di[i],
            _ => unreachable!(),
        }
    }


    fn from_floor_index(&self, index: u8) -> u8
    {
        let i = if index > 6 {
            1
        } else {
            0
        };

        match index % 7 {
            0 => self.po[i],
            1 => self.th[i],
            2 => self.pr[i],
            3 => self.ru[i],
            4 => self.co[i],
            5 => self.el[i],
            6 => self.di[i],
            _ => unreachable!(),
        }
    }

    fn push_adjacent(&self, v: &mut Vec<FloorPlan>)
    {
        let on_this_floor = self.get_floor(self.fl);

        for x in 0..14 {
            if !on_this_floor.get(x) {
                continue;
            }

            // move downwards
            if self.fl > 1 {
                let mut floor = self.clone();
                floor.steps += 1;
                floor.fl -= 1;
                if floor.from_floor_index(x) == 0 {
                    println!("{}", &self);
                }
                *floor.from_floor_index_mut(x) -= 1;

                for y in 0..14 {
                    if !on_this_floor.get(y) || x == y {
                        continue;
                    }

                    *floor.from_floor_index_mut(y) -= 1;
                    if !floor.has_fried_chips() {
                        v.push(floor.clone());
                    }
                    *floor.from_floor_index_mut(y) += 1;
                }

                if !floor.has_fried_chips() {
                    v.push(floor);
                }
            }

            // move upwards
            if self.fl < 4 {
                let mut floor = self.clone();
                floor.steps += 1;
                floor.fl += 1;
                *floor.from_floor_index_mut(x) += 1;

                for y in 0..14 {
                    if !on_this_floor.get(y) || x == y {
                        continue;
                    }

                    *floor.from_floor_index_mut(y) += 1;
                    if !floor.has_fried_chips() {
                        v.push(floor.clone());
                    }
                    *floor.from_floor_index_mut(y) -= 1;
                }

                if !floor.has_fried_chips() {
                    v.push(floor);
                }
            }
        }
    }

    fn compare(&self, other: &FloorPlan) -> cmp::Ordering
    {
        other.steps.cmp(&self.steps)
    }
}

impl Display for FloorPlan
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result
    {
        write!(f, "{}", self.to_string())
    }
}

fn solve_floorplan(input_plan: FloorPlan) -> u32
{
    let mut plan = input_plan;
    let mut plans = Vec::new();
    let mut seen_plans = BTreeSet::new();
    while !plan.is_solved() {
        plan.push_adjacent(&mut plans);
        seen_plans.insert(plan);
        plans.retain(|x| !seen_plans.contains(x));
        plans.as_mut_slice().sort_by(FloorPlan::compare);
        plan = plans.pop().expect("Floorplan has no solution!");
    }

    plan.steps
}

#[test]
fn test_floorplan()
{
    let starting_plan = FloorPlan {
        fl: 1,
        po: [1, 2],
        th: [1, 3],
        pr: [5, 5],
        ru: [5, 5],
        co: [5, 5],
        el: [5, 5],
        di: [5, 5],
        steps: 0,
    };

    let mut equivalent_plan = FloorPlan {
        fl: 1,
        po: [1, 3],
        th: [1, 2],
        pr: [5, 5],
        ru: [5, 5],
        co: [5, 5],
        el: [5, 5],
        di: [5, 5],
        steps: 0,
    };

    assert!(equivalent_plan == starting_plan);

    equivalent_plan.fl = 2;
    assert!(equivalent_plan != starting_plan);
    assert!(cmp::Ordering::Less == starting_plan.cmp(&equivalent_plan));
    equivalent_plan.fl = 1;

    let finished_plan = FloorPlan {
        fl: 4,
        po: [4, 4],
        th: [4, 4],
        pr: [5, 5],
        ru: [5, 5],
        co: [5, 5],
        el: [5, 5],
        di: [5, 5],
        steps: 0,
    };

    assert!(0 == solve_floorplan(finished_plan));

    let first_floor = starting_plan.get_floor(1);
    assert!(first_floor.num_set() == 2);
    assert!(first_floor.get(0));
    assert!(first_floor.get(1));
    assert!(starting_plan.from_floor_index(8) == 3);
    assert!(starting_plan.from_floor_index(7) == 2);
    assert!(starting_plan.from_floor_index(2) == 5);

    let mut plan_2 = starting_plan.clone();
    plan_2.steps += 1;

    assert!(starting_plan == plan_2);

    let fried_plan = FloorPlan {
        fl: 2,
        po: [1, 2],
        th: [2, 3],
        pr: [5, 5],
        ru: [5, 5],
        co: [5, 5],
        el: [5, 5],
        di: [5, 5],
        steps: 1,
    };

    assert!(fried_plan.has_fried_chips());
    assert!(!starting_plan.has_fried_chips());

    let mut set = BTreeSet::new();
    set.insert(plan_2);
    assert!(set.contains(&equivalent_plan));

    let mut adj = Vec::new();
    starting_plan.push_adjacent(&mut adj);
    adj.sort_by(FloorPlan::compare);

    let mut adj_ = Vec::new();
    adj_.push(FloorPlan {
        fl: 2,
        po: [2, 2],
        th: [1, 3],
        pr: [5, 5],
        ru: [5, 5],
        co: [5, 5],
        el: [5, 5],
        di: [5, 5],
        steps: 1,
    });

    assert!((FloorPlan {
                po: [1, 2],
                th: [2, 3],
                pr: [5, 5],
                ru: [5, 5],
                co: [5, 5],
                el: [5, 5],
                di: [5, 5],
                fl: 2,
                steps: 1,
            })
            .has_fried_chips());

    println!("{:?}", adj);
    assert!(adj == adj_);

    let num_steps = solve_floorplan(starting_plan);
    println!("{}", num_steps);
    assert!(num_steps == 11);

    // this test is valuable, but very very slow
    // so it is disabled
    //
    // let second_plan = FloorPlan {
    // fl: 1,
    // po: [2, 1],
    // th: [1, 1],
    // pr: [2, 1],
    // ru: [1, 1],
    // co: [1, 1],
    // el: [5, 5],
    // di: [5, 5],
    // steps: 0,
    // };
    //
    // let num_steps2 = solve_floorplan(second_plan);
    // assert!(num_steps2 == 47);
    //
}

fn day11()
{
    let first_plan = FloorPlan {
        fl: 1,
        po: [2, 1],
        th: [1, 1],
        pr: [2, 1],
        ru: [1, 1],
        co: [1, 1],
        el: [5, 5],
        di: [5, 5],
        steps: 0,
    };

    let second_plan = FloorPlan {
        fl: 1,
        po: [2, 1],
        th: [1, 1],
        pr: [2, 1],
        ru: [1, 1],
        co: [1, 1],
        el: [1, 1],
        di: [1, 1],
        steps: 0,
    };

    println!("Minimum number of steps: {}, including the extra RTGS: {}",
             solve_floorplan(first_plan),
             solve_floorplan(second_plan));
}

fn eval_asb(asb: &[day12::ASB], regs: &mut [i64; 4])
{
    let mut ip: usize = 0;
    while ip < asb.len() {
        ip = asb_step(asb, regs, ip);
    }
}

fn asb_step(asb: &[day12::ASB], regs: &mut [i64; 4], ip: usize) -> usize
{
    if ip > asb.len() {
        return ip;
    }

    match asb[ip] {
        day12::ASB::Cpy(v, r) => regs[asb_reg_idx(r)] = asb_val(v, regs),
        day12::ASB::Inc(r) => regs[asb_reg_idx(r)] += 1,
        day12::ASB::Dec(r) => regs[asb_reg_idx(r)] -= 1,
        day12::ASB::JNZ(v, x) if asb_val(v, regs) != 0 => {
            return if x < 0 {
                ip - x.abs() as usize
            } else {
                ip + x as usize
            }
        },
        day12::ASB::JNZ(_, _) => (),
    }

    ip + 1
}

fn asb_reg_idx(x: day12::Reg) -> usize
{
    match x {
        day12::Reg::A => 0,
        day12::Reg::B => 1,
        day12::Reg::C => 2,
        day12::Reg::D => 3,
    }
}

fn asb_val(v: day12::Val, regs: &[i64; 4]) -> i64
{
    match v {
        day12::Val::Reg(r) => regs[asb_reg_idx(r)],
        day12::Val::Lit(x) => x,
    }
}

enum ASB
{
    MovL(i64, usize),
    MovR(usize, usize),
    Jmp(i32),
    Jnz(usize, i32),
    Add(usize, usize), // adds y to x, sets y to zero
    Sub(usize, usize), // subs y from x, sets y to zero
    NoOp,
}

fn asb_opt(src: &[day12::ASB]) -> Vec<ASB>
{
    let mut v = Vec::new();
    let mut skip = 0;
    for w in src.windows(3) {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        match w {
            &[day12::ASB::Inc(r1), day12::ASB::Dec(r2), day12::ASB::JNZ(r3, -2)] if r2 == r3 => {
                v.push(ASB::NoOp);
                v.push(ASB::NoOp);
                v.push(ASB::Add(asb_reg_idx(r1), asb_reg_idx(r2)));
                skip = 2;
            },

            &[day12::ASB::Inc(r1), day12::ASB::Dec(r2), day12::ASB::JNZ(r3, -2)] if r1 == r3 => {
                v.push(ASB::NoOp);
                v.push(ASB::NoOp);
                v.push(ASB::Sub(asb_reg_idx(r1), asb_reg_idx(r2)));
                skip = 2;
            },

            &[day12::ASB::Dec(r2), day12::ASB::Inc(r1), day12::ASB::JNZ(r3, -2)] if r2 == r3 => {
                v.push(ASB::NoOp);
                v.push(ASB::NoOp);
                v.push(ASB::Add(asb_reg_idx(r1), asb_reg_idx(r2)));
                skip = 2;
            },

            &[day12::ASB::Dec(r2), day12::ASB::Inc(r1), day12::ASB::JNZ(r3, -2)] if r1 == r3 => {
                v.push(ASB::NoOp);
                v.push(ASB::NoOp);
                v.push(ASB::Sub(asb_reg_idx(r1), asb_reg_idx(r2)));
                skip = 2;
            },

            &[day12::ASB::JNZ(day12::Val::Lit(x), y), _, _] => {
                if x != 0 {
                    v.push(ASB::Jmp(y));
                } else {
                    v.push(ASB::NoOp);
                }
            },

            &[day12::ASB::Cpy(day12::Val::Lit(x), r), _, _] => {
                v.push(ASB::MovL(x, asb_reg_idx(r)));
            },

            &[day12::ASB::Cpy(day12::Val::Reg(r1), r2), _, _] => {
                v.push(ASB::MovR(asb_reg_idx(r1), asb_reg_idx(r2)));
            },
        }
    }
    v
}

#[test]
fn test_asb()
{
    let mut regs = [0; 4];
    let code = [day12::ASB::Cpy(day12::Val::Lit(41), day12::Reg::A),
                day12::ASB::Inc(day12::Reg::A),
                day12::ASB::Inc(day12::Reg::A),
                day12::ASB::Dec(day12::Reg::A),
                day12::ASB::JNZ(day12::Val::Reg(day12::Reg::A), 2),
                day12::ASB::Dec(day12::Reg::A)];

    let mut ip = 0;
    assert!(ip == 0 && regs == [0, 0, 0, 0]);
    ip = asb_step(&code[..], &mut regs, ip);
    assert!(ip == 1 && regs == [41, 0, 0, 0]);
    ip = asb_step(&code[..], &mut regs, ip);
    assert!(ip == 2 && regs == [42, 0, 0, 0]);
    ip = asb_step(&code[..], &mut regs, ip);
    assert!(ip == 3 && regs == [43, 0, 0, 0]);
    ip = asb_step(&code[..], &mut regs, ip);
    assert!(ip == 4 && regs == [42, 0, 0, 0]);
    ip = asb_step(&code[..], &mut regs, ip);
    assert!(ip == 6 && regs == [42, 0, 0, 0]);

    assert!(1 ==
            asb_step(&[day12::ASB::JNZ(day12::Val::Lit(1), -1); 3][..],
                     &mut [0, 0, 0, 0],
                     2));
    asb_step(&[day12::ASB::Cpy(day12::Val::Reg(day12::Reg::A),
                               day12::Reg::B)][..],
             &mut regs,
             0);
    assert!(regs == [42, 42, 0, 0]);

}

fn day12()
{
    let mut regs = [0; 4];
    eval_asb(&day12::CODE[..], &mut regs);
    println!("Registers: {:?}", regs);
    println!("Value of register A: {}", regs[0]);
}
fn day13()
{
}
fn day14()
{
}
fn day15()
{
}
fn day16()
{
}
fn day17()
{
}
fn day18()
{
}
fn day19()
{
}
fn day20()
{
}
fn day21()
{
}
fn day22()
{
}
fn day23()
{
}
fn day24()
{
}
fn day25()
{
}

struct StrWindow<'a>
{
    base: &'a str,
    size: usize,
    posn: usize,
}

impl<'a> StrWindow<'a>
{
    fn windows(base: &str, size: usize) -> StrWindow
    {
        StrWindow {
            base: base,
            size: size,
            posn: 0,
        }
    }
}

fn windows(s: &str, n: usize) -> StrWindow
{
    StrWindow::windows(s, n)
}

impl<'a> Iterator for StrWindow<'a>
{
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str>
    {
        if self.size + self.posn > self.base.len() {
            return None;

        }

        let old_posn = self.posn;
        self.posn += 1;

        return Some(&self.base[old_posn..old_posn + self.size]);
    }
}

#[test]
fn test_windows()
{
    let s: &'static str = "abcdefghi";
    let w: [&'static str; 6] = ["abcd", "bcde", "cdef", "defg", "efgh", "fghi"];
    assert!(windows(s, 4).zip(w[..].iter()).fold(true, |x, (y, z)| x && (&y == z)))
}

fn palindrome(txt: &str) -> bool
{
    txt.chars()
       .zip(txt.chars().rev())
       .fold(true, |acc, (x, y)| acc && (x == y))
}

#[allow(dead_code)]
fn write_hex_str(bytes: &[u8], out: &mut String)
{
    let hex_chars: [char; 16] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
    for &byte in bytes {
        out.push(hex_chars[((byte & 0xf0) >> 4) as usize]);
        out.push(hex_chars[(byte & 0x0f) as usize]);
    }
}

#[allow(dead_code)]
fn to_hex_str(bytes: &[u8]) -> String
{
    let mut s = String::with_capacity(bytes.len());
    write_hex_str(bytes, &mut s);
    s
}

fn get_or_insert_mut<'a, 'b: 'a, S, T>(map: &'a mut BTreeMap<S, T>, key: &'b S, alt: T) -> &'a mut T
    where S: Ord + Clone
{
    if map.contains_key(key) {
        map.get_mut(key).unwrap()
    } else {
        map.insert(key.clone(), alt);
        map.get_mut(key).unwrap()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct BitField32(u32);
impl BitField32
{
    fn new() -> BitField32
    {
        BitField32(0)
    }
    fn set(&mut self, idx: u8, val: bool) -> bool
    {
        if idx > 31 {
            return false;
        }

        let mask = 1u32 << idx;
        let old_val = (self.0 & mask) > 0;
        if val {
            self.0 = self.0 | mask;
        } else {
            self.0 = self.0 & !mask;
        }
        old_val
    }

    fn get(&self, idx: u8) -> bool
    {
        if idx > 31 {
            return false;
        }
        let mask = 1u32 << idx;
        (self.0 & mask) > 0
    }

    fn clear(&mut self)
    {
        self.0 = 0;
    }
}

#[test]
fn test_bitfield32()
{
    let mut field = BitField32::new();
    assert!(!field.get(10));
    field.set(3, true);
    assert!(field.get(3));
    assert!(field.0 == 8);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct BitField16(u16);
impl BitField16
{
    fn new() -> BitField16
    {
        BitField16(0)
    }
    fn set(&mut self, idx: u8, val: bool) -> bool
    {
        if idx > 15 {
            return false;
        }

        let mask = 1u16 << idx;
        let old_val = (self.0 & mask) > 0;
        if val {
            self.0 = self.0 | mask;
        } else {
            self.0 = self.0 & !mask;
        }
        old_val
    }

    fn get(&self, idx: u8) -> bool
    {
        if idx > 15 {
            return false;
        }
        let mask = 1u16 << idx;
        (self.0 & mask) > 0
    }

    fn clear(&mut self)
    {
        self.0 = 0;
    }

    fn num_set(&self) -> u8
    {
        self.0.count_ones() as u8
    }

    fn or(&mut self, other: &BitField16)
    {
        self.0 = self.0 | other.0;
    }
}

#[test]
fn test_bitfield16()
{
    let mut field = BitField16::new();
    assert!(!field.get(10));
    field.set(3, true);
    assert!(field.get(3));
    assert!(field.0 == 8);
}

fn fac(n: u16) -> u64
{
    let mut prd = 1u64;
    for x in 0..n {
        prd *= x as u64;
    }

    prd
}

fn choose(n: u16, k: u16) -> u64
{
    if k > n {
        return 0;
    }
    fac(n) / (fac(k) * fac(n - k))
}
