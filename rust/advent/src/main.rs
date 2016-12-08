extern crate crypto;
use crypto::digest::Digest;
use crypto::md5;

use std::collections::{HashSet, HashMap};
use std::fmt::{Formatter, Display};

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
        *self = match *self
        {
            D::N => D::W,
            D::W => D::S,
            D::S => D::E,
            D::E => D::N,
        }
    }
    fn right(&mut self)
    {
        *self = match *self
        {
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
               match *self
               {
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
        let len = if let Ok(n) = i64::from_str_radix(&instr[1..], 10)
        {
            n
        }
        else
        {
            return None;
        };

        if instr.starts_with('R')
        {
            self.dir.right()
        }
        else if instr.starts_with('L')
        {
            self.dir.left()
        }

        let mut first_revisited = None;

        for _ in 0..len
        {
            match self.dir
            {
                D::N => self.y += 1,
                D::S => self.y -= 1,
                D::E => self.x += 1,
                D::W => self.x -= 1,
            }

            if !visited.insert((self.x, self.y)) && first_revisited.is_none()
            {
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

    for instr in day1::DIRECTIONS.split(' ')
    {
        match loc.movn(instr, &mut visited)
        {
            Some((x, y)) if !revisited =>
            {
                print!("First revisited at: {}; ",
                       i64::abs(x) + i64::abs(y));
                revisited = true;
            },
            _ =>
            {},
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


    for line in day2::DIRECTIONS.lines()
    {
        for c in line.chars()
        {
            match c
            {
                'U' if loc.y > 0 =>
                {
                    loc.y -= 1;
                },
                'D' if loc.y < 2 =>
                {
                    loc.y += 1;
                },
                'L' if loc.x > 0 =>
                {
                    loc.x -= 1;
                },
                'R' if loc.x < 2 =>
                {
                    loc.x += 1;
                },
                _ =>
                {},
            }

            match c
            {
                'U' =>
                {
                    if y == 0 || day2::PAD[y - 1][x].is_none()
                    {
                        continue;
                    }
                    y -= 1;
                },
                'R' =>
                {
                    if x == 4 || day2::PAD[y][x + 1].is_none()
                    {
                        continue;
                    }
                    x += 1;
                },
                'D' =>
                {
                    if y == 4 || day2::PAD[y + 1][x].is_none()
                    {
                        continue;
                    }
                    y += 1;
                },
                'L' =>
                {
                    if x == 0 || day2::PAD[y][x - 1].is_none()
                    {
                        continue;
                    };
                    x -= 1;
                },
                _ =>
                {},
            }
        }

        if let Some(c) = day2::PAD[y][x]
        {
            code.push(c);
        }

        code_simple.push((49 + (3 * loc.y) + loc.x) as u8 as char);
    }

    println!("{}, {}", code_simple, code);
}

fn day3()
{

    let mut t: [u64; 3] = [0; 3];
    let mut valid_triangles: u64 = 0;

    for triangle in &day3::TRIANGLES[..]
    {
        t.copy_from_slice(triangle);
        t.sort();
        if t[2] < t[0] + t[1]
        {
            valid_triangles += 1;
        }
    }

    let mut bonus_valid_triangles: u64 = 0;

    for triangles in day3::TRIANGLES[..].chunks(3)
    {
        for col in 0..3
        {
            t[0] = triangles[0][col];
            t[1] = triangles[1][col];
            t[2] = triangles[2][col];

            t.sort();

            if t[2] < t[0] + t[1]
            {
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

    for room_spec in day4::ROOMS.lines()
    {
        letter_freqs.clear();
        letters.clear();
        decryped_name.clear();
        let room_id = day4_inner(room_spec,
                                 &mut letters,
                                 &mut letter_freqs,
                                 &mut decryped_name);
        //if room_id != 0
        //{
        //    println!("Found real room #{} with name {}", room_id, decryped_name);
        //}
        if decryped_name == "northpole object storage"
        {
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

    shift_letters("qzmt-zixmtkozy-ivhz".as_bytes(),
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

    let name_end_idx = if let Some(ix) = room_spec.rfind('-')
    {
        ix
    }
    else
    {
        return 0;
    };

    let name_str = &room_spec[0..name_end_idx];

    for letter in name_str.chars()
    {
        if letter == '-'
        {
            continue;
        }

        if letters.contains_key(&letter)
        {
            letters.get_mut(&letter).map(|x| *x -= 1);
        }
        else
        {
            letters.insert(letter, -1);
        }
    }


    for (key, value) in letters.drain()
    {
        letter_freqs.push((value, key));
    }

    letter_freqs.sort();

    let code_idx = if let Some(idx) = room_spec.find('[')
    {
        idx
    }
    else
    {
        return 0;
    };

    let room_number = if let Ok(x) = u32::from_str_radix(&room_spec[name_end_idx + 1..code_idx],
                                                         10)
    {
        x
    }
    else
    {
        println!("Parse error!");
        return 0;
    };

    for (&(_, x), y) in letter_freqs.iter().zip(room_spec[1 + code_idx..].chars()).take(5)
    {
        if x != y
        {
            return 0;
        }
    }

    shift_letters(&room_spec[0..name_end_idx].as_bytes(),
                  room_number,
                  decryped_name);
    return room_number;
}

fn shift_letters(txt: &[u8], shift: u32, buf: &mut String)
{
    let normalised_shift: u8 = (shift % 26) as u8;

    for &c in txt
    {
        if c == ('-' as u8)
        {
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
        if buf.len() < 3
        {
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

    while (password.len() < 8) || pass2_num_chars < 8
    {
        for x in out_buffer.iter_mut()
        {
            *x = 0;
        }

        plaintext.clear();
        md5.reset();
        plaintext.push_str(day5::ROOM_NUMBER);
        plaintext.push_str(&index.to_string());
        md5.input(plaintext.as_bytes());
        md5.result(&mut out_buffer);

        if first_five_zero(&out_buffer[..])
        {
            if password.len() < 8
            {
                let c: char = hex_chars[(out_buffer[2] & 0x0f) as usize];
                password.push(c);
            }

            let index = (out_buffer[2] & 0x0f) as usize;
            let c2: char = hex_chars[((out_buffer[3] & 0xf0) >> 4) as usize];
            if (index < 8) && password2[index] == ' '
            {
                password2[index] = c2;
                pass2_num_chars += 1;
            }
        }

        index += 1;
    }

    for &c in &password2[..]
    {
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

    for msg in &day6::MESSAGES[..]
    {
        for (i, c) in msg.chars().enumerate()
        {
            assert!(i < 8);
            if hashmaps[i].contains_key(&c)
            {
                hashmaps[i].get_mut(&c).map(|x| *x += 1);
            }
            else
            {
                hashmaps[i].insert(c, 1);
            }
        }
    }
    for map in &mut hashmaps[..]
    {
        let mut max_count = 0;
        let mut max_char = ' ';
        let mut min_count = 1000;
        let mut min_char = ' ';

        for (c, n) in map.drain()
        {
            if n > max_count
            {
                max_char = c;
                max_count = n;
            }

            if n < min_count
            {
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

    for &ip in &day7::IPs[..]
    {
        if supports_tls(ip)
        {
            tls_count += 1;
        }

        if supports_ssl(ip)
        {
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

    for w in windows(ip, 4)
    {
        // filter out transitionary windows
        if w.contains('[')
        {
            is_inner = true;
            continue;
        }

        if w.contains(']')
        {
            is_inner = false;
            continue;
        }

        if is_inner
        {
            inner_palindrome |= palindrome(w) &&
                                w.chars()
                .take(2)
                .fold((true, ' '),
                      |(x, y), c| (x && (y != c), c))
                .0;
        }
        else
        {
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

    fn invert(w: &str, buf: &mut String) -> Result<(), ()>
    {
        if w.len() != 3
        {
            return Err(());
        }
        let mut iter = w.chars();
        buf.clear();
        let inner = iter.next().unwrap();
        let outer = iter.next().unwrap();
        buf.push(outer);
        buf.push(inner);
        buf.push(outer);

        Ok(())
    }


    let mut inner_sections = Vec::new();
    let mut outer_sections = Vec::new();
    let mut bab: String = String::with_capacity(3);
    let mut inner = false;
    for w in windows(ip, 3)
    {
        if w.contains('[')
        {
            inner = true;
            continue;
        }

        if w.contains(']')
        {
            inner = false;
            continue;
        }
        if aba(w)
        {
            if inner
            {
                inner_sections.push(w);
            }
            else
            {
                outer_sections.push(w);
            }
        }
    }

    for section in outer_sections
    {
        for inner_section in &inner_sections
        {
            invert(inner_section, &mut bab);
            if section == bab
            {
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
    for instr in &day8::INSTRUCTIONS[..]
    {
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
        if r >= self.height
        {
            return;
        }

        let row: &mut [bool; 50] = &mut self.data[r];
        let buf: &mut [bool; 50] = &mut self.buf;
        let w: usize = self.width;

        for i in 0..w
        {
            buf[(i + s) % w] = row[i];
        }

        for i in 0..w
        {
            row[i] = buf[i];
        }
    }

    fn rc(&mut self, c: usize, s: usize)
    {
        if c >= self.width
        {
            return;
        }
        let h: usize = self.height;

        for i in 0..h
        {
            self.buf[(i + s) % h] = self.data[i][c];
        }

        for i in 0..h
        {
            self.data[i][c] = self.buf[i];
        }
    }

    fn sq(&mut self, a: usize, b: usize)
    {
        let h = self.height;
        let w = self.width;
        if w < a || h < b
        {
            return;
        }
        for row in &mut self.data[0..b]
        {
            for val in &mut row[0..a]
            {
                *val = true;
            }
        }
    }

    fn cl(&mut self)
    {
        for row in &mut self.data[..]
        {
            for val in &mut row[..]
            {
                *val = false;
            }
        }

        for val in &mut self.buf[..]
        {
            *val = false;
        }
    }

    fn pr(&self)
    {
        let mut s = String::with_capacity((self.width + 1) * self.height);

        for row in &self.data[0..self.height]
        {
            for val in &row[0..self.width]
            {
                s.push(if *val { '#' } else { ' ' });
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
        match *i
        {
            day8::Instr::RRow(row, shift) => self.rr(row, shift),
            day8::Instr::RCol(col, shift) => self.rc(col, shift),
            day8::Instr::Rect(w, h) => self.sq(w, h),
        }
    }

    fn ct(&self) -> u64
    {
        let mut x = 0;
        for row in &self.data[0..self.height]
        {
            for val in &row[0..self.width]
            {
                if *val
                {
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
}
fn day10()
{
}
fn day11()
{
}
fn day12()
{
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
        if self.size + self.posn > self.base.len()
        {
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

fn write_hex_str(bytes: &[u8], out: &mut String)
{
    let hex_chars: [char; 16] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
    for &byte in bytes
    {
        out.push(hex_chars[((byte & 0xf0) >> 4) as usize]);
        out.push(hex_chars[(byte & 0x0f) as usize]);
    }
}

fn to_hex_str(bytes: &[u8]) -> String
{
    let mut s = String::with_capacity(bytes.len());
    write_hex_str(bytes, &mut s);
    s
}
