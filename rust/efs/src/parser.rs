#![feature(slice_patterns, advanced_slice_patterns)]

use std::collections::{VecDeque, BTreeMap};
use std::str::Chars;
use std::path::Path;
use std::fs::File;
use std::io::{Read, Write};
use std::error::Error;

use defs::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum LexMode
{
    Unknown,
    Num,
    Ident,
    String
}

struct Scanner<'a>
{
    buf : &'a mut [char], // charbuffer for partial matching
    ix_e : usize, //<'a current end of the buffer
    store : VecDeque<Token>, // stack of tokens for backtracking
    stream : Chars<'a>, // char stream
    mode : LexMode
}

impl<'a> Iterator for Scanner<'a>
{
    type Item = Token;
    
    fn next(&mut self) -> Option<Token>
    {
        if self.store.len() > 0
        {
            return self.store.pop_front();
        }

        loop
        {
            match self.stream.next()
            {
                Some(c) =>
                {
                    match matches(&self.buf[..self.ix_e], c, self.mode)
                    {
                        Ok((Token::Placeholder, m)) =>
                        {
                            self.buf[self.ix_e] = c;
                            self.ix_e += 1;
                            self.mode = m;
                        },

                        Ok((t, m)) =>
                        {
                            self.buf[0] = c;
                            self.ix_e = 1;
                            self.mode = m;
                            return Some(t)
                        },

                        Err(e) =>
                        {
                            println!("Lex error: {}", e.desc);
                            return None
                        }
                    }
                },

                _ => return None
            }
        }
    }
}

impl<'a> Scanner<'a>
{
    fn new(buf : &'a mut [char], stream : Chars<'a>) -> Scanner<'a>
    {
        Scanner
        {
            buf: buf,
            ix_e: 0,
            store: VecDeque::new(),
            stream: stream,
            mode: LexMode::Unknown
        }
    }

    fn put_back(&mut self, t : Token)
    {
        self.store.push_front(t);
    }

    fn set_mode(&mut self, m : LexMode)
    {
        self.mode = m;
    }
}
fn matches(src : &[char], peek : char, mode : LexMode) -> PResult<(Token, LexMode)>
{
    match mode
    {
        // other identifier currently in buffer
        LexMode::Ident =>
        {
            if peek.is_alphabetic() || peek == '_'
            {
                Ok((Token::Placeholder, mode))
            }
            else
            {
                Ok((Token::Ident(to_string(src)), LexMode::Unknown))
            }
        },

        // numeric code in buffer
        LexMode::Num =>
        {
            if peek.is_digit(10) || peek == '.'
            {
                // not done reading input
                Ok((Token::Placeholder, mode))
            }

            else
            {
                match (parse_int(src), parse_float(src), parse_date(src))
                {
                    (Some(i), _, _) => Ok((Token::Int(i), LexMode::Unknown)),
                    (None, Some(f), _) => Ok((Token::Float(f), LexMode::Unknown)),
                    (None, None, Some((y,m,d))) => Ok((Token::Date(y,m,d),
                                                       LexMode::Unknown)),
                    (None, None, None) =>
                    {
                        println!("Error parsing numeric buffer: {:?}", src);
                        Err(PError::new("Could not parse buffer as number. ", None, None))
                    }
                }
            }
            
        },

        // first char is ", accept everything until corresponding "
        LexMode::String =>
        {
            // actually wait to see the " in the buffer
            // before throwing it away
            // otherwise could fool the lexer into thinking
            // everything is a new string
            if src.len() > 1 && src[src.len() - 1] == '"'
            {
                Ok((Token::Str(to_string(&src[1..src.len() -1])), LexMode::Unknown))
            }
            else
            {
                Ok((Token::Placeholder, mode))
            }
        },

        // indeterminate token
        // returning a real token just from the peek does still dispose
        // of the peek char 
        LexMode::Unknown =>
        {
            if src.len() == 0
            {
                match peek
                {
                    '{' => Ok((Token::Placeholder, mode)),
                    '}' => Ok((Token::Placeholder, mode)),
                    '=' => Ok((Token::Placeholder, mode)),
                    '-' => Ok((Token::Placeholder, mode)),
                    c if c.is_whitespace() =>
                    {
                        Ok((Token::Placeholder, mode))
                    },
                    '"' => Ok((Token::Placeholder, LexMode::String)),
                    c if c.is_digit(10) =>
                    {
                        Ok((Token::Placeholder, LexMode::Num))
                    },
                    _ => Ok((Token::Placeholder, LexMode::Ident))
                }
            }
            else
            {
                let next_mode : LexMode = match peek
                {
                    c if c.is_digit(10) => LexMode::Num,
                    '{' |
                    '}' |
                    '=' => LexMode::Unknown,
                    '-' => LexMode::Unknown,
                    c if c.is_whitespace() => LexMode::Unknown,
                    '"' => LexMode::String,
                    _ => LexMode::Ident
                };
                
                match src
                {
                    // when returning a token from Unknown, also have to match
                    // the next token and set the mode!
                    // since aside from right at the start
                    // the lexer will never have an empty buffer
                    
                    ['{'] => Ok((Token::OpenBrace, next_mode)),
                    ['}'] => Ok((Token::CloseBrace, next_mode)),
                    ['='] => Ok((Token::Eq, next_mode)),
                    [c] if c.is_whitespace() =>
                    {
                        Ok((Token::Delim, next_mode))
                    },
                    ['"'] => Ok((Token::Placeholder, LexMode::String)),
                    [c] |
                    ['-', c] if c.is_digit(10) =>
                    {
                        Ok((Token::Placeholder, next_mode))
                    },
                    _ => Ok((Token::Placeholder, LexMode::Ident))

                }
            }
        }
    }
}

#[test]
fn test_match() -> ()
{
    let c : char = '{';
    let expected : Token = Token::OpenBrace;

    assert_eq!(matches(&[c], ' '), Some(expected, LexMode::Unknown))
}

pub fn to_string(chars : &[char]) -> String
{
    let mut string : String = String::with_capacity(chars.len());
    for c in chars.iter()
    {
        string.push(*c);
    }

    string
}

#[inline]
fn parse_int(src : &[char]) -> Option<i64>
{
    to_string(src).parse::<i64>().ok()
}

#[inline]
fn parse_float(src : &[char]) -> Option<f64>
{
    to_string(src).parse::<f64>().ok()
}

#[inline]
fn parse_date(src : &[char]) -> Option<(i64,i64,i64)>
{
    match src
    {
        [y1,y2,y3,y4,'.',m1,m2,'.',d1,d2] =>
        {
            let yr : Option<i64> = to_string(&[y1,y2,y3,y4]).parse().ok();
            let mn : Option<i64> = to_string(&[m1,m2]).parse().ok();
            let dy : Option<i64> = to_string(&[d1,d2]).parse().ok();

            match (yr,mn,dy)
            {
                (Some(y), Some(m), Some(d)) => Some((y,m,d)),
                _ => None
            }
        },
        [y1,y2,y3,y4,'.',m1,m2,'.',d1] =>
        {
            let yr : Option<i64> = to_string(&[y1,y2,y3,y4]).parse().ok();
            let mn : Option<i64> = to_string(&[m1,m2]).parse().ok();
            let dy : Option<i64> = to_string(&[d1]).parse().ok();

            match (yr,mn,dy)
            {
                (Some(y), Some(m), Some(d)) => Some((y,m,d)),
                _ => None
            }
        },
        [y1,y2,y3,y4,'.',m1,'.',d1,d2] =>
        {
            let yr : Option<i64> = to_string(&[y1,y2,y3,y4]).parse().ok();
            let mn : Option<i64> = to_string(&[m1]).parse().ok();
            let dy : Option<i64> = to_string(&[d1,d2]).parse().ok();

            match (yr,mn,dy)
            {
                (Some(y), Some(m), Some(d)) => Some((y,m,d)),
                _ => None
            }
        },
        [y1,y2,y3,y4,'.',m1,'.',d1] =>
        {
            let yr : Option<i64> = to_string(&[y1,y2,y3,y4]).parse().ok();
            let mn : Option<i64> = to_string(&[m1]).parse().ok();
            let dy : Option<i64> = to_string(&[d1]).parse().ok();

            match (yr,mn,dy)
            {
                (Some(y), Some(m), Some(d)) => Some((y,m,d)),
                _ => None
            }
        },

        _ => None
    }
}

// expect a tag to be a 3-char uppercase string
#[inline]
fn is_tag(src : &str) -> bool
{
    if src.len() == 3
    {
        for c in src.chars()
        {
            if !c.is_uppercase()
            {
                return false
            }
        }

        return true

    }

    false
}

// note to self: never do this
// it is a sure-fire recipe for rsi
#[inline]
fn to_regtype(s : &str) -> Option<Regtype>
{
    if s == "irregular" { return Some(Regtype::Irr) }
    if s == "cavalry" { return Some(Regtype::Cav) }
    if s == "infantry" { return Some(Regtype::Inf)}
    if s == "hussar" { return Some(Regtype::Hus)}
    if s == "cuirassier" { return Some(Regtype::Cui)}
    if s == "dragoon" { return Some(Regtype::Dra)}
    if s == "artillery" { return Some(Regtype::Art)}
    if s == "engineer" { return Some(Regtype::Eng)}
    if s == "guard" { return Some(Regtype::Gua)}
    if s == "tank" { return Some(Regtype::Arm)}
    if s == "plane" { return Some(Regtype::Pla)}

    None
}

/*

 The grammar we are interested in is:

 FILE := PREAMBLE+ NATION+ $
 PREAMBLE := ident '=' BLOCK
 BLOCK := ident | '{' PREAMBLE '}'
 NATION := TAG '=' '{' NBLOCK* '}'
 NBLOCK := PREAMBLE | ARMY | NAVY
 ARMY := 'army' '=' '{' REGBLOCK '}'
 NAVY := 'navy' '=' '{' SHIPBLOCK '}'
 REGBLOCK := PREAMBLE | 'type' '=' regtype
 SHIPBLOCK := PREAMBLE | 'type' '=' shiptype

 This is not the complete grammar, but only a fragment.

*/

pub fn parse<'a>(path : &Path)-> PResult<Save>
{
    let mut buf : [char; 1024] = [0 as char; 1024]; // should be enough!
    let mut src : String = String::with_capacity(20 * 1024 * 1024); // 20MB to start

    match File::open(path)
    {
        Ok(mut file) => match file.read_to_string(&mut src)
        {
            Ok(n) => { println!("Read {} bytes...", n) }
            Err(e) =>
            {
                println!("Error: {}", e.description());
                return Err(PError::new("Could not open file!", None, None))
            }
        },
        Err(_) => return Err(PError::new("Could not open file!", None, None))
    }

    

    let mut tokens = Scanner::new(&mut buf, src.chars());

    let mut tag : [char; 3] = [0 as char; 3];

    try!(root(&mut tokens));

    Ok(results)
}

fn root(tokens : &mut Scanner) -> PResult<Save>
{
    let mut nats: BTreeMap<Tag, Nation> = BTreeMap::new();
    let mut prov: BTreeMap<u64, Province> = BTreeMap::new();
    let mut tag: Tag = [0 as char; 3];
    loop
    {
        match tokens.next()
        {
            Some(Token::Ident(s)) =>
            {
                if is_tag(&s)
                {
                    tokens.put_back(Token::Ident(s));
                    
                    match nation(tokens, &mut tag)
                    {
                        Ok(n) => nations.insert(tag.clone(), n), // tag should be set by nation
                        Err(e) =>
                        {
                            return Err(e);
                        }
                    }
                }
            },

            Some(Token::Int) => match tokens.next()
            {
                Some(Token::Eq) => try!(province(tokens, &mut tag, &mut prov)),
                _ => {}
            },

            None => return Ok(()),

            Some(t) => { }
        }
    }

    Ok(())
}

fn nation(tokens : &mut Scanner,
          tag : &mut Tag) -> PResult<Nation>
{
    match tokens.next()
    {
        Some(Token::Ident(mut s)) =>
        {
            // pop runs in reverse order
            tag[2] = s.pop().expect("Bad tag");
            tag[1] = s.pop().expect("Bad tag");
            tag[0] = s.pop().expect("Bad tag");
        }

        t => return Err(PError::new("Not a tag",
                                    t,
                                    Some(Token::Ident("TAG".to_string()))))
    }

    println!("Found tag {:?}", tag);

    let mut num_braces : u64 = 0;

    let mut nation: Nation = Nation::new();

    loop
    {
        match tokens.next()
        {
            Some(Token::Ident(s)) => match &s
            {
                &["army"] =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(army(tokens, tag, &mut nation.armies));
                },
                &["navy"] =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(army(tokens, tag, &mut nation.navies));
                },
                &["treasury"] =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(treasury(tokens, tag, &mut nation.money));
                },
                &["primary"] =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(primary(tokens, tag, &mut nation.accepted));
                },
                &["accepted"] =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(accepted(tokens, tag, &mut nation.accepted));
                }
                ref t if is_tech(t) =>
                {
                    tokens.put_back(Token::Ident(s));
                    try!(tech(tokens, tag, &mut nation.tech));
                }
            }

            Some(Token::CloseBrace) if num_braces == 1 =>
            {
                println!("Returning to root..");
                return Ok(())
            },


            Some(Token::OpenBrace) => num_braces += 1,
            Some(Token::CloseBrace) if num_braces > 1 => num_braces -= 1,

            // have reached the end of the nation block
            // so we can return control
            None if num_braces > 1 => return ueof(Token::CloseBrace),
            None if num_braces == 1 => return Ok(()),

            _ => ()
        }
    }
    
}

fn army(tokens : &mut Scanner, tag : &mut Tag, res : &mut Vec<Mil>) -> PResult<()>
{
    let mut army : Army = Army::new();
    let mut num_braces : u64 = 0;
    
    //println!("Found army...");
    match tokens.next()
    {
        Some(Token::Ident(s)) =>
        {
            if &s != "army"
            {
                return Err(PError::new("Not an army",
                                       Some(Token::Ident(s)),
                                       Some(Token::Ident("army".to_string()))))
            }
            else
            {
                ()
            }
        }
        t => return Err(PError::new("Not an army",
                                    t,
                                    Some(Token::Ident("army".to_string()))))
    }

    loop
    {
        match tokens.next()
        {
            Some(Token::OpenBrace) => num_braces += 1,
            Some(Token::CloseBrace) if num_braces == 1 =>
            {
                res.push(Mil::A(army));
                println!("Finished with this army, returning to nation");
                return Ok(())
            }
            Some(Token::CloseBrace) if num_braces > 1 => num_braces -= 1,

            Some(Token::Ident(s)) =>
            {
                if &s == "regiment"
                {
                    tokens.put_back(Token::Ident(s));
                    try!(regiment(tokens, tag, res, &mut army));
                }
            },

            None => return ueof(Token::CloseBrace),
            _ => ()
        }
    }

}

fn regiment(tokens : &mut Scanner,
            tag : &mut Tag,
            res : &mut Vec<Mil>,
            army : &mut Army) -> PResult<()>
{
//    println!("Found regiment...");
    let mut num_braces : u64 = 0;

    match tokens.next()
    {
        Some(Token::Ident(s)) =>
        {
            if &s != "regiment"
            {
                return Err(PError::new("Not a regiment block",
                                       Some(Token::Ident(s)),
                                       Some(Token::Ident("regiment".to_string()))))
            }
            else
            {
                ()
            }
        }

        t => return Err(PError::new("Not a regiment block",
                                    t,
                                    Some(Token::Ident("regiment".to_string()))))
    }

    loop
    {
        match tokens.next()
        {
            Some(Token::OpenBrace) => num_braces += 1,
            Some(Token::CloseBrace) if num_braces == 1 => return Ok(()),
            Some(Token::CloseBrace) if num_braces > 1 => num_braces -= 1,
            Some(Token::Ident(s)) =>
            {
                if &s == "type"
                {
                    match regiment_type(tokens, tag, res, army)
                    {
                        Ok(()) => (),
                        // if it's not actually a regiment type line
                        // tidy things up and continue
                        Err(_) =>
                        {
                            // in the future if more things are needed from
                            // regiment blocks then put the tokens back
                            // and continue with alternate parse trees
                            // until then, just consume without issue
                            // probably won't bother leaving the structures in
                            // place for the navy parser
                            // but it is just the same

                            // tokens.put_back(Token::Ident(s));
                        }
                    }
                }
            },

            None => return ueof(Token::CloseBrace),

            _ => () // don't care about other tokens
        }
    }
    
}

fn regiment_type(tokens : &mut Scanner,
                 tag : &mut Tag,
                 res : &mut Vec<Mil>,
                 army : &mut Army) -> PResult<()>
{
    // expect '=', regtype
    match tokens.next()
    {
        Some(Token::Eq) =>
        {
            match tokens.next()
            {
                Some(Token::Ident(s)) =>
                {
                    if let Some(regtype) = to_regtype(&s)
                    {
                        match regtype
                        {
                            Regtype::Irr => army.irr += 1,
                            Regtype::Cav => army.cav += 1,
                            Regtype::Inf => army.inf += 1,
                            Regtype::Hus => army.hus += 1,
                            Regtype::Cui => army.cui += 1,
                            Regtype::Dra => army.dra += 1,
                            Regtype::Art => army.art += 1,
                            Regtype::Eng => army.eng += 1,
                            Regtype::Gua => army.gua += 1,
                            Regtype::Arm => army.arm += 1,
                            Regtype::Pla => army.pla += 1
                        };

                        println!("Found regiment: {}", s);
                        Ok(())
                    }

                    else
                    {
                        // see above on why these lines are commented
                        // tokens.put_back(Token::Ident(s));
                        Err(PError::new("Not regiment type",
                                        None,
                                        None))
                    }
                },

                t =>
                {
                    // tokens.put_back(t);
                    Err(PError::new("Not regiment type",
                                    None,
                                    None))
                }
            }
        },

        t =>
        {
            // tokens.put_back(t);
            Err(PError::new("Not regiment type",
                            None,
                            None))
        }
    }
}

fn navy(tokens : &mut Scanner, tag : &mut Tag, res : &mut Vec<Mil>) -> PResult<()>
{
    let mut navy : Navy = Navy::new(tag);
    let mut num_braces : u64 = 0;

//    println!("Found navy");
    // expect "navy={...}"
    match tokens.next()
    {
        Some(Token::Ident(s)) =>
        {
            if &s != "navy"
            {
                return Err(PError::new("Not a navy",
                                       Some(Token::Ident(s)),
                                       Some(Token::Ident("navy".to_string()))))
            }
        },
        
        t => return Err(PError::new("Not a navy",
                                    t,
                                    Some(Token::Ident("navy".to_string()))))
    }

    loop
    {
        match tokens.next()
        {
            Some(Token::OpenBrace) => num_braces += 1,
            Some(Token::CloseBrace) if num_braces > 1 => num_braces -= 1,
            Some(Token::CloseBrace) if num_braces == 1 =>
            {
                println!("Done with this navy");
                res.push(Mil::N(navy));
                return Ok(())
            },
            Some(Token::Ident(s)) =>
            {
                if &s == "ship"
                {
                    tokens.put_back(Token::Ident(s));
                    try!(ship(tokens, tag, res, &mut navy));
                }
            },
            None => return ueof(Token::CloseBrace),
            _ => ()
        }
    }
}

fn ship(tokens : &mut Scanner,
        tag : &mut Tag,
        res : &mut Vec<Mil>,
        navy : &mut Navy) -> PResult<()>
{
    let mut num_braces : u64 = 0;
 //   println!("Found ship...");

    match tokens.next()
    {
        Some(Token::Ident(s)) =>
        {
            if &s != "ship"
            {
                return Err(PError::new("Not a ship block",
                                       Some(Token::Ident(s)),
                                       Some(Token::Ident("ship".to_string()))))
            }
        },

        t => return Err(PError::new("Not a ship block",
                                    t,
                                    Some(Token::Ident("ship".to_string()))))
    }

    loop
    {
        match tokens.next()
        {
            Some(Token::OpenBrace) => num_braces += 1,
            Some(Token::CloseBrace) if num_braces > 1 => num_braces -= 1,
            Some(Token::CloseBrace) if num_braces == 1 => return Ok(()),
            Some(Token::Ident(s)) =>
            {
                if &s == "type"
                {
                    match ship_type(tokens, tag, res, navy)
                    {
                        Ok(()) => (),
                        Err(_) => (),
                    }
                }
            },
            None => return ueof(Token::CloseBrace),
            _ => ()
        }
    }
}

fn ship_type(tokens : &mut Scanner,
             tag : &mut Tag,
             res : &mut Vec<Mil>,
             navy : &mut Navy) -> PResult<()>
{
    // expect '=', shiptype
    match tokens.next()
    {
        Some(Token::Eq) =>
        {
            match tokens.next() 
            {
                Some(Token::Ident(s)) =>
                {
                    if &s == "clipper_transport" { navy.cli += 1; Ok(()) }
                    else if &s == "manowar" { navy.man += 1; Ok(())}
                    else if &s == "frigate" { navy.fri += 1; Ok(())}
                    else if &s == "commerce_raider" { navy.com += 1; Ok(())}
                    else if &s == "steam_transport" { navy.tra += 1; Ok(())}
                    else if &s == "monitor" { navy.mon += 1; Ok(())}
                    else if &s == "ironclad" { navy.iro += 1; Ok(())}
                    else if &s == "cruiser" { navy.cru += 1; Ok(())}
                    else if &s == "battleship" { navy.bat += 1; Ok(())}
                    else if &s == "dreadnought" { navy.dre += 1; Ok(())}
                    else {Err(PError::new("Not a shiptype", None, None))}
                },
                None => return ueof(Token::Ident("".to_string())),
                _ => Err(PError::new("Not a shiptype", None, None))
            }
        },
        _ => Err(PError::new("Not a shiptype", None, None))
    }
}

fn treasury(tokens: &mut Scanner,
            tag: &Tag,
            money: &mut f64) -> PResult<()>
{
    match tokens.next()
    {
        Some(Token::Ident(s)) => match &s
        {
            &["treasury"] => match tokens.next()
            {
                Some(Token::Eq) => match tokens.next()
                {
                    Some(Token::Float(f)) => *money = f;
                }
            },
            _ => Err(PError::new("Bad ident",
                                 Some(Token::Ident(s)),
                                 Some(Token::Ident("treasury".into()))))
        },

        Some(t) => Err(PError::new("Not an ident",
                                   Some(t),
                                   Some(Token::Ident("treasury".into())))),

        _ => ueof(Token::Ident("treasury".into()))
    }
}

fn primary(tokens: &mut Scanner,
           tag: &Tag,
           accepted: &mut Vec<String>) -> PResult<()>

{
    Ok(())
}

fn accepted(tokens: &mut Scanner,
            tag: &Tag,
            accepted: &mut Vec<String>) -> PResult<()>
{
    Ok(())
}

fn is_tech(t : &str) -> bool
{
    match t
    {
        ;
    }

    false
}

// for unexpected EOF errors
// could probably be a macro
#[inline]
fn ueof(expected : Token) -> PResult<()>
{
    Err(PError::new("Unexpected EOF", None, Some(expected)))
}
