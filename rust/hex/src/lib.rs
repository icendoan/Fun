#![feature(plugin)]
#![plugin(clippy)]

mod gfx;
mod config;
#[macro_use]
mod util;

use gfx::*;
use config::*;
use util::*;

use std::collections::{BTreeMap, VecDeque};
use std::ops::{Index, IndexMut};

pub struct Game
{
    map: Map,
    map_buffer: Map,
    players: Vec<PlayerInfo>,
    ticksize: u64,
    effects: VecDeque<Message>,
    msg_queue: VecDeque<Message>
}

struct PlayerInfo
{
    
    
}

struct Player
{
    id: Id,
    map: Map,
    map_buffer: Map,
    effects: VecDeque<Message>,
    msg_queue: VecDeque<Message>,
    
}

struct Map
{
    board : BTreeMap<Hex, Tile>, // index the map by its coordinates via a hashmap
    dimensions : (usize, usize),
}

struct Unit
{
    id : Id, // unique id!
    size : Size, // raw size of the unit
    maxsize : Size, // maximum size
    poptype : Poptype, // type - index into config struct
    location : Hex, // coord of current location
    speed : Speed, // distance covered per 'tick'
    attack : Attack, // raw attack, before modifiers
    defence : Defence, // raw defence, before modifiers
    width : Width, // Total space taken up on the battlefield, on infrastructure, etc
    produces : Vec<(Product, Size)>,
    requires : Vec<(Product, Size)>,
    moving : Option<Vec<Hex>>, // Path of hexes to move through
    atk_modifier : Attack, // persistent, because fighting through lack of supplies/etc should stack with itself, etc
    def_modifier : Defence,
}

struct Tile
{
    terrain : Terrain,
    coords : Hex,
    elevation : Elevation,
    weather : Weather,
    raw_res : Vec<(Product, Size)>, // raw resources - (type, max extracted per tick)
    produces : Vec<(Product, Size)>, // actual current production
    units : Vec<Unit>, // tiles own the units on them?
    goods : Vec<(Product, Size)>, // goods currently in the local stockpile
    exports : Vec<(Hex, Product, Size, Priority)>, // exports to neighbouring hexes
    movements : [Movement; 6] // for each neighbouring hex, how to move there - roads, rail, pipes, boat, etc (start at north, move clockwise)
}

index_impl!{Vec<Unit>,Id,Unit}

fn build_map(radius : usize) -> Map
{
}

impl Map
{
    fn get_tile(&self, hex : &Hex) -> &Tile
    {
        self.board.get(hex).unwrap()
    }

    // the map should always keep ownership of the tile!
    // use a mutable reference instead
    fn get_tile_mut(&mut self, hex : &Hex) -> &mut Tile
    {
        self.board.get_mut(hex).unwrap()
    }
}

impl<'b> Index<&'b Hex> for Map
{
    type Output = Tile;
    fn index<'a>(&'a self, index : &'b Hex) -> &'a Tile
    {
        self.get_tile(index)
    }
}
        
impl<'b> IndexMut<&'b Hex> for Map
{
    fn index_mut<'a>(&'a mut self, index : &'b Hex) -> &'a mut Tile
    {
        self.board.get_mut(index).unwrap()
    }
}

impl Tile
{
    // tries to move the unit to the tile specified by hex
    // returns the number of ticks until the unit reaches the next
    // fails with a HexError if there is no route, or unit cannot move, etc
    fn move_unit(&mut self, unit : Id, to : Hex) -> Result<Size,HexError>
    {
        if !self.units.contains(unit)
            { return Err(HexError::Minor(format!("No unit with Id: {:?} exists in {:?}", unit, self.coords))) }
        match find_path(self.coords, to)
        {
            Some(path) => {
                self.units[unit].moving = Some(path);
                Ok(trace_costs(&path))
            },
            None => Err(HexError::Minor(format!("No path exists from {:?} to {:?}", self.coords, to)))
        }
    }

    fn add_export(&mut self, product : Product, amount : Size, priority : Priority, to : Hex) -> Result<Size, HexError>
    {
        if self.is_neighbour(to)
        {
            let mut exported : Size = Size(0);
            for i in 0..self.exports.len() + 1
            {
                if self.exports[i].4 < priority
                {
                    self.exports.insert((to, product, amount, priority));
                    break;
                }
            }
        }
        else
    }
}

fn trace_costs(path : &Vec<Hex>) -> Size
{
}

fn find_path(from : Hex, to : Hex) -> Option<Vec<Hex>>
{
}
