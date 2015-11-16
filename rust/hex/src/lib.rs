#![feature(plugin)]
#![plugin(clippy)]

mod gfx;
mod config;
mod util;

use gfx::*;
use config::*;
use util::*;

use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};

pub struct Map
{
    board : BTreeMap<Hex, Tile>, // index the map by its coordinates via a hashmap
    dimensions : (usize, usize),
}

pub struct Unit
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

pub struct Tile
{
    terrain : Terrain,
    coords : Hex,
    elevation : Elevation,
    weather : Weather,
    raw_res : Vec<(Product, usize)>, // raw resources - (type, max extracted per tick)
    produces : Vec<(Product, usize)>, // actual current production
    units : Vec<Unit>, // tiles own the units on them?
    goods : Vec<(Product, usize)>, // goods currently in the local stockpile
    exports : Vec<(Hex, Product, usize)>, // exports to neighbouring hexes
    infrastructure : Width, // total width of Units moving(!) through the tile at any one moment
}


pub fn build_map(radius : usize) -> Map
{
}

impl Map
{
    pub fn get_tile(&self, hex : Hex) -> &Tile
    {
    }

    // the map should always keep ownership of the tile!
    // use a mutable reference instead
    pub fn get_tile_mut(&mut self, hex : Hex) -> &mut Tile
    {
    }
}

impl<'b> Index<&'b Hex> for Map
{
    type Output = Tile;
    fn index<'a>(&'a self, index : &'b Hex) -> &'a Tile
    {
        self.board.get(index).unwrap()
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
    pub fn move_unit(&mut self, unit : Id, to : Hex) -> Result<usize,HexError>
    {
        match find_path(self.coords, to)
        {
            Some(path) => {
                self.units[unit].moving = Some(path);
                Ok(trace_costs(&path))
            },
            None => Err(HexError::Minor(format!("No path exists from {:?} to {:?}", self.coords, to)))
        }
    }

    pub fn add_export(&mut self, product : Product, to : Hex) -> Result<usize, HexError>
    {
    }
}

fn trace_costs(path : &Vec<Hex>) -> Size
{
}

fn find_path(from : Hex, to : Hex) -> Option<Vec<Hex>>
{
}
