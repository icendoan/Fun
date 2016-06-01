use util::*;

use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};

pub struct Map
{
    board : BTreeMap<Hex, Tile>, // index the map by its coordinates via a hashmap
    dimensions : (usize, usize),
}

pub fn build_map(radius : usize) -> Map
{
}

impl Map
{
    pub fn get_tile(&self, hex : &Hex) -> Option<&Tile>
    {
        self.board.get(hex)
    }

    pub fn get_tile_mut(&mut self, hex : &Hex) -> Option<&mut Tile>
    {
        self.board.get_mut(hex)
    }

    pub fn shortest_path(&self, to : &Hex) -> Option<Vec<Hex>>
    {
        None
    }
}

impl<'b> Index<&'b Hex> for Map
{
    type Output = Tile;
    fn index<'a>(&'a self, index : &'b Hex) -> &'a Tile
    {
        self.get_tile(index).unwrap()
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
    
}
