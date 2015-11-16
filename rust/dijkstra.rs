use std::collections::BTreeMap;

fn main() -> ()
{
    println!("Hello, World!");
}

// finds the shortest path in a 2d rectangular grid via Dijkstra's algorithm
fn find_path(start : isize, target : isize, pmap : &[isize], pmap_width : isize) -> Option<Vec<isize>>
{
    let mut to_visit : Vec<(isize,isize,isize)> = Vec::new();
    let mut came_from : BTreeMap<isize, isize> = BTreeMap::new();
    let mut current = start;
    let mut distances : BTreeMap<isize, isize> = BTreeMap::new();
    let mut current_distance : isize = 0;

    // while not at our target
    while current != target
    {
        // add adjacent nodes if possible
        for i in 0..4
        {
            let adjacent = match i
            {
                0 => if current % pmap_width != 0
                {
                    current - 1
                } else
                {
                    0
                },
               
                1 => if current + pmap_width > (pmap.len() as isize)
                {
                    0
                }
                else { current + pmap_width },
                2 => if (current + 1) % pmap_width != 0 { current + 1 } else { 0 },
                3 => if (current - pmap_width) >= 0 { current - pmap_width } else { 0 },
                _ => { 0 }
            };
            if pmap[adjacent as usize] == 1 && adjacent != current
            {
                insert_node(&mut to_visit, adjacent, current_distance + 1, current);
            }
        }

        if to_visit.is_empty()
        {
            return None;
        }

        let (next,dist,prev) = to_visit.pop().unwrap();
        if distances.contains_key(&next)
        {
            let score = distances.get_mut(&next).unwrap();
            if *score > current_distance
            {
                *score = current_distance;
                came_from.insert(next, prev);
            }
        }
        else
        {
            distances.insert(next, current_distance + 1);
            came_from.insert(next, prev);
        }
        
        current = next;
        current_distance = dist + 1;
    }

    let mut path : Vec<isize> = Vec::new();

    while current != start
    {
        let prev = came_from.get(&current).unwrap();
        path.push(*prev);
        current = *prev;
    }
    
    return Some(path);
}

fn insert_node(v : &mut Vec<(isize, isize, isize)>, i : isize, d : isize, p : isize)
{
    for j in 0..v.len()
    {
        let (_,e,_) = v[j];
        if d < e
        {
            v.insert(j,(i,d,p));
            return;
        }
    }
    v.push((i,d,p));
}
