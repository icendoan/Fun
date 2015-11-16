#![macro_use]
extern crate glium;


extern crate hex;

use glium::DisplayBuild;

fn main() -> ()
{
    let display = glium::glutin::WindowBuilder::new().build_glium().unwrap();
    loop
    {
        for event in display.poll_events()
        {
            match event
            {
                glium::glutin::Event::Closed => return (),
                _ => (),
            }
        }
    }
}
