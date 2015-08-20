extern crate gl;
extern crate glfw;

use gl::types::*;
use glfw::{Action, Context, Key};
use std::mem;
use std::ffi::CString;
use std::str;
use std::ptr;

struct ColourState
{
    r : GLfloat,
    g : GLfloat,
    b : GLfloat,
}

struct ViewState
{
    x : GLint,
    y : GLint,
}

static VERTICES : [GLfloat; 18] =
    [
        -0.5, -0.5, 0.0, 1.0, 0.0, 0.0,  // bottom left
        0.0, 0.5, 0.0,  0.0, 1.0, 0.0,// middle left
        0.5, -0.5, 0.0,  0.0, 0.0, 1.0//middle
    ];

static TRIANGLE_ONE : [GLuint; 3] = [0, 1, 2];

static VERTEX_SRC : &'static str =
    "#version 330 core\n\
     layout (location = 0) in vec3 position;\n\
     layout (location = 1) in vec3 colours;\n\
     out vec3 ourColours;\n\
     void main()\n\
     {\n\
         gl_Position = vec4(position.x, position.y, position.z, 1.0);\n\
         ourColours = colours;\n\
     }";

static FRAGMENT_SRC : &'static str =
    "#version 330 core\n\
     out vec4 color;\n\
     in vec3 ourColours;\n\
     void main()\n\
     {\n\
         color = vec4(ourColours, 1.0f);\n\
     }";

fn main() -> ()
{
    // init window system
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::WindowHint::Resizable(true));
    glfw.window_hint(glfw::WindowHint::RefreshRate(60));
    glfw.window_hint(glfw::WindowHint::ContextVersion(3,3));
    glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));
    let (mut window, events) = glfw.create_window(800,600,"glfw",glfw::WindowMode::Windowed).expect("Failed to create GLFW window.");
    window.set_key_polling(true);
    window.make_current();

    // init opengl function pointers
    gl::load_with(|s| window.get_proc_address(s));

    // opengl init
    let mut vbo : GLuint = 0; //make a vertex buffer object to store the triangle coordinates
    unsafe
    {
        gl::Viewport(0, 0, 800, 600);
        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
        gl::BufferData(gl::ARRAY_BUFFER, (VERTICES.len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&VERTICES[0]), gl::DYNAMIC_DRAW);
    }

    let mut cstate : ColourState = ColourState{ r : 0.0, g : 0.0, b : 0.0};
    let mut vstate : ViewState = ViewState { x : 0, y : 0 };
    
    //compile shaders
    let vs = compile_shader(VERTEX_SRC, gl::VERTEX_SHADER);
    let fs = compile_shader(FRAGMENT_SRC, gl::FRAGMENT_SHADER);

    // link shaders
    let program = link_program(vs, fs);

    // delete shaders

    unsafe
    {
        gl::DeleteShader(vs);
        gl::DeleteShader(fs);
    }

    //make a vertex array object
    let mut vao : GLuint = 0;
    unsafe
    {
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
        gl::BufferData(gl::ARRAY_BUFFER, (VERTICES.len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&VERTICES[0]), gl::DYNAMIC_DRAW);
        //position attribute
        gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (6 * mem::size_of::<GLfloat>()) as GLsizei, ptr::null());
        gl::EnableVertexAttribArray(0);
        //colour attribute
        gl::VertexAttribPointer(1, 3, gl::FLOAT, gl::FALSE, (6 * mem::size_of::<GLfloat>()) as GLsizei, (3 * mem::size_of::<GLfloat>()) as *const GLvoid);
        gl::EnableVertexAttribArray(1);
        gl::BindVertexArray(0);
    }

    // make element array objects
    let mut ebos : [GLuint ; 2] = [0,0];
    unsafe
    {
        gl::GenBuffers(2, mem::transmute(&mut ebos[0]));
        gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebos[0]);
        gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, (TRIANGLE_ONE.len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&TRIANGLE_ONE[0]), gl::DYNAMIC_DRAW);
    }
    
    // main loop
    while !window.should_close()
    {
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events)
        {
            handle_keypress(&mut window, event, &mut cstate, &mut vstate)
        }
        unsafe
        {
            //change the background colour
            gl::ClearColor(cstate.r, cstate.g, cstate.b, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            //change the viewport
            gl::Viewport(vstate.x, vstate.y, 800, 600);

            gl::BindVertexArray(vao); // set triangles' vertices

            gl::UseProgram(program);

            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebos[0]); // draw triangle one
            gl::DrawElements(gl::TRIANGLES, TRIANGLE_ONE.len() as GLsizei, gl::UNSIGNED_INT, ptr::null()); 

            gl::BindVertexArray(0);

        }

        window.swap_buffers();
    }

    // cleanup
}

fn handle_keypress(window : &mut glfw::Window, event : glfw::WindowEvent, cstate : &mut ColourState, vstate : &mut ViewState) -> ()
{
    match event
    {
        glfw::WindowEvent::Key(k, _, Action::Press, _) => match k
        {
            Key::Escape => window.set_should_close(true),
            Key::R => { cstate.r += 0.05 },
            Key::G => { cstate.g += 0.05 },
            Key::B => { cstate.b += 0.05 },
            Key::C => { cstate.r = 0.0; cstate.b = 0.0; cstate.g = 0.0 },           
            Key::W => { cstate.r = 1.0; cstate.b = 1.0; cstate.g = 1.0 },
            Key::Left => vstate.x -= 5,
            Key::Right => vstate.x += 5,
            Key::Up => vstate.y += 5,
            Key::Down => vstate.y -= 5,
            _ => {}
        },
        _ => {},
    }
    cstate.r = cstate.r % 1.01;
    cstate.g = cstate.g % 1.01;
    cstate.b = cstate.b % 1.01;
}


fn compile_shader(src: &str, ty: GLenum) -> GLuint
{
    let shader;
    unsafe
    {
        shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes()).unwrap();
        gl::ShaderSource(shader, 1, &c_str.as_ptr(), ptr::null());
        gl::CompileShader(shader);

        // Get the compile status
        let mut status = gl::FALSE as GLint;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as GLint)
        {
            let mut len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            gl::GetShaderInfoLog(shader, len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
            panic!("{}", str::from_utf8(&buf).ok().expect("ShaderInfoLog not valid utf8"));
        }
    }
    shader
}

fn link_program(vs: GLuint, fs: GLuint) -> GLuint
{
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);
        gl::LinkProgram(program);
        // Get the link status
        let mut status = gl::FALSE as GLint;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);
        
        // Fail on error
        if status != (gl::TRUE as GLint)
        {
            let mut len: GLint = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            gl::GetProgramInfoLog(program, len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
            panic!("{}", str::from_utf8(&buf).ok().expect("ProgramInfoLog not valid utf8"));
        }
        program
    }
}
