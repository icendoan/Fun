extern crate gl;
extern crate glfw;
extern crate glm;

use gl::types::*;
use glfw::{Action, Context, Key};
use std::mem;
use std::ffi::CString;
use std::str;
use std::ptr;

// attributes and shaders

static PYRAMID_VERTEX_COLOUR : [GLfloat; 28] =
    [
        -0.5, -0.5, -0.5, 1.0, 0.0, 0.0, 0.0,
         0.5, -0.5, -0.5, 0.0, 1.0, 0.0, 0.0,
         0.0,  0.5, -0.5, 0.0, 0.0, 1.0, 0.0,
         0.0,  0.0,  0.5, 0.0, 0.0, 0.0, 1.0
            ];

static TRIANGLE_BASE : [GLuint; 3] = [0, 1, 2];
static TRIANGLE_ONE : [GLuint; 3] = [0, 1, 3];
static TRIANGLE_TWO : [GLuint; 3] = [0, 2, 3];
static TRIANGLE_THREE : [GLuint; 3] = [1, 2, 3];

static TRIANGLES : [&'static [GLuint;3];4] = [&TRIANGLE_BASE, &TRIANGLE_ONE, &TRIANGLE_TWO, &TRIANGLE_THREE];

static VS_SOURCE : &'static str =
    "#version 330 core\n\
     layout (location = 0) in vec3 position;\n\
     layout (location = 1) in vec4 colours;\n\
     out vec4 our_colours;\n\
     void main()\n\
     {\n\
          gl_Position = vec4(position, 1.0);\n\
          our_colours = colours;\n\
     }";

static FS_SOURCE : &'static str =
    "#version 330 core\n\
     out vec4 color;\n\
     in vec4 our_colours;\n\
     void main()\n\
     {\n\
         color = our_colours;\n\
     }";

fn main() -> ()
{
    //make window
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::WindowHint::Resizable(true));
    glfw.window_hint(glfw::WindowHint::RefreshRate(60));
    glfw.window_hint(glfw::WindowHint::ContextVersion(3,3));
    glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));
    let (mut window, events) = glfw.create_window(800,600,"Pyramid",glfw::WindowMode::Windowed).expect("Failed to create window.");

    window.set_key_polling(true);
    window.make_current();

    // load OpenGL function pointers
    gl::load_with(|s| window.get_proc_address(s));

    // init OpenGL systems
    let mut vbos : [GLuint; 1] = [0];
    let mut vaos : [GLuint; 1] = [0];
    let mut ebos : [GLuint; 4] = [0;4];
    let (mut a, mut b, mut c) : (f32, f32, f32) = (0.0, 0.0, 0.0);
    
    let (vs, fs) = (compile_shader(VS_SOURCE, gl::VERTEX_SHADER), compile_shader(FS_SOURCE, gl::FRAGMENT_SHADER));
    let shader_program = link_program(vs, fs);

    unsafe
    {
        // delete used shaders
        gl::DeleteShader(vs);
        gl::DeleteShader(fs);
        
        gl::ClearColor(0.3, 0.3, 0.3, 1.0);

        //gen buffers

        gl::GenVertexArrays(vaos.len() as GLint, mem::transmute(&mut vaos[0]));
        gl::GenBuffers(vbos.len() as GLint, mem::transmute(&mut vbos[0]));
        gl::GenBuffers(ebos.len() as GLint, mem::transmute(&mut ebos[0]));

        // fill buffers
        gl::BindVertexArray(vaos[0]);

        gl::BindBuffer(gl::ARRAY_BUFFER, vbos[0]);
        gl::BufferData(gl::ARRAY_BUFFER, (PYRAMID_VERTEX_COLOUR.len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&PYRAMID_VERTEX_COLOUR[0]), gl::DYNAMIC_DRAW);

        //positions

        gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, (7 * mem::size_of::<GLfloat>()) as GLsizei, ptr::null());
        gl::EnableVertexAttribArray(0);

        //colours

        gl::VertexAttribPointer(1, 4, gl::FLOAT, gl::FALSE, (7 * mem::size_of::<GLfloat>()) as GLsizei, (3 * mem::size_of::<GLfloat>()) as *const GLvoid);
        gl::EnableVertexAttribArray(1);

        //unbind array
        gl::BindVertexArray(0);

        for x in (0..TRIANGLES.len())
        {
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebos[x]);
            gl::BufferData(gl::ELEMENT_ARRAY_BUFFER, ((*TRIANGLES[x]).len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&(TRIANGLES[x][0])), gl::DYNAMIC_DRAW);
        }

        //use generated shader program
        gl::UseProgram(shader_program);
    }

    let (mut start, mut end) : ((f64, f64), (f64, f64)) = ((0.0, 0.0), (0.0, 0.0));
    let mut mouse_released : bool = false;

    while !window.should_close()
    {
        // handle user input 
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events)
        {
            handle_input(&mut window, event, &mut start, &mut end, &mut mouse_released);
        }

        if mouse_released
        {
            // calculate view changes

            mouse_released = false;
        }
        

        // OpenGL loop

        unsafe
        {
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::BindVertexArray(vaos[0]);
            for x in 0..TRIANGLES.len()
            {
                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebos[x]);
                gl::DrawElements(gl::TRIANGLES, TRIANGLES[x].len() as GLsizei, gl::UNSIGNED_INT, ptr::null());
            }
            gl::BindVertexArray(0);

        }

        //println!("{:?}", window.get_cursor_pos());

        window.swap_buffers();
    }
}

fn handle_input(window : &mut glfw::Window, event : glfw::WindowEvent, start_coords : &mut (f64, f64), end_coords : &mut  (f64, f64), mouse_released : &mut bool) -> ()
{
    match event
    {
        glfw::WindowEvent::Key(k, _, Action::Press, _) => match k
        {
            _ => {}
        },
        glfw::WindowEvent::MouseButton(button, action, modifiers) => match (button, action, modifiers)
        {
            (glfw::MouseButtonLeft, glfw::Action::Press, _) => *start_coords = window.get_cursor_pos(),
            (glfw::MouseButtonLeft, glfw::Action::Release, _) => {*end_coords = window.get_cursor_pos(); *mouse_released = true},
            _ => {}
        },
        _ => {}
    }
}


// Shader helper functions

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
