program ex1;
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

uses
  pax.gl,
  pax.glfw,
  LinMath;

type
  PVertex = ^TVertex;

  TVertex = record
    pos: TVec2;
    col: TVec3;
  end;

const
  vertices: array[0..2] of TVertex = (
    (pos: (-0.6, -0.4); col: (1.0, 0.0, 0.0)),
    (pos: (0.6, -0.4); col: (0.0, 1.0, 0.0)),
    (pos: (0.0, 0.6); col: (0.0, 0.0, 1.0))
    );

  vertex_shader_text =
    '#version 330'#10 + 'uniform mat4 MVP;'#10 + 'in vec3 vCol;'#10 + 'in vec2 vPos;'#10 + 'out vec3 color;'#10 + 'void main()'#10 + '{'#10 + '    gl_Position = MVP * vec4(vPos, 0.0, 1.0);'#10 + '    color = vCol;'#10 + '}';

  fragment_shader_text =
    '#version 330'#10 + 'in vec3 color;'#10 + 'out vec4 fragment;'#10 + 'void main()'#10 + '{'#10 + '    fragment = vec4(color, 1.0);'#10 + '}';

  procedure error_callback(error: integer; const description: pansichar); cdecl;
  begin
    Writeln(ErrOutput, 'Error: ', description);
  end;

  procedure key_callback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    with getGLFW do
    begin
      if (key = GLFW_KEY_ESCAPE) and (action = GLFW_PRESS) then
        glfwSetWindowShouldClose(window, True);
    end;
  end;

  function CompileShader(shaderType: GLenum; Source: pansichar): GLuint;
  var
    shader: GLuint;
    compiled: GLint;
    logLen: GLint;
    log: PGLchar;
  begin
    with getGLFW, GetOpenGL do
    begin
      shader := glCreateShader(shaderType);
      glShaderSource(shader, 1, @Source, nil);
      glCompileShader(shader);

      glGetShaderiv(shader, GL_COMPILE_STATUS, @compiled);
      if compiled = GL_FALSE then
      begin
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, @logLen);
        GetMem(log, logLen);
        glGetShaderInfoLog(shader, logLen, nil, log);
        Writeln(ErrOutput, 'Shader compilation error: ', log);
        FreeMem(log);
        glDeleteShader(shader);
        Result := 0;
        Exit;
      end;

      Result := shader;
    end;
  end;

  function LinkProgram(vs, fs: GLuint): GLuint;
  var
    program_: GLuint;
    linked: GLint;
    logLen: GLint;
    log: PGLchar;
  begin
    with getGLFW, GetOpenGL do
    begin

      program_ := glCreateProgram();
      glAttachShader(program_, vs);
      glAttachShader(program_, fs);
      glLinkProgram(program_);

      glGetProgramiv(program_, GL_LINK_STATUS, @linked);
      if linked = GL_FALSE then
      begin
        glGetProgramiv(program_, GL_INFO_LOG_LENGTH, @logLen);
        GetMem(log, logLen);
        glGetProgramInfoLog(program_, logLen, nil, log);
        Writeln(ErrOutput, 'Program link error: ', log);
        FreeMem(log);
        glDeleteProgram(program_);
        Result := 0;
        Exit;
      end;

    end;
    Result := program_;
  end;

var
  window: PGLFWwindow;
  Width, Height: integer;
  ratio: single;
  vertex_buffer, vertex_array: GLuint;
  vertex_shader, fragment_shader: GLuint;

  program_: GLuint;

  mvp_location, vpos_location, vcol_location: GLint;
  m, p, mvp: TMat4x4;
  time: double;

begin
  with getGLFW, GetOpenGL do
  begin

    glfwSetErrorCallback(@error_callback);

    if not glfwInit() then
    begin
      Writeln(ErrOutput, 'Failed to initialize GLFW');
      Halt(1);
    end;

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    window := glfwCreateWindow(640, 480, 'OpenGL Triangle', nil, nil);
    if window = nil then
    begin
      Writeln(ErrOutput, 'Failed to create GLFW window');
      glfwTerminate();
      Halt(1);
    end;

    glfwSetKeyCallback(window, @key_callback);
    glfwMakeContextCurrent(window);

    glfwSwapInterval(1);

    // Vertex Buffer
    glGenBuffers(1, @vertex_buffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices, GL_STATIC_DRAW);

    // Shader
    vertex_shader := CompileShader(GL_VERTEX_SHADER, pansichar(vertex_shader_text));
    fragment_shader := CompileShader(GL_FRAGMENT_SHADER, pansichar(fragment_shader_text));
    if (vertex_shader = 0) or (fragment_shader = 0) then
    begin
      glfwTerminate();
      Halt(1);
    end;

    program_ := LinkProgram(vertex_shader, fragment_shader);
    if program_ = 0 then
    begin
      glfwTerminate();
      Halt(1);
    end;

    mvp_location := glGetUniformLocation(program_, 'MVP');
    vpos_location := glGetAttribLocation(program_, 'vPos');
    vcol_location := glGetAttribLocation(program_, 'vCol');

    // Vertex Array Object
    glGenVertexArrays(1, @vertex_array);
    glBindVertexArray(vertex_array);

    glEnableVertexAttribArray(vpos_location);
    glVertexAttribPointer(vpos_location, 2, GL_FLOAT, GL_FALSE,
      SizeOf(TVertex), Pointer(@PVertex(nil)^.pos));

    glEnableVertexAttribArray(vcol_location);
    glVertexAttribPointer(vcol_location, 3, GL_FLOAT, GL_FALSE,
      SizeOf(TVertex), Pointer(@PVertex(nil)^.col));

    while not glfwWindowShouldClose(window) do
    begin
      glfwGetFramebufferSize(window, Width, Height);
      ratio := Width / Height;

      glViewport(0, 0, Width, Height);
      glClear(GL_COLOR_BUFFER_BIT);

      Mat4x4_Identity(m);
      Mat4x4_Rotate_Z(m, m, glfwGetTime());  // Rotazione nel tempo
      Mat4x4_Ortho(p, -ratio, ratio, -1.0, 1.0, 1.0, -1.0);
      Mat4x4_Mul(mvp, p, m);

      glUseProgram(program_);
      glUniformMatrix4fv(mvp_location, 1, GL_FALSE, @mvp);

      glBindVertexArray(vertex_array);
      glDrawArrays(GL_TRIANGLES, 0, 3);

      glfwSwapBuffers(window);
      glfwPollEvents();
    end;

    // Cleanup (opzionale, ma buona pratica)
    glDeleteVertexArrays(1, @vertex_array);
    glDeleteBuffers(1, @vertex_buffer);
    glDeleteProgram(program_);
    glDeleteShader(fragment_shader);
    glDeleteShader(vertex_shader);

    glfwDestroyWindow(window);
    glfwTerminate();

  end;
end.
