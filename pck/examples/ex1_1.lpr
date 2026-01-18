program ex1_1;
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

uses
  SysUtils,
  pax.gl,
  pax.glfw,
  LinMath,
  gl_utils, pax.vulkan.video.codecav1, pax.vulkan.video.codecvp9;

type
  TVertex = record
    pos: TVec2;
    uv: TVec2;   // ← coordinate texture (UV)
  end;

const
  vertices: array[0..2] of TVertex = (
    (pos: (-0.6, -0.4); uv: (0.0, 0.0)),
    (pos: (0.6, -0.4); uv: (1.0, 0.0)),
    (pos: (0.0, 0.6); uv: (0.5, 1.0))
    );

  vertex_shader_text =
    '#version 330 core' + LineEnding + // 0
    'layout(location = 0) in vec2 vPos;' + LineEnding + // 0
    'layout(location = 1) in vec2 vUV;' + LineEnding + // 0
    'out vec2 fragUV;' + LineEnding + // 0
    'uniform mat4 MVP;' + LineEnding + // 0
    'void main()' + LineEnding + // 0
    '{' + LineEnding + // 0
    '    gl_Position = MVP * vec4(vPos, 0.0, 1.0);' + LineEnding + // 0
    '    fragUV = vUV;' + LineEnding + // 0
    '}';

  fragment_shader_text =
    '#version 330 core' + LineEnding + // 0
    'in vec2 fragUV;' + LineEnding + // 0
    'out vec4 fragment;' + LineEnding + // 0
    'uniform sampler2D texChecker;' + LineEnding + // 0
    'void main()' + LineEnding + // 0
    '{' + LineEnding + // 0
    '    fragment = texture(texChecker, fragUV);' + LineEnding + // 0
    '}';

  procedure error_callback(error: integer; const description: pchar); cdecl;
  begin
    WriteLn(StdErr, 'GLFW Error ', error, ': ', description);
  end;

  procedure key_callback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    if (key = GLFW_KEY_ESCAPE) and (action = GLFW_PRESS) then
      getGLFW.glfwSetWindowShouldClose(window, true);
  end;

var
  window: PGLFWwindow;
  Width, Height: integer;
  ratio: single;

  vertex_buffer: GLuint = 0;
  vertex_array: GLuint = 0;

  shaderProg: TShaderProgram;
  checkerTex: TTexture;

  mvp_location: GLint;
  tex_location: GLint;
  m, p, mvp: TMat4x4;

begin
  with getGLFW, GetOpenGL do
  begin
    glfwSetErrorCallback(@error_callback);

    if not glfwInit() then Halt(1);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE);

    window := glfwCreateWindow(800, 600, 'Textured Rotating Triangle', nil, nil);
    if window = nil then
    begin
      glfwTerminate();
      Halt(1);
    end;

    glfwSetKeyCallback(window, @key_callback);
    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);

    // ==============================================
    // Inizializzazione texture procedurale
    // ==============================================
    checkerTex.Init;
    checkerTex.CreateCheckerboard(128, 16);  // 128×128 con quadretti 16×16

    // ==============================================
    // Shader
    // ==============================================
    shaderProg.Init;

    if shaderProg.CompileVertex(vertex_shader_text) = 0 then
    begin
      WriteLn('Vertex shader failed: ', shaderProg.log);
      glfwDestroyWindow(window);
      glfwTerminate();
      Halt(1);
    end;

    if shaderProg.CompileFragment(fragment_shader_text) = 0 then
    begin
      WriteLn('Fragment shader failed: ', shaderProg.log);
      glfwDestroyWindow(window);
      glfwTerminate();
      Halt(1);
    end;

    if shaderProg.LinkProgram = 0 then
    begin
      WriteLn('Linking failed: ', shaderProg.log);
      glfwDestroyWindow(window);
      glfwTerminate();
      Halt(1);
    end;

    mvp_location := glGetUniformLocation(shaderProg.id, 'MVP');
    tex_location := glGetUniformLocation(shaderProg.id, 'texChecker');

    // ==============================================
    // Buffer e VAO
    // ==============================================
    glGenBuffers(1, @vertex_buffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices, GL_STATIC_DRAW);

    glGenVertexArrays(1, @vertex_array);
    glBindVertexArray(vertex_array);

    // Posizione (location 0)
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
      SizeOf(TVertex),
      Pointer(@TVertex(nil^).pos));

    // Coordinate texture UV (location 1)
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE,
      SizeOf(TVertex),
      Pointer(@TVertex(nil^).uv));

    glBindVertexArray(0);

    WriteLn('OpenGL Renderer: ', glGetString(GL_RENDERER));
    WriteLn('OpenGL Version:  ', glGetString(GL_VERSION));

    // ==============================================
    // Main loop
    // ==============================================
    while not glfwWindowShouldClose(window) do
    begin
      glfwGetFramebufferSize(window, Width, Height);
      ratio := Width / Height;

      glViewport(0, 0, Width, Height);
      glClearColor(0.08, 0.10, 0.14, 1.0);
      glClear(GL_COLOR_BUFFER_BIT);

      Mat4x4_Identity(m);
      Mat4x4_Rotate_Z(m, m, glfwGetTime() * 0.8);
      Mat4x4_Ortho(p, -ratio, ratio, -1.0, 1.0, -1.0, 1.0);
      Mat4x4_Mul(mvp, p, m);

      shaderProg.Use;

      glUniformMatrix4fv(mvp_location, 1, GL_FALSE, @mvp);

      // Bind della texture sulla unit 0
      checkerTex.Bind(0);
      glUniform1i(tex_location, 0);  // comunica allo shader che la texture è sulla unit 0

      glBindVertexArray(vertex_array);
      glDrawArrays(GL_TRIANGLES, 0, 3);

      glfwSwapBuffers(window);
      glfwPollEvents();
    end;

    // Cleanup
    glBindVertexArray(0);
    glDeleteVertexArrays(1, @vertex_array);
    glDeleteBuffers(1, @vertex_buffer);

    checkerTex.Clear;
    shaderProg.Clear;

    glfwDestroyWindow(window);
    glfwTerminate();

    WriteLn('Program terminated normally.');
  end;
end.
