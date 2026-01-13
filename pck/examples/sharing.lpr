program ContextSharingExample;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  pax.glfw,
  pax.gl;

const
  VertexShaderText: PGLchar =
    '#version 110'#10 + 'uniform mat4 MVP;'#10 + 'attribute vec2 vPos;'#10 + 'varying vec2 texcoord;'#10 + 'void main()'#10 + '{'#10 + '    gl_Position = MVP * vec4(vPos, 0.0, 1.0);'#10 + '    texcoord = vPos;'#10 + '}'#10#0;

  FragmentShaderText: PGLchar =
    '#version 110'#10 + 'uniform sampler2D texture;'#10 + 'uniform vec3 color;'#10 + 'varying vec2 texcoord;'#10 + 'void main()'#10 + '{'#10 + '    gl_FragColor = vec4(color * texture2D(texture, texcoord).rgb, 1.0);'#10 + '}'#10#0;

type
  TVertex2f = record
    x, y: GLfloat;
  end;

const
  Vertices: array[0..3] of TVertex2f = (
    (x: 0.0; y: 0.0),
    (x: 1.0; y: 0.0),
    (x: 1.0; y: 1.0),
    (x: 0.0; y: 1.0)
    );

var
  Windows: array[0..1] of PGLFWwindow;
  Texture, ProgramID, VertexBuffer: GLuint;
  MvpLocation, ColorLocation, TextureLocation, VposLocation: GLint;
type
  TMat4 = array[0..3, 0..3] of GLfloat;

  procedure Mat4Ortho(out m: TMat4; const left, right, bottom, top, zNear, zFar: GLfloat);
  begin
    m[0, 0] := 2.0 / (right - left);
    m[0, 1] := 0;
    m[0, 2] := 0;
    m[0, 3] := -(right + left) / (right - left);
    m[1, 0] := 0;
    m[1, 1] := 2.0 / (top - bottom);
    m[1, 2] := 0;
    m[1, 3] := -(top + bottom) / (top - bottom);
    m[2, 0] := 0;
    m[2, 1] := 0;
    m[2, 2] := -2.0 / (zFar - zNear);
    m[2, 3] := -(zFar + zNear) / (zFar - zNear);
    m[3, 0] := 0;
    m[3, 1] := 0;
    m[3, 2] := 0;
    m[3, 3] := 1.0;
  end;

  // Callback di errore
  procedure ErrorCallback(error: integer; const description: pchar); cdecl;
  begin
    WriteLn(StdErr, 'Error: ', description);
  end;

  // Callback tasto ESC per chiudere
  procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    if (action = GLFW_PRESS) and (key = GLFW_KEY_ESCAPE) then
      getGLFW.glfwSetWindowShouldClose(window, true);
  end;

  procedure CreateSharedObjects;
  var
    VertexShader, FragmentShader: GLuint;
    Pixels: array[0..15 * 15] of byte;
    x, y: integer;
  begin
    Randomize;
    for y := 0 to 15 do
      for x := 0 to 15 do
        Pixels[y * 16 + x] := Random(256);
    with GetOpenGL do
    begin
      try
        glGenTextures(1, @Texture);
        glBindTexture(GL_TEXTURE_2D, Texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, 16, 16, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, @Pixels[0]);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

        VertexShader := glCreateShader(GL_VERTEX_SHADER);
        glShaderSource(VertexShader, 1, @VertexShaderText, nil);
        glCompileShader(VertexShader);

        FragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
        glShaderSource(FragmentShader, 1, @FragmentShaderText, nil);
        glCompileShader(FragmentShader);

        ProgramID := glCreateProgram;
        glAttachShader(ProgramID, VertexShader);
        glAttachShader(ProgramID, FragmentShader);
        glLinkProgram(ProgramID);

        MvpLocation := glGetUniformLocation(ProgramID, 'MVP');
        ColorLocation := glGetUniformLocation(ProgramID, 'color');
        TextureLocation := glGetUniformLocation(ProgramID, 'texture');
        VposLocation := glGetAttribLocation(ProgramID, 'vPos');

        glGenBuffers(1, @VertexBuffer);
        glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
        glBufferData(GL_ARRAY_BUFFER, SizeOf(Vertices), @Vertices[0], GL_STATIC_DRAW);
      except
        on E: Exception do
        begin
          Writeln(e.Message);
          halt(1);
        end;
      end;
    end;
  end;

var
  xpos, ypos, Width, left, right: integer;
  i: integer;
const
  Colors: array[0..1] of array[0..2] of GLfloat = (
    (0.8, 0.4, 1.0),
    (0.3, 0.4, 1.0)
    );
var
  w, h: integer;
var
  mvp: TMat4;

begin
  with getGLFW, GetOpenGL do
  begin
    glfwSetErrorCallback(@ErrorCallback);

    if not glfwInit then
    begin
      WriteLn('Impossibile inizializzare GLFW');
      Halt(1);
    end;

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

    Windows[0] := glfwCreateWindow(400, 400, 'First', nil, nil);
    if not Assigned(Windows[0]) then
    begin
      glfwTerminate;
      Halt(1);
    end;

    glfwSetKeyCallback(Windows[0], @KeyCallback);

    glfwMakeContextCurrent(Windows[0]);
    glfwSwapInterval(1);
    CreateSharedObjects;

    Windows[1] := glfwCreateWindow(400, 400, 'Second', nil, Windows[0]);
    if not Assigned(Windows[1]) then
    begin
      glfwTerminate;
      Halt(1);
    end;

    glfwGetWindowSize(Windows[0], @Width, nil);
    glfwGetWindowFrameSize(Windows[0], @left, nil, @right, nil);
    glfwGetWindowPos(Windows[0], @xpos, @ypos);
    glfwSetWindowPos(Windows[1], xpos + Width + left + right, ypos);

    glfwSetKeyCallback(Windows[1], @KeyCallback);

    glfwMakeContextCurrent(Windows[1]);
    glUseProgram(ProgramID);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, Texture);
    glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
    glEnableVertexAttribArray(VposLocation);
    glVertexAttribPointer(VposLocation, 2, GL_FLOAT, GL_FALSE, SizeOf(TVertex2f), nil);

    // Loop principale
    while (glfwWindowShouldClose(Windows[0])) and (not glfwWindowShouldClose(Windows[1])) do
    begin

      for i := 0 to 1 do
      begin
        glfwGetFramebufferSize(Windows[i], w, h);
        glfwMakeContextCurrent(Windows[i]);

        glViewport(0, 0, w, h);

        Mat4Ortho(mvp, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0);
        glUniformMatrix4fv(MvpLocation, 1, GL_FALSE, @mvp[0, 0]);
        glUniform3fv(ColorLocation, 1, @Colors[i][0]);
        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

        glfwSwapBuffers(Windows[i]);
      end;

      glfwWaitEvents;
    end;

    glfwTerminate;
  end;
end.
