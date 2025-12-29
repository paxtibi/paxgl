program HeightmapDemo;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  SysUtils,
  Math,
  pax.gl,
  pax.glfw;

const
  MAP_SIZE = 10.0;
  MAP_NUM_VERTICES = 80;
  MAP_NUM_TOTAL_VERTICES = MAP_NUM_VERTICES * MAP_NUM_VERTICES;
  MAP_NUM_LINES = 3 * (MAP_NUM_VERTICES - 1) * (MAP_NUM_VERTICES - 1) + 2 * (MAP_NUM_VERTICES - 1);

  MAX_CIRCLE_SIZE = 5.0;
  MAX_DISPLACEMENT = 1.0;
  DISPLACEMENT_SIGN_LIMIT = 0.3;
  MAX_ITER = 200;
  NUM_ITER_AT_A_TIME = 1;

type
  TVertexData = record
    x, y, z: single;
  end;

  TMapVertices = array[0..MAP_NUM_TOTAL_VERTICES - 1] of single;
  TMap3DVertices = array[0..MAP_NUM_TOTAL_VERTICES - 1] of TVertexData;

var
  map_x: TMapVertices;
  map_y: TMapVertices;
  map_z: TMapVertices;

  map_indices: array of GLuint;

  shaderProgram: GLuint;
  vao: GLuint;
  vbo: array[0..3] of GLuint;

  projectionLoc, modelviewLoc: GLint;

  projectionMatrix: array[0..15] of single;
  modelviewMatrix: array[0..15] of single;

  // ----------------------------------------------------------------------------
  function LoadShader(const AType: GLenum; const ASource: string): GLuint;
  var
    shader: GLuint;
    status: GLint;
    infoLog: string;
  begin
    with GetOpenGL do
    begin
      Result := 0;
      shader := glCreateShader(AType);
      if shader = 0 then Exit;

      glShaderSource(shader, 1, @ASource, nil);
      glCompileShader(shader);
      glGetShaderiv(shader, GL_COMPILE_STATUS, @status);

      if status <> GL_TRUE then
      begin
        glGetShaderInfoLog(shader, 8192, nil, PGLchar(infoLog));
        WriteLn('Errore compilazione shader:');
        WriteLn(infoLog);
        glDeleteShader(shader);
        Exit;
      end;

      Result := shader;
    end;
  end;

  // ----------------------------------------------------------------------------
  function CreateShaderProgram(const vs, fs: string): GLuint;
  var
    vsObj, fsObj: GLuint;
    status: GLint;
    infoLog: string;
  begin
    with getOpenGl do
    begin
      Result := 0;

      vsObj := LoadShader(GL_VERTEX_SHADER, vs);
      if vsObj = 0 then Exit;

      fsObj := LoadShader(GL_FRAGMENT_SHADER, fs);
      if fsObj = 0 then
      begin
        glDeleteShader(vsObj);
        Exit;
      end;

      Result := glCreateProgram();
      glAttachShader(Result, vsObj);
      glAttachShader(Result, fsObj);
      glLinkProgram(Result);

      glGetProgramiv(Result, GL_LINK_STATUS, @status);
      if status <> GL_TRUE then
      begin
        glGetProgramInfoLog(Result, 8192, nil, PGLchar(infoLog));
        WriteLn('Errore link programma:');
        WriteLn(infoLog);
        glDeleteProgram(Result);
        Result := 0;
      end;

      glDeleteShader(vsObj);
      glDeleteShader(fsObj);

    end;
  end;

  // ----------------------------------------------------------------------------
  procedure InitMap;
  var
    i, j, k: integer;
    step: single;
    x, z: single;
    ref: integer;
  begin
    step := MAP_SIZE / (MAP_NUM_VERTICES - 1);
    k := 0;

    // Genera griglia piatta
    x := 0;
    for i := 0 to MAP_NUM_VERTICES - 1 do
    begin
      z := 0;
      for j := 0 to MAP_NUM_VERTICES - 1 do
      begin
        map_x[k] := x;
        map_y[k] := 0.0;
        map_z[k] := z;
        Inc(k);
        z := z + step;
      end;
      x := x + step;
    end;

    // Generazione indici per linee (wireframe)
    SetLength(map_indices, MAP_NUM_LINES * 2);
    k := 0;

    // bordo superiore
    for i := 0 to MAP_NUM_VERTICES - 2 do
    begin
      map_indices[k] := (i + 1) * MAP_NUM_VERTICES - 1;
      Inc(k);
      map_indices[k] := (i + 2) * MAP_NUM_VERTICES - 1;
      Inc(k);
    end;

    // bordo destro
    for i := 0 to MAP_NUM_VERTICES - 2 do
    begin
      map_indices[k] := (MAP_NUM_VERTICES - 1) * MAP_NUM_VERTICES + i;
      Inc(k);
      map_indices[k] := (MAP_NUM_VERTICES - 1) * MAP_NUM_VERTICES + i + 1;
      Inc(k);
    end;

    // griglia interna
    for i := 0 to MAP_NUM_VERTICES - 2 do
      for j := 0 to MAP_NUM_VERTICES - 2 do
      begin
        ref := i * MAP_NUM_VERTICES + j;

        // linea orizzontale
        map_indices[k] := ref;
        Inc(k);
        map_indices[k] := ref + 1;
        Inc(k);

        // linea verticale
        map_indices[k] := ref;
        Inc(k);
        map_indices[k] := ref + MAP_NUM_VERTICES;
        Inc(k);

        // diagonale (opzionale, per più dettaglio)
        map_indices[k] := ref;
        Inc(k);
        map_indices[k] := ref + MAP_NUM_VERTICES + 1;
        Inc(k);
      end;
  end;

  // ----------------------------------------------------------------------------
  procedure GenerateHeightmapCircle(out cx, cz, size_, disp: single);
  var
    sign: single;
  begin
    cx := Random * MAP_SIZE;
    cz := Random * MAP_SIZE;
    size_ := Random * MAX_CIRCLE_SIZE;

    sign := Random;
    if sign < DISPLACEMENT_SIGN_LIMIT then
      sign := -1.0
    else
      sign := 1.0;

    disp := sign * (Random * MAX_DISPLACEMENT);
  end;

  // ----------------------------------------------------------------------------
  procedure UpdateMap(iterations: integer);
  var
    cx, cz, r, d: single;
    i: integer;
    dx, dz, pd, newHeight: single;
  begin
    while iterations > 0 do
    begin
      GenerateHeightmapCircle(cx, cz, r, d);
      d := d / 2.0;

      for i := 0 to MAP_NUM_TOTAL_VERTICES - 1 do
      begin
        dx := cx - map_x[i];
        dz := cz - map_z[i];
        pd := 2.0 * Sqrt(dx * dx + dz * dz) / r;

        if Abs(pd) <= 1.0 then
        begin
          newHeight := d + Cos(pd * Pi) * d;
          map_y[i] := map_y[i] + newHeight;
        end;
      end;

      Dec(iterations);
    end;
  end;

  // ----------------------------------------------------------------------------
  procedure MakeMesh;
  var
    loc: GLint;
  begin
    with GetOpenGL do
    begin
      glGenVertexArrays(1, @vao);
      glGenBuffers(4, @vbo[0]);

      glBindVertexArray(vao);

      // Element buffer (indicizzazioni)
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo[3]);
      glBufferData(GL_ELEMENT_ARRAY_BUFFER,
        Length(map_indices) * SizeOf(GLuint), @map_indices[0], GL_STATIC_DRAW);

      // x
      loc := glGetAttribLocation(shaderProgram, 'x');
      glBindBuffer(GL_ARRAY_BUFFER, vbo[0]);
      glBufferData(GL_ARRAY_BUFFER, MAP_NUM_TOTAL_VERTICES * SizeOf(single), @map_x[0], GL_STATIC_DRAW);
      glEnableVertexAttribArray(loc);
      glVertexAttribPointer(loc, 1, GL_FLOAT, GL_FALSE, 0, nil);

      // z
      loc := glGetAttribLocation(shaderProgram, 'z');
      glBindBuffer(GL_ARRAY_BUFFER, vbo[2]);
      glBufferData(GL_ARRAY_BUFFER, MAP_NUM_TOTAL_VERTICES * SizeOf(single), @map_z[0], GL_STATIC_DRAW);
      glEnableVertexAttribArray(loc);
      glVertexAttribPointer(loc, 1, GL_FLOAT, GL_FALSE, 0, nil);

      // y (altezza - dinamico)
      loc := glGetAttribLocation(shaderProgram, 'y');
      glBindBuffer(GL_ARRAY_BUFFER, vbo[1]);
      glBufferData(GL_ARRAY_BUFFER, MAP_NUM_TOTAL_VERTICES * SizeOf(single), @map_y[0], GL_DYNAMIC_DRAW);
      glEnableVertexAttribArray(loc);
      glVertexAttribPointer(loc, 1, GL_FLOAT, GL_FALSE, 0, nil);

      glBindVertexArray(0);
    end;
  end;

  // ----------------------------------------------------------------------------
  procedure UpdateMesh;
  begin
    with GetOpenGL do
    begin
      glBindBuffer(GL_ARRAY_BUFFER, vbo[1]);
      glBufferSubData(GL_ARRAY_BUFFER, 0,
        MAP_NUM_TOTAL_VERTICES * SizeOf(single), @map_y[0]);
      glBindBuffer(GL_ARRAY_BUFFER, 0);
    end;
  end;

var
  window: PGLFWwindow;
  iterCount: integer = 0;
  lastUpdate: double;
  running: boolean = True;

const
  VertexShader = '#version 150'#10 + // 0
    'uniform mat4 project;'#10 +// 0
    'uniform mat4 modelview;'#10 +// 0
    'in float x;'#10 +// 0
    'in float y;'#10 +// 0
    'in float z;'#10 +// 0
    'void main() {'#10 +// 0
    '   gl_Position = project * modelview * vec4(x, y, z, 1.0);'#10 +// 0
    '}'#10;// 0

  FragmentShader = '#version 150'#10 +// 0
    'out vec4 color;'#10 +// 0
    'void main() {'#10 +// 0
    '    color = vec4(0.2, 1.0, 0.2, 1.0);'#10 +// 0
    '}'#10;

begin
  with GetOpenGL, getGLFW do
  begin
    Randomize;

    if glfwInit = 0 then
    begin
      WriteLn('Errore inizializzazione GLFW');
      Exit;
    end;

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);

    window := glfwCreateWindow(800, 600, 'Heightmap Demo - Pascal', nil, nil);
    if window = nil then
    begin
      glfwTerminate;
      Exit;
    end;

    glfwMakeContextCurrent(window);
    InitOpenGL;

    // Creazione shader
    shaderProgram := CreateShaderProgram(VertexShader, FragmentShader);
    if shaderProgram = 0 then
    begin
      glfwTerminate;
      Exit;
    end;

    glUseProgram(shaderProgram);
    projectionLoc := glGetUniformLocation(shaderProgram, 'project');
    modelviewLoc := glGetUniformLocation(shaderProgram, 'modelview');

    InitMap;
    MakeMesh;

    glEnable(GL_DEPTH_TEST);
    glClearColor(0.1, 0.1, 0.2, 1.0);

    lastUpdate := glfwGetTime;

    while running and (glfwWindowShouldClose(window) = 0) do
    begin
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glUseProgram(shaderProgram);
      glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, @projectionMatrix[0]);
      glUniformMatrix4fv(modelviewLoc, 1, GL_FALSE, @modelviewMatrix[0]);

      glBindVertexArray(vao);
      glDrawElements(GL_LINES, Length(map_indices), GL_UNSIGNED_INT, nil);
      glBindVertexArray(0);

      glfwSwapBuffers(window);
      glfwPollEvents;

      // Aggiornamento heightmap ogni ~0.2 secondi
      if glfwGetTime - lastUpdate > 0.2 then
      begin
        if iterCount < MAX_ITER then
        begin
          UpdateMap(NUM_ITER_AT_A_TIME);
          UpdateMesh;
          Inc(iterCount, NUM_ITER_AT_A_TIME);
        end;
        lastUpdate := glfwGetTime;
      end;
    end;

    glfwDestroyWindow(window);
    glfwTerminate;
  end;
end.
