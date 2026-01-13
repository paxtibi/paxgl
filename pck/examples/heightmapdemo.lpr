program HeightmapDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, Math,
  pax.glfw, pax.gl;

const
  MAP_W = 64;        // Larghezza heightmap
  MAP_H = 64;        // Altezza heightmap
  MAP_SIZE = MAP_W * MAP_H;

var
  window: PGLFWwindow;
  Width, Height: integer;

  view_rotx: single = 20.0;
  view_roty: single = 30.0;
  view_rotz: single = 0.0;

  heightmap: array[0..MAP_SIZE-1] of GLubyte;  // Altezze (0..255)
  texcoord: array[0..MAP_SIZE*2-1] of GLfloat; // Coordinate texture

  texture_id: GLuint = 0;

// ============================================================================
// Genera heightmap semplice (sinusoidale + rumore)
// ============================================================================
procedure GenerateHeightmap;
var
  x, y, i: Integer;
  value: single;
begin
  for y := 0 to MAP_H - 1 do
  begin
    for x := 0 to MAP_W - 1 do
    begin
      i := y * MAP_W + x;
      value := Sin(x * 0.1) * Cos(y * 0.1) * 0.5 + 0.5;  // Base ondulata
      value += Random * 0.2;                              // Rumore
      heightmap[i] := Round(value * 255);
    end;
  end;

  // Precalcola coordinate texture
  for y := 0 to MAP_H - 1 do
    for x := 0 to MAP_W - 1 do
    begin
      i := (y * MAP_W + x) * 2;
      texcoord[i]   := x / (MAP_W - 1.0);
      texcoord[i+1] := y / (MAP_H - 1.0);
    end;
end;

// ============================================================================
// Disegna la heightmap come griglia di triangoli
// ============================================================================
procedure DrawHeightmap;
var
  x, y: Integer;
  h1, h2, h3, h4: GLubyte;
  scale: single = 0.05;  // Scala verticale
begin
  with getOpenGL do
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture_id);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);

    glNormalPointer(GL_FLOAT, 0, @heightmap[0]);  // Normale approssimata da altezza
    glTexCoordPointer(2, GL_FLOAT, 0, @texcoord[0]);

    glPushMatrix;
    glScalef(1.0, scale, 1.0);  // Scala verticale

    for y := 0 to MAP_H - 2 do
    begin
      glBegin(GL_TRIANGLE_STRIP);
      for x := 0 to MAP_W - 1 do
      begin
        h1 := heightmap[y * MAP_W + x];
        h2 := heightmap[(y+1) * MAP_W + x];

        glVertex3f(x - MAP_W/2, h1, y - MAP_H/2);
        glVertex3f(x - MAP_W/2, h2, y+1 - MAP_H/2);
      end;
      glEnd;
    end;

    glPopMatrix;

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);

    glDisable(GL_TEXTURE_2D);
  end;
end;

// ============================================================================
// Setup luci e materiali
// ============================================================================
procedure SetupLights;
const
  light_pos: array[0..3] of single = (5.0, 5.0, 10.0, 1.0);
  ambient: array[0..3] of single = (0.3, 0.3, 0.3, 1.0);
  diffuse: array[0..3] of single = (0.8, 0.8, 0.8, 1.0);
begin
  with getOpenGL do
  begin
    glLightfv(GL_LIGHT0, GL_POSITION, @light_pos);
    glLightfv(GL_LIGHT0, GL_AMBIENT, @ambient);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, @diffuse);

    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
  end;
end;

// ============================================================================
// Inizializzazione
// ============================================================================
procedure InitGL;
const
  tex_data: array[0..63] of GLubyte = (
    $80,$80,$80,$FF, $A0,$A0,$A0,$FF, $80,$80,$80,$FF, $60,$60,$60,$FF,
    $A0,$A0,$A0,$FF, $C0,$C0,$C0,$FF, $A0,$A0,$A0,$FF, $80,$80,$80,$FF,
    $80,$80,$80,$FF, $A0,$A0,$A0,$FF, $80,$80,$80,$FF, $60,$60,$60,$FF,
    $60,$60,$60,$FF, $80,$80,$80,$FF, $A0,$A0,$A0,$FF, $80,$80,$80,$FF
  );
begin
  with getOpenGL do
  begin
    glGenTextures(1, @texture_id);
    glBindTexture(GL_TEXTURE_2D, texture_id);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 4, 4, 0, GL_RGBA,
                 GL_UNSIGNED_BYTE, @tex_data);

    GenerateHeightmap;
    SetupLights;
  end;
end;

// ============================================================================
// Disegna scena
// ============================================================================
procedure Draw;
begin
  with getOpenGL do
  begin
    glClearColor(0.2, 0.3, 0.8, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glTranslatef(0.0, -2.0, -10.0);

    glRotatef(view_rotx, 1.0, 0.0, 0.0);
    glRotatef(view_roty, 0.0, 1.0, 0.0);
    glRotatef(view_rotz, 0.0, 0.0, 1.0);

    DrawHeightmap;
  end;
end;

// ============================================================================
// Animazione (rotazione automatica)
// ============================================================================
procedure Animate;
begin
  view_roty += 0.5;
  if view_roty > 360 then view_roty -= 360;
end;

// ============================================================================
// Callback tasto
// ============================================================================
procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: Integer); cdecl;
begin
  if action <> GLFW_PRESS then Exit;

  with getGLFW do
  begin
    case key of
      GLFW_KEY_ESCAPE:
        glfwSetWindowShouldClose(window, True);
      GLFW_KEY_UP:    view_rotx += 5.0;
      GLFW_KEY_DOWN:  view_rotx -= 5.0;
      GLFW_KEY_LEFT:  view_roty += 5.0;
      GLFW_KEY_RIGHT: view_roty -= 5.0;
    end;
  end;
end;

// ============================================================================
// Callback ridimensionamento
// ============================================================================
procedure Reshape(window: PGLFWwindow; w, h: Integer); cdecl;
var
  aspect: single;
begin
  Width := w;
  Height := h;

  with getOpenGL do
  begin
    glViewport(0, 0, w, h);

    if h = 0 then h := 1;
    aspect := w / h;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glFrustum(-aspect, aspect, -1.0, 1.0, 1.0, 100.0);

    glMatrixMode(GL_MODELVIEW);
  end;
end;

// ============================================================================
// Main
// ============================================================================
begin
  with getGLFW, GetOpenGL do
  begin
    if not glfwInit then
    begin
      WriteLn(ErrOutput, 'Errore inizializzazione GLFW');
      Halt(1);
    end;

    glfwWindowHint(GLFW_DEPTH_BITS, 24);

    window := glfwCreateWindow(640, 480, 'Heightmap Demo', nil, nil);
    if window = nil then
    begin
      WriteLn(ErrOutput, 'Errore creazione finestra');
      glfwTerminate;
      Halt(1);
    end;

    glfwMakeContextCurrent(window);


    WriteLn('OpenGL Version: ', PChar(glGetString(GL_VERSION)));

    glfwSwapInterval(1);

    glfwSetFramebufferSizeCallback(window, @Reshape);
    glfwSetKeyCallback(window, @KeyCallback);

    glfwGetFramebufferSize(window, @Width, @Height);
    Reshape(window, Width, Height);

    InitGL;

    while not glfwWindowShouldClose(window) do
    begin
      Animate;
      Draw;

      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    if texture_id <> 0 then
      glDeleteTextures(1, @texture_id);

    glfwDestroyWindow(window);
    glfwTerminate;
  end;
end.
