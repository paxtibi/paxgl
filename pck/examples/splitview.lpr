program splitview;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Math,
  pax.glfw,
  pax.gl;

var
  window: PGLFWwindow;
  Width, Height: integer;

  angle: single = 0.0;

  // ============================================================================
  // Disegna un cubo colorato semplice
  // ============================================================================
  procedure DrawCube;
  begin
    with getOpenGL do
    begin
      glBegin(GL_QUADS);

      // Front
      glColor3f(1.0, 0.0, 0.0);
      glVertex3f(-1.0, -1.0, 1.0);
      glVertex3f(1.0, -1.0, 1.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(-1.0, 1.0, 1.0);

      // Back
      glColor3f(0.0, 1.0, 0.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(-1.0, 1.0, -1.0);
      glVertex3f(1.0, 1.0, -1.0);
      glVertex3f(1.0, -1.0, -1.0);

      // Top
      glColor3f(0.0, 0.0, 1.0);
      glVertex3f(-1.0, 1.0, -1.0);
      glVertex3f(-1.0, 1.0, 1.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(1.0, 1.0, -1.0);

      // Bottom
      glColor3f(1.0, 1.0, 0.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(1.0, -1.0, -1.0);
      glVertex3f(1.0, -1.0, 1.0);
      glVertex3f(-1.0, -1.0, 1.0);

      // Right
      glColor3f(1.0, 0.0, 1.0);
      glVertex3f(1.0, -1.0, -1.0);
      glVertex3f(1.0, 1.0, -1.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(1.0, -1.0, 1.0);

      // Left
      glColor3f(0.0, 1.0, 1.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(-1.0, -1.0, 1.0);
      glVertex3f(-1.0, 1.0, 1.0);
      glVertex3f(-1.0, 1.0, -1.0);

      glEnd;
    end;
  end;

  // ============================================================================
  // Disegna la scena completa (due viewport split)
  // ============================================================================
  procedure Draw;
  var
    half_width: integer;
  begin
    with getOpenGL do
    begin
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      half_width := Width div 2;

      // === Viewport sinistro (vista frontale) ===
      glViewport(0, 0, half_width, Height);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(-2.0, 2.0, -2.0, 2.0, -10.0, 10.0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glPushMatrix;
      glRotatef(0.0, 1.0, 0.0, 0.0);  // Vista frontale
      glRotatef(angle, 0.0, 1.0, 0.0);
      DrawCube;
      glPopMatrix;

      // === Viewport destro (vista dall'alto) ===
      glViewport(half_width, 0, half_width, Height);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(-2.0, 2.0, -2.0, 2.0, -10.0, 10.0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glPushMatrix;
      glRotatef(90.0, 1.0, 0.0, 0.0);  // Vista dall'alto
      glRotatef(angle, 0.0, 0.0, 1.0);
      DrawCube;
      glPopMatrix;
    end;
  end;

  // ============================================================================
  // Animazione (rotazione automatica)
  // ============================================================================
  procedure Animate;
  begin
    angle := 100.0 * getGLFW.glfwGetTime();
  end;

  // ============================================================================
  // Callback tasto premuto
  // ============================================================================
  procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    if action <> GLFW_PRESS then Exit;

    with getGLFW do
    begin
      case key of
        GLFW_KEY_ESCAPE:
          glfwSetWindowShouldClose(window, True);
      end;
    end;
  end;

  // ============================================================================
  // Callback ridimensionamento finestra
  // ============================================================================
  procedure Reshape(window: PGLFWwindow; w, h: integer); cdecl;
  begin
    Width := w;
    Height := h;

    with getOpenGL do
    begin
      glViewport(0, 0, w, h);
    end;
  end;

  // ============================================================================
  // Inizializzazione OpenGL
  // ============================================================================
  procedure InitGL;
  begin
    with getOpenGL do
    begin
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_CULL_FACE);
      glCullFace(GL_BACK);
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

    window := glfwCreateWindow(800, 600, 'Split View Demo', nil, nil);
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

    glfwDestroyWindow(window);
    glfwTerminate;
  end;
end.
