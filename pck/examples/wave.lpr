program wave;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Math,
  pax.glfw,
  pax.gl;

var
  window: PGLFWwindow;
  Width: integer = 640;
  Height: integer = 480;

  paused: boolean = False;
  wireframe: boolean = False;
  speed: single = 1.0;

  // ============================================================================
  // Disegna l'onda sinusoidale animata
  // ============================================================================
  procedure DrawWave(t: double);
  const
    SEGMENTS = 200;
    AMPLITUDE = 0.5;
    FREQUENCY = 5.0;
  var
    i: integer;
    x, y: single;
  begin
    with getOpenGL do
    begin
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glTranslatef(0.0, 0.0, -1.0);

      if wireframe then
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
      else
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

      // Onda principale (sinusoidale)
      glBegin(GL_LINE_STRIP);
      glColor3f(1.0, 1.0, 1.0);
      for i := 0 to SEGMENTS do
      begin
        x := -1.0 + 2.0 * (i / SEGMENTS);
        y := AMPLITUDE * Sin(FREQUENCY * x + t * speed);
        glVertex2f(x, y);
      end;
      glEnd;

      // Onda secondaria (più veloce, più piccola)
      glBegin(GL_LINE_STRIP);
      glColor3f(1.0, 0.5, 0.5);
      for i := 0 to SEGMENTS do
      begin
        x := -1.0 + 2.0 * (i / SEGMENTS);
        y := 0.3 * AMPLITUDE * Sin(FREQUENCY * 2.5 * x + t * speed * 1.5);
        glVertex2f(x, y);
      end;
      glEnd;

      // Onda terza (lenta, grande)
      glBegin(GL_LINE_STRIP);
      glColor3f(0.5, 0.5, 1.0);
      for i := 0 to SEGMENTS do
      begin
        x := -1.0 + 2.0 * (i / SEGMENTS);
        y := 0.7 * AMPLITUDE * Sin(FREQUENCY * 0.8 * x + t * speed * 0.6);
        glVertex2f(x, y);
      end;
      glEnd;
    end;
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

        GLFW_KEY_SPACE:
          paused := not paused;

        GLFW_KEY_W:
          wireframe := not wireframe;

        GLFW_KEY_UP:
          speed += 0.5;

        GLFW_KEY_DOWN:
          speed := Max(0.1, speed - 0.5);
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

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

      glMatrixMode(GL_MODELVIEW);
    end;
  end;

  // ============================================================================
  // Inizializzazione OpenGL
  // ============================================================================
  procedure InitGL;
  begin
    with getOpenGL do
    begin
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glEnable(GL_LINE_SMOOTH);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glLineWidth(2.0);
    end;
  end;

  // ============================================================================
  // Main
  // ============================================================================
var
  t: double;
begin
  with getGLFW, GetOpenGL do
  begin
    if not glfwInit then
    begin
      WriteLn(ErrOutput, 'Errore inizializzazione GLFW');
      Halt(1);
    end;

    glfwWindowHint(GLFW_DEPTH_BITS, 16);

    window := glfwCreateWindow(640, 480, 'Wave Demo', nil, nil);
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
      if not paused then
        t := glfwGetTime();

      DrawWave(t);

      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glfwDestroyWindow(window);
    glfwTerminate;
  end;
end.
