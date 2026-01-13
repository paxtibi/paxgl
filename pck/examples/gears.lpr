program gears;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Math,
  pax.glfw,
  pax.gl;

var
  window: PGLFWwindow;
  Width, Height: integer;

  view_rotx: single = 20.0;
  view_roty: single = 30.0;
  view_rotz: single = 0.0;

  gear1, gear2, gear3: GLuint;
  angle: single = 0.0;

  // ============================================================================
  // Disegna un ingranaggio 3D
  // ============================================================================
  procedure Gear(inner_radius, outer_radius, Width: GLfloat; teeth: GLint; tooth_depth: GLfloat);
  var
    i: integer;
    r0, r1, r2: GLfloat;
    angle, da: GLfloat;
    u, v, len: GLfloat;
  begin
    with getOpenGL do
    begin
      r0 := inner_radius;
      r1 := outer_radius - tooth_depth / 2;
      r2 := outer_radius + tooth_depth / 2;

      da := 2 * Pi / teeth / 4;

      glShadeModel(GL_FLAT);
      glNormal3f(0, 0, 1);

      // Front face
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2 * Pi / teeth;
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), Width * 0.5);
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), Width * 0.5);
        if i < teeth then
        begin
          glVertex3f(r0 * Cos(angle), r0 * Sin(angle), Width * 0.5);
          glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), Width * 0.5);
        end;
      end;
      glEnd;

      // Front sides of teeth
      glBegin(GL_QUADS);
      for i := 0 to teeth - 1 do
      begin
        angle := i * 2 * Pi / teeth;
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), Width * 0.5);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), Width * 0.5);
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), Width * 0.5);
      end;
      glEnd;

      glNormal3f(0, 0, -1);

      // Back face
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2 * Pi / teeth;
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), -Width * 0.5);
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), -Width * 0.5);
        if i < teeth then
        begin
          glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), -Width * 0.5);
          glVertex3f(r0 * Cos(angle), r0 * Sin(angle), -Width * 0.5);
        end;
      end;
      glEnd;

      // Back sides of teeth
      glBegin(GL_QUADS);
      for i := 0 to teeth - 1 do
      begin
        angle := i * 2 * Pi / teeth;
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), -Width * 0.5);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), -Width * 0.5);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), -Width * 0.5);
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), -Width * 0.5);
      end;
      glEnd;

      // Outward faces of teeth
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth - 1 do
      begin
        angle := i * 2 * Pi / teeth;

        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), Width * 0.5);
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), -Width * 0.5);

        u := r2 * Cos(angle + da) - r1 * Cos(angle);
        v := r2 * Sin(angle + da) - r1 * Sin(angle);
        len := Sqrt(u * u + v * v);
        if len > 0 then
        begin
          u := u / len;
          v := v / len;
        end;

        glNormal3f(v, -u, 0);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), -Width * 0.5);

        glNormal3f(Cos(angle), Sin(angle), 0);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), -Width * 0.5);

        u := r1 * Cos(angle + 3 * da) - r2 * Cos(angle + 2 * da);
        v := r1 * Sin(angle + 3 * da) - r2 * Sin(angle + 2 * da);
        glNormal3f(v, -u, 0);

        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), Width * 0.5);
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), -Width * 0.5);

        glNormal3f(Cos(angle), Sin(angle), 0);
      end;

      glVertex3f(r1 * Cos(0), r1 * Sin(0), Width * 0.5);
      glVertex3f(r1 * Cos(0), r1 * Sin(0), -Width * 0.5);
      glEnd;

      glShadeModel(GL_SMOOTH);

      // Cilindro interno
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2 * Pi / teeth;
        glNormal3f(-Cos(angle), -Sin(angle), 0);
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), -Width * 0.5);
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), Width * 0.5);
      end;
      glEnd;
    end;
  end;

  // ============================================================================
  // Disegna la scena completa
  // ============================================================================
  procedure Draw;
  begin
    with getOpenGL do
    begin
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glPushMatrix;
      glRotatef(view_rotx, 1.0, 0.0, 0.0);
      glRotatef(view_roty, 0.0, 1.0, 0.0);
      glRotatef(view_rotz, 0.0, 0.0, 1.0);

      glPushMatrix;
      glTranslatef(-3.0, -2.0, 0.0);
      glRotatef(angle, 0.0, 0.0, 1.0);
      glCallList(gear1);
      glPopMatrix;

      glPushMatrix;
      glTranslatef(3.1, -2.0, 0.0);
      glRotatef(-2.0 * angle - 9.0, 0.0, 0.0, 1.0);
      glCallList(gear2);
      glPopMatrix;

      glPushMatrix;
      glTranslatef(-3.1, 4.2, 0.0);
      glRotatef(-2.0 * angle - 25.0, 0.0, 0.0, 1.0);
      glCallList(gear3);
      glPopMatrix;

      glPopMatrix;
    end;
  end;

  // ============================================================================
  // Animazione (aggiornamento angolo)
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

        GLFW_KEY_UP: view_rotx += 5.0;
        GLFW_KEY_DOWN: view_rotx -= 5.0;
        GLFW_KEY_LEFT: view_roty += 5.0;
        GLFW_KEY_RIGHT: view_roty -= 5.0;

        GLFW_KEY_Z:
          if (mods and GLFW_MOD_SHIFT) <> 0 then
            view_rotz -= 5.0
          else
            view_rotz += 5.0;
      end;
    end;
  end;

  // ============================================================================
  // Callback ridimensionamento finestra
  // ============================================================================
  procedure Reshape(window: PGLFWwindow; w, h: integer); cdecl;
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
      glFrustum(-1.0, 1.0, -aspect, aspect, 5.0, 60.0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glTranslatef(0.0, 0.0, -20.0);
    end;
  end;

  // ============================================================================
  // Inizializzazione OpenGL + ingranaggi
  // ============================================================================
  procedure InitGL;
  const
    pos: array[0..3] of single = (5.0, 5.0, 10.0, 0.0);
    red: array[0..3] of single = (0.8, 0.1, 0.0, 1.0);
    green: array[0..3] of single = (0.0, 0.8, 0.2, 1.0);
    blue: array[0..3] of single = (0.2, 0.2, 1.0, 1.0);
  begin
    with getOpenGL do
    begin
      glLightfv(GL_LIGHT0, GL_POSITION, @pos);
      glEnable(GL_CULL_FACE);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);

      // Ingranaggio 1 (rosso)
      gear1 := glGenLists(1);
      glNewList(gear1, GL_COMPILE);
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @red);
      Gear(1.0, 4.0, 1.0, 20, 0.7);
      glEndList;

      // Ingranaggio 2 (verde)
      gear2 := glGenLists(1);
      glNewList(gear2, GL_COMPILE);
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @green);
      Gear(0.5, 2.0, 2.0, 10, 0.7);
      glEndList;

      // Ingranaggio 3 (blu)
      gear3 := glGenLists(1);
      glNewList(gear3, GL_COMPILE);
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @blue);
      Gear(1.3, 2.0, 0.5, 10, 0.7);
      glEndList;

      glEnable(GL_NORMALIZE);
    end;
  end;

  // ============================================================================
  // Programma principale
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

    window := glfwCreateWindow(640, 480, 'Gears', nil, nil);
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

    glfwGetFramebufferSize(window, Width, Height);
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
