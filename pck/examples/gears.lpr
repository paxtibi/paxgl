program gears;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Math,
  pax.gl,
  pax.glfw,
  LinMath;

var
  window: PGLFWwindow;
  Width, Height: integer;

  view_rotx: single = 20.0;
  view_roty: single = 30.0;
  view_rotz: single = 0.0;

  gear1, gear2, gear3: GLuint;
  angle: single = 0.0;

  procedure gear(inner_radius, outer_radius, Width: single; teeth: integer; tooth_depth: single);
  var
    i: integer;
    r0, r1, r2: single;
    angle, da: single;
    u, v, len: single;
  begin
    with GetOpenGL do
    begin
      r0 := inner_radius;
      r1 := outer_radius - tooth_depth / 2.0;
      r2 := outer_radius + tooth_depth / 2.0;

      da := 2.0 * Pi / teeth / 4.0;

      glShadeModel(GL_FLAT);
      glNormal3f(0.0, 0.0, 1.0);

      // Front face
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2.0 * Pi / teeth;
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
        angle := i * 2.0 * Pi / teeth;
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), Width * 0.5);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), Width * 0.5);
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), Width * 0.5);
      end;
      glEnd;

      glNormal3f(0.0, 0.0, -1.0);

      // Back face
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2.0 * Pi / teeth;
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
        angle := i * 2.0 * Pi / teeth;
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
        angle := i * 2.0 * Pi / teeth;

        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), Width * 0.5);
        glVertex3f(r1 * Cos(angle), r1 * Sin(angle), -Width * 0.5);

        u := r2 * Cos(angle + da) - r1 * Cos(angle);
        v := r2 * Sin(angle + da) - r1 * Sin(angle);
        len := Sqrt(u * u + v * v);
        if len > 0.0 then
        begin
          u /= len;
          v /= len;
        end;
        glNormal3f(v, -u, 0.0);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + da), r2 * Sin(angle + da), -Width * 0.5);

        glNormal3f(Cos(angle), Sin(angle), 0.0);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), Width * 0.5);
        glVertex3f(r2 * Cos(angle + 2 * da), r2 * Sin(angle + 2 * da), -Width * 0.5);

        u := r1 * Cos(angle + 3 * da) - r2 * Cos(angle + 2 * da);
        v := r1 * Sin(angle + 3 * da) - r2 * Sin(angle + 2 * da);
        if len > 0.0 then
        begin
          u /= len;
          v /= len;
        end;
        glNormal3f(v, -u, 0.0);
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), Width * 0.5);
        glVertex3f(r1 * Cos(angle + 3 * da), r1 * Sin(angle + 3 * da), -Width * 0.5);

        glNormal3f(Cos(angle), Sin(angle), 0.0);
      end;

      glVertex3f(r1 * Cos(0.0), r1 * Sin(0.0), Width * 0.5);
      glVertex3f(r1 * Cos(0.0), r1 * Sin(0.0), -Width * 0.5);
      glEnd;

      glShadeModel(GL_SMOOTH);

      // Inside radius cylinder
      glBegin(GL_QUAD_STRIP);
      for i := 0 to teeth do
      begin
        angle := i * 2.0 * Pi / teeth;
        glNormal3f(-Cos(angle), -Sin(angle), 0.0);
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), -Width * 0.5);
        glVertex3f(r0 * Cos(angle), r0 * Sin(angle), Width * 0.5);
      end;
      glEnd;
    end;
  end;

  procedure Draw;
  begin
    with GetOpenGL do
    begin
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glPushMatrix();
      begin
        glRotatef(view_rotx, 1.0, 0.0, 0.0);
        glRotatef(view_roty, 0.0, 1.0, 0.0);
        glRotatef(view_rotz, 0.0, 0.0, 1.0);

        glPushMatrix();
        begin
          glTranslatef(-3.0, -2.0, 0.0);
          glRotatef(angle, 0.0, 0.0, 1.0);
          glCallList(gear1);
        end;
        glPopMatrix();

        glPushMatrix();
        begin
          glTranslatef(3.1, -2.0, 0.0);
          glRotatef(-2.0 * angle - 9.0, 0.0, 0.0, 1.0);
          glCallList(gear2);
        end;
        glPopMatrix();

        glPushMatrix();
        begin
          glTranslatef(-3.10, 4.20, 0.0);
          glRotatef(-2.0 * angle - 25.0, 0.0, 0.0, 1.0);
          glCallList(gear3);
        end;
        glPopMatrix();
      end;

      glPopMatrix();
    end;
  end;

  procedure Animate;
  begin
    with getGLFW do
    begin
      angle := 100.0 * glfwGetTime();
    end;
  end;

  procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    with getGLFW do
    begin
      if action <> GLFW_PRESS then Exit;

      case key of
        GLFW_KEY_Z:
          if (mods and GLFW_MOD_SHIFT) <> 0 then
            view_rotz -= 5.0
          else
            view_rotz += 5.0;
        GLFW_KEY_ESCAPE:
          glfwSetWindowShouldClose(window, GLFW_TRUE);
        GLFW_KEY_UP:
          view_rotx += 5.0;
        GLFW_KEY_DOWN:
          view_rotx -= 5.0;
        GLFW_KEY_LEFT:
          view_roty += 5.0;
        GLFW_KEY_RIGHT:
          view_roty -= 5.0;
      end;
    end;
  end;

  procedure Reshape(window: PGLFWwindow; w, h: integer); cdecl;
  var
    aspect: single;
    znear, zfar, xmax: single;
  begin
    with GetOpenGL do
    begin
      Width := w;
      Height := h;

      glViewport(0, 0, w, h);

      aspect := h / w;
      znear := 5.0;
      zfar := 30.0;
      xmax := znear * 0.5;

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glFrustum(-xmax, xmax, -xmax * aspect, xmax * aspect, znear, zfar);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glTranslatef(0.0, 0.0, -20.0);
    end;
  end;

  procedure InitGL;
  var
    pos: array[0..3] of single = (5.0, 5.0, 10.0, 0.0);
    red: array[0..3] of single = (0.8, 0.1, 0.0, 1.0);
    green: array[0..3] of single = (0.0, 0.8, 0.2, 1.0);
    blue: array[0..3] of single = (0.2, 0.2, 1.0, 1.0);
  begin
    with GetOpenGL do
    begin
      glLightfv(GL_LIGHT0, GL_POSITION, @pos);
      glEnable(GL_CULL_FACE);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);

      // Crea display lists per gli ingranaggi
      gear1 := glGenLists(1);
      begin
        glNewList(gear1, GL_COMPILE);
        glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @red);
        gear(1.0, 4.0, 1.0, 20, 0.7);
        glEndList;
      end;

      gear2 := glGenLists(1);
      begin
        glNewList(gear2, GL_COMPILE);
        glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @green);
        gear(0.5, 2.0, 2.0, 10, 0.7);
        glEndList;
      end;

      gear3 := glGenLists(1);
      begin
        glNewList(gear3, GL_COMPILE);
        glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, @blue);
        gear(1.3, 2.0, 0.5, 10, 0.7);
        glEndList;
      end;
      glEnable(GL_NORMALIZE);
    end;
  end;

begin
  with getGLFW do
  begin
    if glfwInit() = GLFW_FALSE then
    begin
      Writeln(ErrOutput, 'Failed to initialize GLFW');
      Halt(1);
    end;

    glfwWindowHint(GLFW_DEPTH_BITS, 16);
    glfwWindowHint(GLFW_TRANSPARENT_FRAMEBUFFER, GLFW_TRUE);

    window := glfwCreateWindow(300, 300, 'Gears', nil, nil);
    if window = nil then
    begin
      Writeln(ErrOutput, 'Failed to open GLFW window');
      glfwTerminate();
      Halt(1);
    end;

    glfwSetFramebufferSizeCallback(window, @Reshape);
    glfwSetKeyCallback(window, @KeyCallback);

    glfwMakeContextCurrent(window);

    glfwSwapInterval(1);

    glfwGetFramebufferSize(window, @Width, @Height);
    Reshape(window, Width, Height);

    InitGL;

    while glfwWindowShouldClose(window) = GLFW_FALSE do
    begin
      Animate;
      Draw;

      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glfwTerminate;

  end;
end.
