program glboing;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Math,
  pax.gl,
  pax.glfw,
  LinMath;

const
  RADIUS = 70.0;
  STEP_LONGITUDE = 22.5;
  STEP_LATITUDE = 22.5;

  DIST_BALL = RADIUS * 2.0 + RADIUS * 0.1;
  VIEW_SCENE_DIST = DIST_BALL * 3.0 + 200.0;
  GRID_SIZE = RADIUS * 4.5;
  BOUNCE_HEIGHT = RADIUS * 2.1;
  BOUNCE_WIDTH = RADIUS * 2.1;

  SHADOW_OFFSET_X = -20.0;
  SHADOW_OFFSET_Y = 10.0;
  SHADOW_OFFSET_Z = 0.0;

  WALL_L_OFFSET = 0.0;
  WALL_R_OFFSET = 5.0;

  ANIMATION_SPEED = 50.0;
  MAX_DELTA_T = 0.02;

type
  TDrawBallMode = (DRAW_BALL, DRAW_BALL_SHADOW);

  TVertex = record
    x, y, z: single;
  end;

var
  window: PGLFWwindow;
  Width, Height: integer;
  windowed_xpos, windowed_ypos, windowed_width, windowed_height: integer;

  deg_rot_y: single = 0.0;
  deg_rot_y_inc: single = 2.0;

  override_pos: boolean = False;
  cursor_x, cursor_y: single;

  ball_x: single = -RADIUS;
  ball_y: single = -RADIUS;
  ball_x_inc: single = 1.0;
  ball_y_inc: single = 2.0;

  drawBallHow: TDrawBallMode;

  t, t_old, dt: double;

  function TruncateDeg(deg: single): single;
  begin
    if deg >= 360.0 then
      Result := deg - 360.0
    else
      Result := deg;
  end;

  function Deg2Rad(deg: double): double; inline;
  begin
    Result := deg / 360.0 * (2.0 * Pi);
  end;

  function SinDeg(deg: double): double; inline;
  begin
    Result := Sin(Deg2Rad(deg));
  end;

  function CosDeg(deg: double): double; inline;
  begin
    Result := Cos(Deg2Rad(deg));
  end;

  procedure CrossProduct(const a, b, c: TVertex; out n: TVertex);
  var
    u1, u2, u3, v1, v2, v3: single;
  begin
    u1 := b.x - a.x;
    u2 := b.y - a.y;
    u3 := b.z - a.z;
    v1 := c.x - a.x;
    v2 := c.y - a.y;
    v3 := c.z - a.z;

    n.x := u2 * v3 - v2 * u3;
    n.y := u3 * v1 - v3 * u1;
    n.z := u1 * v2 - v1 * u2;
  end;

  procedure BounceBall(delta_t: double);
  var
    sign: single;
    deg: double;
  begin
    if override_pos then Exit;

    // Rimbalzo sui muri laterali
    if ball_x > (BOUNCE_WIDTH / 2.0 + WALL_R_OFFSET) then
    begin
      ball_x_inc := -0.5 - 0.75 * Random;
      deg_rot_y_inc := -deg_rot_y_inc;
    end;
    if ball_x < -(BOUNCE_WIDTH / 2.0 + WALL_L_OFFSET) then
    begin
      ball_x_inc := 0.5 + 0.75 * Random;
      deg_rot_y_inc := -deg_rot_y_inc;
    end;

    // Rimbalzo su pavimento/tetto
    if ball_y > BOUNCE_HEIGHT / 2.0 then
      ball_y_inc := -0.75 - 1.0 * Random;
    if ball_y < -BOUNCE_HEIGHT / 2.0 * 0.85 then
      ball_y_inc := 0.75 + 1.0 * Random;

    // Aggiornamento posizione
    ball_x += ball_x_inc * (delta_t * ANIMATION_SPEED);
    ball_y += ball_y_inc * (delta_t * ANIMATION_SPEED);

    // Simulazione gravità
    if ball_y_inc < 0 then sign := -1.0
    else
      sign := 1.0;
    deg := (ball_y + BOUNCE_HEIGHT / 2.0) * 90.0 / BOUNCE_HEIGHT;
    if deg > 80.0 then deg := 80.0;
    if deg < 10.0 then deg := 10.0;
    ball_y_inc := sign * 4.0 * SinDeg(deg);
  end;

  procedure DrawBoingBallBand(long_lo, long_hi: single);
  var
    vert_ne, vert_nw, vert_sw, vert_se, vert_norm: TVertex;
    lat_deg: single;
    colorToggle: boolean = False;
  begin
    with GetOpenGL do
    begin
      lat_deg := 0.0;
      while lat_deg < 360.0 - STEP_LATITUDE do
      begin
        // Alternanza rosso/bianco
        if colorToggle then
          glColor3f(0.8, 0.1, 0.1)
        else
          glColor3f(0.95, 0.95, 0.95);
        colorToggle := not colorToggle;

        // Ombra: grigio scuro
        if drawBallHow = DRAW_BALL_SHADOW then
          glColor3f(0.35, 0.35, 0.35);

        // Coordinate Y
        vert_ne.y := CosDeg(long_hi) * RADIUS;
        vert_nw.y := CosDeg(long_hi) * RADIUS;
        vert_sw.y := CosDeg(long_lo) * RADIUS;
        vert_se.y := CosDeg(long_lo) * RADIUS;

        // Coordinate X,Z
        vert_ne.x := CosDeg(lat_deg) * (RADIUS * SinDeg(long_lo + STEP_LONGITUDE));
        vert_se.x := CosDeg(lat_deg) * (RADIUS * SinDeg(long_lo));
        vert_nw.x := CosDeg(lat_deg + STEP_LATITUDE) * (RADIUS * SinDeg(long_lo + STEP_LONGITUDE));
        vert_sw.x := CosDeg(lat_deg + STEP_LATITUDE) * (RADIUS * SinDeg(long_lo));

        vert_ne.z := SinDeg(lat_deg) * (RADIUS * SinDeg(long_lo + STEP_LONGITUDE));
        vert_se.z := SinDeg(lat_deg) * (RADIUS * SinDeg(long_lo));
        vert_nw.z := SinDeg(lat_deg + STEP_LATITUDE) * (RADIUS * SinDeg(long_lo + STEP_LONGITUDE));
        vert_sw.z := SinDeg(lat_deg + STEP_LATITUDE) * (RADIUS * SinDeg(long_lo));

        // Disegna quadrilatero
        glBegin(GL_POLYGON);
        CrossProduct(vert_ne, vert_nw, vert_sw, vert_norm);
        glNormal3f(vert_norm.x, vert_norm.y, vert_norm.z);

        glVertex3f(vert_ne.x, vert_ne.y, vert_ne.z);
        glVertex3f(vert_nw.x, vert_nw.y, vert_nw.z);
        glVertex3f(vert_sw.x, vert_sw.y, vert_sw.z);
        glVertex3f(vert_se.x, vert_se.y, vert_se.z);
        glEnd();
        lat_deg += STEP_LATITUDE;
      end;

    end;
  end;

  procedure DrawBoingBall;
  var
    lon_deg: single;
    dt_total, dt2: double;
  begin
    with GetOpenGL do
    begin
      glPushMatrix;

      glTranslatef(0.0, 0.0, DIST_BALL);

      // Aggiornamento fisica (suddiviso se dt troppo grande)
      dt_total := dt;
      while dt_total > 0.0 do
      begin
        dt2 := IfThen(dt_total > MAX_DELTA_T, MAX_DELTA_T, dt_total);
        dt_total -= dt2;
        BounceBall(dt2);
        deg_rot_y := TruncateDeg(deg_rot_y + deg_rot_y_inc * (dt2 * ANIMATION_SPEED));
      end;

      // Posizione palla
      glTranslatef(ball_x, ball_y, 0.0);

      // Offset ombra
      if drawBallHow = DRAW_BALL_SHADOW then
        glTranslatef(SHADOW_OFFSET_X, SHADOW_OFFSET_Y, SHADOW_OFFSET_Z);

      // Inclinazione
      glRotatef(-20.0, 0.0, 0.0, 1.0);

      // Rotazione continua
      glRotatef(deg_rot_y, 0.0, 1.0, 0.0);

      glCullFace(GL_FRONT);
      glEnable(GL_CULL_FACE);
      glEnable(GL_NORMALIZE);

      // Disegna fasce di latitudine
      lon_deg := 0.0;
      while lon_deg < 180.0 do
      begin
        DrawBoingBallBand(lon_deg, lon_deg + STEP_LONGITUDE);
        lon_deg += STEP_LONGITUDE;
      end;

      glPopMatrix;

    end;
  end;

  procedure DrawGrid;
  var
    row, col: integer;
    rowTotal: integer = 12;
    colTotal: integer = 12;
    widthLine: single = 2.0;
    sizeCell: single;
    z_offset: single = -40.0;
    xl, xr, yt, yb: single;
  begin
    with GetOpenGL do
    begin
      sizeCell := GRID_SIZE / rowTotal;

      glPushMatrix;
      glDisable(GL_CULL_FACE);
      glTranslatef(0.0, 0.0, DIST_BALL);

      // Linee verticali
      for col := 0 to colTotal do
      begin
        xl := -GRID_SIZE / 2.0 + col * sizeCell;
        xr := xl + widthLine;
        yt := GRID_SIZE / 2.0;
        yb := -GRID_SIZE / 2.0 - widthLine;

        glBegin(GL_POLYGON);
        glColor3f(0.6, 0.1, 0.6);
        glVertex3f(xr, yt, z_offset);
        glVertex3f(xl, yt, z_offset);
        glVertex3f(xl, yb, z_offset);
        glVertex3f(xr, yb, z_offset);
        glEnd;
      end;

      // Linee orizzontali
      for row := 0 to rowTotal do
      begin
        yt := GRID_SIZE / 2.0 - row * sizeCell;
        yb := yt - widthLine;
        xl := -GRID_SIZE / 2.0;
        xr := GRID_SIZE / 2.0 + widthLine;

        glBegin(GL_POLYGON);
        glColor3f(0.6, 0.1, 0.6);
        glVertex3f(xr, yt, z_offset);
        glVertex3f(xl, yt, z_offset);
        glVertex3f(xl, yb, z_offset);
        glVertex3f(xr, yb, z_offset);
        glEnd;
      end;

      glPopMatrix;

    end;
  end;

  procedure Display;
  begin
    with GetOpenGL do
    begin
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glPushMatrix;

      drawBallHow := DRAW_BALL_SHADOW;
      DrawBoingBall;

      DrawGrid;

      drawBallHow := DRAW_BALL;
      DrawBoingBall;

      glPopMatrix;

    end;
  end;

  procedure Reshape(window: PGLFWwindow; w, h: integer); cdecl;
  var
    projection, view: TMat4x4;
    eye, center, up: TVec3;
  begin
    with GetOpenGL do
    begin
      Width := w;
      Height := h;
      glViewport(0, 0, w, h);

      Mat4x4_Perspective(projection,
        2.0 * ArcTan(RADIUS / 200.0),
        w / h,
        1.0, VIEW_SCENE_DIST);
      glMatrixMode(GL_PROJECTION);
      glLoadMatrixf(@projection);

      eye := [0.0, 0.0, VIEW_SCENE_DIST];
      center := [0.0, 0.0, 0.0];
      up := [0.0, -1.0, 0.0];
      Mat4x4_Look_At(view, eye, center, up);

      glMatrixMode(GL_MODELVIEW);
      glLoadMatrixf(@view);

    end;
  end;

  procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  var
    monitor: PGLFWmonitor;
    mode: PGLFWvidmode;
  begin
    with getGLFW, GetOpenGL do
    begin
      if action <> GLFW_PRESS then Exit;

      if (key = GLFW_KEY_ESCAPE) and (mods = 0) then
        glfwSetWindowShouldClose(window, true);

      if ((key = GLFW_KEY_ENTER) and (mods = GLFW_MOD_ALT)) or ((key = GLFW_KEY_F11) and (mods = GLFW_MOD_ALT)) then
      begin
        monitor := glfwGetWindowMonitor(window);
        if monitor <> nil then
        begin
          glfwSetWindowMonitor(window, nil, windowed_xpos, windowed_ypos,
            windowed_width, windowed_height, 0);
        end
        else
        begin
          monitor := glfwGetPrimaryMonitor();
          if monitor <> nil then
          begin
            mode := glfwGetVideoMode(monitor);
            glfwGetWindowPos(window, @windowed_xpos, @windowed_ypos);
            glfwGetWindowSize(window, @windowed_width, @windowed_height);
            glfwSetWindowMonitor(window, monitor, 0, 0, mode^.Width, mode^.Height, mode^.refreshRate);
          end;
        end;
      end;

    end;
  end;

  procedure SetBallPos(x, y: single);
  begin
    ball_x := (Width / 2.0) - x;
    ball_y := y - (Height / 2.0);
  end;

  procedure MouseButtonCallback(window: PGLFWwindow; button, action, mods: integer); cdecl;
  begin
    if button <> GLFW_MOUSE_BUTTON_LEFT then Exit;

    if action = GLFW_PRESS then
    begin
      override_pos := True;
      SetBallPos(cursor_x, cursor_y);
    end
    else
      override_pos := False;
  end;

  procedure CursorPosCallback(window: PGLFWwindow; x, y: double); cdecl;
  begin
    cursor_x := x;
    cursor_y := y;

    if override_pos then
      SetBallPos(cursor_x, cursor_y);
  end;

  procedure InitGL;
  begin
    with GetOpenGL do
    begin
      glClearColor(0.55, 0.55, 0.55, 0.0);
      glShadeModel(GL_FLAT);
      glEnable(GL_DEPTH_TEST);

    end;
  end;

begin
  with GetOpenGL, getGLFW do
  begin
    Randomize;

    if not glfwInit() then
    begin
      Writeln(ErrOutput, 'Errore inizializzazione GLFW');
      Halt(1);
    end;

    window := glfwCreateWindow(400, 400, 'Boing (classic Amiga demo)', nil, nil);
    if window = nil then
    begin
      Writeln(ErrOutput, 'Impossibile creare finestra');
      glfwTerminate();
      Halt(1);
    end;

    glfwSetWindowAspectRatio(window, 1, 1);

    glfwSetFramebufferSizeCallback(window, @Reshape);
    glfwSetKeyCallback(window, @KeyCallback);
    glfwSetMouseButtonCallback(window, @MouseButtonCallback);
    glfwSetCursorPosCallback(window, @CursorPosCallback);

    glfwMakeContextCurrent(window);

    glfwSwapInterval(1);

    glfwGetFramebufferSize(window, @Width, @Height);
    Reshape(window, Width, Height);

    glfwSetTime(0.0);
    t_old := 0.0;

    InitGL;

    while not glfwWindowShouldClose(window) do
    begin
      t := glfwGetTime();
      dt := t - t_old;
      t_old := t;

      Display();

      glfwSwapBuffers(window);
      glfwPollEvents();
    end;

    glfwTerminate();
  end;
end.
