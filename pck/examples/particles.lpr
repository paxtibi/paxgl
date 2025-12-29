{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
program GLParticles;

uses
  SysUtils,
  Math,
  LinMath,
  syncobjs,
  pax.gl,
  pax.glfw;

  // ============================================================================
  // Definizioni di tipi
  // ============================================================================
type
  TVec3 = record
    x, y, z: single;
  end;

  // Struttura per array di vertici interleaved
  TVertex = record
    s, t: GLfloat;         // Coordinate texture
    rgba: GLuint;          // Colore packed (4 byte)
    x, y, z: GLfloat;         // Coordinate vertice
  end;
  PVertex = ^TVertex;

  // ============================================================================
  // Variabili globali di controllo programma
  // ============================================================================
var
  aspect_ratio: single;
  wireframe: boolean = False;

  // Struttura per sincronizzazione tra thread
type
  TThreadSync = record
    t: double;     // tempo assoluto (secondi)
    dt: single;     // delta time ultimo frame
    p_frame: integer;    // contatore frame fisica
    d_frame: integer;    // contatore frame disegno
    p_done: boolean;    // fisica terminata per questo frame
    d_done: boolean;    // disegno terminato
    particles_lock: TMutex;
  end;

var
  thread_sync: TThreadSync;

  // ============================================================================
  // Texture (hard-coded)
  // ============================================================================
const
  P_TEX_WIDTH = 8;
  P_TEX_HEIGHT = 8;
  F_TEX_WIDTH = 16;
  F_TEX_HEIGHT = 16;

var
  particle_tex_id: GLuint = 0;
  floor_tex_id: GLuint = 0;

const
  particle_texture: array[0..P_TEX_WIDTH * P_TEX_HEIGHT - 1] of byte = (
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $11, $22, $22, $11, $00, $00,
    $00, $11, $33, $88, $77, $33, $11, $00,
    $00, $22, $88, $ff, $ee, $77, $22, $00,
    $00, $22, $77, $ee, $ff, $88, $22, $00,
    $00, $11, $33, $77, $88, $33, $11, $00,
    $00, $00, $11, $33, $22, $11, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
    );

  floor_texture: array[0..F_TEX_WIDTH * F_TEX_HEIGHT - 1] of byte = (
    $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $30, $30, $30, $30, $30, $30, $30, $30,
    $ff, $f0, $cc, $f0, $f0, $f0, $ff, $f0, $30, $30, $30, $30, $30, $30, $30, $30,
    $f0, $cc, $ee, $ff, $f0, $f0, $f0, $f0, $30, $66, $30, $30, $30, $20, $30, $30,
    $f0, $f0, $f0, $f0, $f0, $ee, $f0, $f0, $30, $30, $30, $30, $30, $30, $30, $30,
    $f0, $f0, $f0, $f0, $cc, $f0, $f0, $f0, $30, $30, $55, $30, $30, $44, $30, $30,
    $f0, $dd, $f0, $f0, $f0, $f0, $f0, $f0, $33, $30, $30, $30, $30, $30, $30, $30,
    $f0, $f0, $f0, $f0, $f0, $ff, $f0, $f0, $30, $30, $30, $30, $30, $30, $60, $30,
    $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $33, $33, $30, $30, $30, $30, $30, $30,
    $30, $30, $30, $30, $30, $30, $33, $30, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0,
    $30, $30, $30, $30, $30, $20, $30, $30, $f0, $ff, $f0, $f0, $dd, $f0, $f0, $ff,
    $30, $30, $30, $30, $30, $30, $55, $33, $f0, $f0, $f0, $f0, $f0, $ff, $f0, $f0,
    $30, $44, $66, $30, $30, $30, $30, $30, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0,
    $30, $30, $30, $30, $30, $30, $30, $30, $f0, $f0, $f0, $aa, $f0, $f0, $cc, $f0,
    $30, $30, $30, $30, $30, $30, $30, $30, $ff, $f0, $f0, $f0, $ff, $f0, $dd, $f0,
    $30, $30, $30, $77, $30, $30, $30, $30, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0,
    $30, $30, $30, $30, $30, $30, $30, $30, $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0
    );

  // ============================================================================
  // Costanti parametri motore particelle
  // ============================================================================
const
  MAX_PARTICLES = 3000;
  LIFE_SPAN = 8.0;
  BIRTH_INTERVAL = LIFE_SPAN / MAX_PARTICLES;
  PARTICLE_SIZE = 0.7;
  GRAVITY = 9.8;
  cVELOCITY = 8.0;
  FRICTION = 0.75;
  FOUNTAIN_HEIGHT = 3.0;
  FOUNTAIN_RADIUS = 1.6;
  MIN_DELTA_T = BIRTH_INTERVAL * 0.5;

  // ============================================================================
  // Struttura e variabili globali particelle
  // ============================================================================
type
  TParticle = record
    x, y, z: single;     // posizione
    vx, vy, vz: single;     // velocità
    r, g, b: single;     // colore
    life: single;     // vita rimanente (1..0)
    active: boolean;
  end;
  PParticle = ^TParticle;

var
  particles: array[0..MAX_PARTICLES - 1] of TParticle;
  min_age: single = 0.0;

  glow_color: array[0..3] of GLfloat;
  glow_pos: array[0..3] of GLfloat;

  // ============================================================================
  // Materiali e nebbia
  // ============================================================================
const
  fountain_diffuse: array[0..3] of GLfloat = (0.7, 1.0, 1.0, 1.0);
  fountain_specular: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  fountain_shininess = 12.0;

  floor_diffuse: array[0..3] of GLfloat = (1.0, 0.6, 0.6, 1.0);
  floor_specular: array[0..3] of GLfloat = (0.6, 0.6, 0.6, 1.0);
  floor_shininess = 18.0;

  fog_color: array[0..3] of GLfloat = (0.1, 0.1, 0.1, 1.0);

  // ============================================================================
  // Inizializzazione nuova particella
  // ============================================================================
  procedure init_particle(p: PParticle; t: double);
  var
    xy_angle, velocity: single;
  begin
    p^.x := 0.0;
    p^.y := 0.0;
    p^.z := FOUNTAIN_HEIGHT;

    p^.vz := 0.7 + (0.3 / 4096.0) * (Random(4096));

    xy_angle := (2.0 * PI / 4096.0) * Random(4096);
    p^.vx := 0.4 * cos(xy_angle);
    p^.vy := 0.4 * sin(xy_angle);

    velocity := cVELOCITY * (0.8 + 0.1 * (sin(0.5 * t) + sin(1.31 * t)));
    p^.vx *= velocity;
    p^.vy *= velocity;
    p^.vz *= velocity;

    p^.r := 0.7 + 0.3 * sin(0.34 * t + 0.1);
    p^.g := 0.6 + 0.4 * sin(0.63 * t + 1.1);
    p^.b := 0.6 + 0.4 * sin(0.91 * t + 2.1);

    // Per la luce della fontana
    glow_pos[0] := 0.4 * sin(1.34 * t);
    glow_pos[1] := 0.4 * sin(3.11 * t);
    glow_pos[2] := FOUNTAIN_HEIGHT + 1.0;
    glow_pos[3] := 1.0;

    glow_color[0] := p^.r;
    glow_color[1] := p^.g;
    glow_color[2] := p^.b;
    glow_color[3] := 1.0;

    p^.life := 1.0;
    p^.active := True;
  end;

  // ============================================================================
  // Aggiornamento fisica di una singola particella
  // ============================================================================
const
  FOUNTAIN_R2 = sqr(FOUNTAIN_RADIUS + PARTICLE_SIZE / 2);

  procedure update_particle(p: PParticle; dt: single);
  begin
    if not p^.active then Exit;

    p^.life -= dt * (1.0 / LIFE_SPAN);
    if p^.life <= 0.0 then
    begin
      p^.active := False;
      Exit;
    end;

    p^.vz -= GRAVITY * dt;

    p^.x += p^.vx * dt;
    p^.y += p^.vy * dt;
    p^.z += p^.vz * dt;

    // Collisioni semplici
    if p^.vz < 0 then
    begin
      // Rimbalzo sulla fontana
      if (p^.x * p^.x + p^.y * p^.y < FOUNTAIN_R2) and (p^.z < FOUNTAIN_HEIGHT + PARTICLE_SIZE / 2) then
      begin
        p^.vz := -FRICTION * p^.vz;
        p^.z := FOUNTAIN_HEIGHT + PARTICLE_SIZE / 2 + FRICTION * (FOUNTAIN_HEIGHT + PARTICLE_SIZE / 2 - p^.z);
      end
      // Rimbalzo sul pavimento
      else if p^.z < PARTICLE_SIZE / 2 then
      begin
        p^.vz := -FRICTION * p^.vz;
        p^.z := PARTICLE_SIZE / 2 + FRICTION * (PARTICLE_SIZE / 2 - p^.z);
      end;
    end;
  end;

  // ============================================================================
  // Motore principale delle particelle (fisica)
  // ============================================================================
  procedure particle_engine(t: double; dt: single);
  var
    i: integer;
    dt2: single;
  begin
    while dt > 0.0 do
    begin
      dt2 := Min(dt, MIN_DELTA_T);

      for i := 0 to MAX_PARTICLES - 1 do
        update_particle(@particles[i], dt2);

      min_age += dt2;

      while min_age >= BIRTH_INTERVAL do
      begin
        min_age -= BIRTH_INTERVAL;

        for i := 0 to MAX_PARTICLES - 1 do
          if not particles[i].active then
          begin
            init_particle(@particles[i], t + min_age);
            update_particle(@particles[i], min_age);
            Break;
          end;
      end;

      dt -= dt2;
    end;
  end;

const
  BATCH_PARTICLES = 70;
  PARTICLE_VERTS = 4;

  procedure draw_particles(window: PGLFWwindow; t: double; dt: single);
  var
    i, particle_count: integer;
    vertex_array: array[0..BATCH_PARTICLES * PARTICLE_VERTS - 1] of TVertex;
    vptr: PVertex;
    alpha: single;
    rgba: GLuint;
    quad_ll, quad_lr: TVec3;
    mat: array[0..15] of GLfloat;
    pptr: PParticle;
  begin
    with getOpenGL, getGLFW do
    begin
      // Ottieni la matrice modelview (solo la parte rotazionale 3x3 ci serve)
      glGetFloatv(GL_MODELVIEW_MATRIX, @mat);

      // Calcolo dei vettori per i 4 angoli del billboard (lower-left e lower-right)
      quad_ll.x := (-PARTICLE_SIZE / 2) * (mat[0] + mat[1]);
      quad_ll.y := (-PARTICLE_SIZE / 2) * (mat[4] + mat[5]);
      quad_ll.z := (-PARTICLE_SIZE / 2) * (mat[8] + mat[9]);

      quad_lr.x := (PARTICLE_SIZE / 2) * (mat[0] - mat[1]);
      quad_lr.y := (PARTICLE_SIZE / 2) * (mat[4] - mat[5]);
      quad_lr.z := (PARTICLE_SIZE / 2) * (mat[8] - mat[9]);

      glDepthMask(GL_FALSE);              // Non scrivere nel depth buffer
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);

      if not wireframe then
      begin
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, particle_tex_id);
      end;

      // Preparazione array interleaved (T2F_C4UB_V3F)
      glInterleavedArrays(GL_T2F_C4UB_V3F, 0, @vertex_array);

      // Attendi che la fisica abbia finito questo frame
      thread_sync.particles_lock.Acquire;
      while (glfwWindowShouldClose(window) = 0) and (thread_sync.p_frame <= thread_sync.d_frame) do
      begin
        // Piccola attesa con timeout (100ms)
        Sleep(1);
      end;

      // Comunichiamo alla fisica il tempo attuale
      thread_sync.t := t;
      thread_sync.dt := dt;
      Inc(thread_sync.d_frame);

      // Costruiamo i vertici
      particle_count := 0;
      vptr := @vertex_array;
      pptr := @particles[0];

      for i := 0 to MAX_PARTICLES - 1 do
      begin
        if pptr^.active then
        begin
          // Intensità (rimane al massimo per il 75% della vita, poi sfuma)
          alpha := 4.0 * pptr^.life;
          if alpha > 1.0 then alpha := 1.0;

          // Colore packed in uint32 (RGBA)
          rgba := (Round(pptr^.r * 255) shl 0) or (Round(pptr^.g * 255) shl 8) or (Round(pptr^.b * 255) shl 16) or (Round(alpha * 255) shl 24);

          // Lower Left
          vptr^.s := 0.0;
          vptr^.t := 0.0;
          vptr^.rgba := rgba;
          vptr^.x := pptr^.x + quad_ll.x;
          vptr^.y := pptr^.y + quad_ll.y;
          vptr^.z := pptr^.z + quad_ll.z;
          Inc(vptr);

          // Lower Right
          vptr^.s := 1.0;
          vptr^.t := 0.0;
          vptr^.rgba := rgba;
          vptr^.x := pptr^.x + quad_lr.x;
          vptr^.y := pptr^.y + quad_lr.y;
          vptr^.z := pptr^.z + quad_lr.z;
          Inc(vptr);

          // Upper Right
          vptr^.s := 1.0;
          vptr^.t := 1.0;
          vptr^.rgba := rgba;
          vptr^.x := pptr^.x - quad_ll.x;
          vptr^.y := pptr^.y - quad_ll.y;
          vptr^.z := pptr^.z - quad_ll.z;
          Inc(vptr);

          // Upper Left
          vptr^.s := 0.0;
          vptr^.t := 1.0;
          vptr^.rgba := rgba;
          vptr^.x := pptr^.x - quad_lr.x;
          vptr^.y := pptr^.y - quad_lr.y;
          vptr^.z := pptr^.z - quad_lr.z;
          Inc(vptr);

          Inc(particle_count);

          if particle_count >= BATCH_PARTICLES then
          begin
            glDrawArrays(GL_QUADS, 0, PARTICLE_VERTS * particle_count);
            particle_count := 0;
            vptr := @vertex_array;
          end;
        end;
        Inc(pptr);
      end;

      // Ultimo batch rimanente
      if particle_count > 0 then
        glDrawArrays(GL_QUADS, 0, PARTICLE_VERTS * particle_count);

      thread_sync.particles_lock.Release;
      // Segnaliamo alla fisica che abbiamo finito di leggere
      thread_sync.d_done := True;

      glDisableClientState(GL_VERTEX_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      glDisableClientState(GL_COLOR_ARRAY);

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glDepthMask(GL_TRUE);
    end;
  end;

const
  FOUNTAIN_SIDE_POINTS = 14;
  FOUNTAIN_SWEEP_STEPS = 32;

const
  fountain_side: array[0..FOUNTAIN_SIDE_POINTS * 2 - 1] of single = (
    1.2, 0.0, 1.0, 0.2, 0.41, 0.3, 0.4, 0.35,
    0.4, 1.95, 0.41, 2.0, 0.8, 2.2, 1.2, 2.4,
    1.5, 2.7, 1.55, 2.95, 1.6, 3.0, 1.0, 3.0,
    0.5, 3.0, 0.0, 3.0
    );

  fountain_normal: array[0..FOUNTAIN_SIDE_POINTS * 2 - 1] of single = (
    1.0000, 0.0000, 0.6428, 0.7660, 0.3420, 0.9397, 1.0000, 0.0000,
    1.0000, 0.0000, 0.3420, -0.9397, 0.4226, -0.9063, 0.5000, -0.8660,
    0.7660, -0.6428, 0.9063, -0.4226, 0.0000, 1.0000, 0.0000, 1.0000,
    0.0000, 1.0000, 0.0000, 1.0000
    );

var
  fountain_list: GLuint = 0;

  procedure draw_fountain;
  var
    angle: double;
    x, y: single;
    m, n: integer;
  begin
    with GetOpenGL do
    begin
      if fountain_list = 0 then
      begin
        fountain_list := glGenLists(1);
        glNewList(fountain_list, GL_COMPILE_AND_EXECUTE);

        glMaterialfv(GL_FRONT, GL_DIFFUSE, @fountain_diffuse);
        glMaterialfv(GL_FRONT, GL_SPECULAR, @fountain_specular);
        glMaterialf(GL_FRONT, GL_SHININESS, fountain_shininess);

        for n := 0 to FOUNTAIN_SIDE_POINTS - 2 do
        begin
          glBegin(GL_TRIANGLE_STRIP);
          for m := 0 to FOUNTAIN_SWEEP_STEPS do
          begin
            angle := m * (2.0 * PI / FOUNTAIN_SWEEP_STEPS);
            x := cos(angle);
            y := sin(angle);

            glNormal3f(x * fountain_normal[n * 2 + 2], y * fountain_normal[n * 2 + 2], fountain_normal[n * 2 + 3]);
            glVertex3f(x * fountain_side[n * 2 + 2], y * fountain_side[n * 2 + 2], fountain_side[n * 2 + 3]);

            glNormal3f(x * fountain_normal[n * 2], y * fountain_normal[n * 2], fountain_normal[n * 2 + 1]);
            glVertex3f(x * fountain_side[n * 2], y * fountain_side[n * 2], fountain_side[n * 2 + 1]);
          end;
          glEnd;
        end;

        glEndList;
      end
      else
        glCallList(fountain_list);

    end;
  end;

  procedure tessellate_floor(x1, y1, x2, y2: single; depth: integer);
  var
    delta, xm, ym: single;
  begin
    with GetOpenGL do
    begin
      if depth >= 5 then
        delta := 999999.0
      else
      begin
        xm := IfThen(Abs(x1) < Abs(x2), Abs(x1), Abs(x2));
        ym := IfThen(Abs(y1) < Abs(y2), Abs(y1), Abs(y2));
        delta := xm * xm + ym * ym;
      end;

      if delta < 0.1 then
      begin
        xm := (x1 + x2) * 0.5;
        ym := (y1 + y2) * 0.5;
        tessellate_floor(x1, y1, xm, ym, depth + 1);
        tessellate_floor(xm, y1, x2, ym, depth + 1);
        tessellate_floor(x1, ym, xm, y2, depth + 1);
        tessellate_floor(xm, ym, x2, y2, depth + 1);
      end
      else
      begin
        glTexCoord2f(x1 * 30.0, y1 * 30.0);
        glVertex3f(x1 * 80.0, y1 * 80.0, 0.0);
        glTexCoord2f(x2 * 30.0, y1 * 30.0);
        glVertex3f(x2 * 80.0, y1 * 80.0, 0.0);
        glTexCoord2f(x2 * 30.0, y2 * 30.0);
        glVertex3f(x2 * 80.0, y2 * 80.0, 0.0);
        glTexCoord2f(x1 * 30.0, y2 * 30.0);
        glVertex3f(x1 * 80.0, y2 * 80.0, 0.0);
      end;
    end;
  end;

var
  floor_list: GLuint = 0;

  procedure draw_floor;
  begin
    with GetOpenGL do
    begin
      if not wireframe then
      begin
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, floor_tex_id);
      end;

      if floor_list = 0 then
      begin
        floor_list := glGenLists(1);
        glNewList(floor_list, GL_COMPILE_AND_EXECUTE);

        glMaterialfv(GL_FRONT, GL_DIFFUSE, @floor_diffuse);
        glMaterialfv(GL_FRONT, GL_SPECULAR, @floor_specular);
        glMaterialf(GL_FRONT, GL_SHININESS, floor_shininess);

        glNormal3f(0.0, 0.0, 1.0);
        glBegin(GL_QUADS);

        tessellate_floor(-1.0, -1.0, 0.0, 0.0, 0);
        tessellate_floor(0.0, -1.0, 1.0, 0.0, 0);
        tessellate_floor(0.0, 0.0, 1.0, 1.0, 0);
        tessellate_floor(-1.0, 0.0, 0.0, 1.0, 0);

        glEnd;
        glEndList;
      end
      else
        glCallList(floor_list);

      glDisable(GL_TEXTURE_2D);
    end;
  end;

  procedure setup_lights;
  const
    l1pos: array[0..3] of single = (0.0, -9.0, 8.0, 1.0);
    l1amb: array[0..3] of single = (0.2, 0.2, 0.2, 1.0);
    l1dif: array[0..3] of single = (0.8, 0.4, 0.2, 1.0);
    l1spec: array[0..3] of single = (1.0, 0.6, 0.2, 0.0);

    l2pos: array[0..3] of single = (-15.0, 12.0, 1.5, 1.0);
    l2amb: array[0..3] of single = (0.0, 0.0, 0.0, 1.0);
    l2dif: array[0..3] of single = (0.2, 0.4, 0.8, 1.0);
    l2spec: array[0..3] of single = (0.2, 0.6, 1.0, 0.0);
  begin
    with GetOpenGL do
    begin
      glLightfv(GL_LIGHT1, GL_POSITION, @l1pos);
      glLightfv(GL_LIGHT1, GL_AMBIENT, @l1amb);
      glLightfv(GL_LIGHT1, GL_DIFFUSE, @l1dif);
      glLightfv(GL_LIGHT1, GL_SPECULAR, @l1spec);

      glLightfv(GL_LIGHT2, GL_POSITION, @l2pos);
      glLightfv(GL_LIGHT2, GL_AMBIENT, @l2amb);
      glLightfv(GL_LIGHT2, GL_DIFFUSE, @l2dif);
      glLightfv(GL_LIGHT2, GL_SPECULAR, @l2spec);

      // Luce dinamica della fontana (aggiornata ogni frame)
      glLightfv(GL_LIGHT3, GL_POSITION, @glow_pos);
      glLightfv(GL_LIGHT3, GL_DIFFUSE, @glow_color);
      glLightfv(GL_LIGHT3, GL_SPECULAR, @glow_color);

      glEnable(GL_LIGHT1);
      glEnable(GL_LIGHT2);
      glEnable(GL_LIGHT3);
    end;
  end;

  procedure draw_scene(window: PGLFWwindow; t: double);
  var
    dt: single;
    projection: TMat4x4;  // oppure array[0..15] of single se non usi LinMath
    xpos, ypos, zpos: single;
    angle_x, angle_y, angle_z: double;
  const
    t_old: double = 0.0;
  begin
    with GetOpenGL do
    begin
      // Calcolo delta time
      dt := single(t - t_old);
      t_old := t;

      // Proiezione prospettica
      Mat4x4_Perspective(projection,
        DegToRad(65.0),
        aspect_ratio,
        1.0,
        60.0);

      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glMatrixMode(GL_PROJECTION);
      glLoadMatrixf(@projection);

      // Reset modelview
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();

      // Rotazione camera (leggermente modificata rispetto all'originale per stabilità)
      angle_x := 90.0 - 10.0;                   // inclinazione verticale fissa
      angle_y := 10.0 * sin(0.3 * t);           // oscillazione orizzontale lenta
      angle_z := 10.0 * t;                      // rotazione continua

      glRotated(-angle_x, 1.0, 0.0, 0.0);
      glRotated(-angle_y, 0.0, 1.0, 0.0);
      glRotated(-angle_z, 0.0, 0.0, 1.0);

      // Posizione camera (orbita attorno alla fontana)
      xpos := 15.0 * sin(DegToRad(angle_z)) + 2.0 * sin(DegToRad(3.1 * t));
      ypos := -15.0 * cos(DegToRad(angle_z)) + 2.0 * cos(DegToRad(2.9 * t));
      zpos := 4.0 + 2.0 * cos(DegToRad(4.9 * t));

      glTranslated(-xpos, -ypos, -zpos);

      // Impostazioni base rendering
      glFrontFace(GL_CCW);
      glCullFace(GL_BACK);
      glEnable(GL_CULL_FACE);

      // Luci
      setup_lights;
      glEnable(GL_LIGHTING);

      // Nebbia (fog)
      glEnable(GL_FOG);
      glFogi(GL_FOG_MODE, GL_EXP);
      glFogf(GL_FOG_DENSITY, 0.05);
      glFogfv(GL_FOG_COLOR, @fog_color);

      // Pavimento
      draw_floor;

      // Abilitazione depth test per oggetti solidi
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LEQUAL);
      glDepthMask(GL_TRUE);

      // Fontana
      draw_fountain;

      // Disattivazione illuminazione e nebbia prima delle particelle
      glDisable(GL_LIGHTING);
      glDisable(GL_FOG);

      // Particelle (sempre per ultime – trasparenti)
      draw_particles(window, t, dt);

      // Non serve più il depth test alla fine
      glDisable(GL_DEPTH_TEST);
    end;
  end;



  procedure resize_callback(window: PGLFWwindow; Width, Height: integer); cdecl;
  begin

    GetOpenGL.glViewport(0, 0, Width, Height);

    // Aggiorna il rapporto di aspetto globale
    if Height > 0 then
      aspect_ratio := Width / Height
    else
      aspect_ratio := 1.0;
  end;

  procedure key_callback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    with GetOpenGL, getGLFW do
    begin
      if action = GLFW_PRESS then
      begin
        case key of
          GLFW_KEY_ESCAPE:
            glfwSetWindowShouldClose(window, GLFW_TRUE);

          GLFW_KEY_W:
          begin
            wireframe := not wireframe;
            if wireframe then
              glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            else
              glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
          end;
        end;
      end;
    end;

  end;

  // Variabili globali necessarie al main
var
  physics_thread: TThreadID;
  window: PGLFWwindow;
  monitor: PGLFWmonitor = nil;
  Width, Height: integer;
  ch: char;

  // Funzione del thread di fisica
  function physics_thread_main(arg: Pointer): PtrInt;
  var
    win: PGLFWwindow absolute arg;
  begin
    with getGLFW, GetOpenGL do
    begin
      while True do
      begin
        thread_sync.particles_lock.Acquire;

        // Attende che il rendering abbia finito di usare i dati precedenti
        while (glfwWindowShouldClose(win) = 0) and (thread_sync.p_frame > thread_sync.d_frame) do
        begin
          Sleep(1);  // semplice polling – in produzione usare condition variables
        end;

        if glfwWindowShouldClose(win) = 1 then
        begin
          thread_sync.particles_lock.Release;
          Break;
        end;

        // Aggiorna fisica
        particle_engine(thread_sync.t, thread_sync.dt);

        // Segnala nuovo frame fisico pronto
        Inc(thread_sync.p_frame);

        thread_sync.particles_lock.Release;
        // Qui potresti usare condition variable per svegliare il rendering
      end;

      Result := 0;
    end;
  end;

begin
  with getGLFW, GetOpenGL do
  begin
    // Inizializzazione GLFW
    if glfwInit() = 0 then
    begin
      WriteLn('Errore inizializzazione GLFW');
      Halt(1);
    end;

    // Gestione argomenti (semplificata – solo -f per fullscreen)
    // In un programma reale useresti ParamStr / GetOpt
    if (ParamCount >= 1) and (ParamStr(1) = '-f') then
      monitor := glfwGetPrimaryMonitor;

    if monitor <> nil then
    begin
      with glfwGetVideoMode(monitor)^ do
      begin
        glfwWindowHint(GLFW_RED_BITS, redBits);
        glfwWindowHint(GLFW_GREEN_BITS, greenBits);
        glfwWindowHint(GLFW_BLUE_BITS, blueBits);
        glfwWindowHint(GLFW_REFRESH_RATE, refreshRate);
        Width := Width;
        Height := Height;
      end;
    end
    else
    begin
      Width := 1024;
      Height := 768;
    end;

    window := glfwCreateWindow(Width, Height, 'GLParticles - Fountain', monitor, nil);
    if window = nil then
    begin
      WriteLn('Errore creazione finestra GLFW');
      glfwTerminate;
      Halt(1);
    end;

    glfwMakeContextCurrent(window);
    // Load estensioni OpenGL (pax.gl dovrebbe già farlo, altrimenti usare glad/glext)
    glfwSwapInterval(1);

    // Callbacks
    glfwSetFramebufferSizeCallback(window, @resize_callback);
    glfwSetKeyCallback(window, @key_callback);

    // Inizializzazione aspect ratio
    glfwGetFramebufferSize(window, @Width, @Height);
    resize_callback(window, Width, Height);

    // Creazione texture particelle
    glGenTextures(1, @particle_tex_id);
    glBindTexture(GL_TEXTURE_2D, particle_tex_id);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, P_TEX_WIDTH, P_TEX_HEIGHT,
      0, GL_LUMINANCE, GL_UNSIGNED_BYTE, @particle_texture);

    // Creazione texture pavimento
    glGenTextures(1, @floor_tex_id);
    glBindTexture(GL_TEXTURE_2D, floor_tex_id);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, F_TEX_WIDTH, F_TEX_HEIGHT,
      0, GL_LUMINANCE, GL_UNSIGNED_BYTE, @floor_texture);

    // Supporto specular color separato (opzionale)
    if glfwExtensionSupported('GL_EXT_separate_specular_color') = 1 then
      glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR);

    // Stato iniziale
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    wireframe := False;

    // Inizializzazione sincronizzazione thread
    thread_sync.t := 0.0;
    thread_sync.dt := 0.001;
    thread_sync.p_frame := 0;
    thread_sync.d_frame := 0;
    thread_sync.p_done := False;
    thread_sync.d_done := False;

    thread_sync.particles_lock.Create();

    // Avvio thread fisica
    BeginThread(@physics_thread_main, window, physics_thread);

    glfwSetTime(0.0);

    // Loop principale
    while glfwWindowShouldClose(window)=0 do
    begin
      draw_scene(window, glfwGetTime());

      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    // Cleanup
    CloseThread(physics_thread);
    glfwDestroyWindow(window);
    glfwTerminate;

    thread_sync.particles_lock.Release;
  end;
end.
