program WindowsDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  pax.glfw,
  pax.gl, pax.vulkan.video;

const
  MAX_WINDOWS = 4;

type
  TWindowInfo = record
    window: PGLFWwindow;
    title: string;
    color: array[0..2] of single;  // Colore di sfondo per questa finestra
    angle: single;                 // Rotazione personale
  end;

var
  Windows: array[0..MAX_WINDOWS - 1] of TWindowInfo;
  window_count: integer = 0;
  Width, Height: integer;
  // ============================================================================
  // Callback di errore globale
  // ============================================================================
  procedure ErrorCallback(error: integer; const description: pchar); cdecl;
  begin
    WriteLn(ErrOutput, 'Errore GLFW ', error, ': ', description);
  end;

  // ============================================================================
  // Callback tasto per finestra
  // ============================================================================
  procedure KeyCallback(window: PGLFWwindow; key, scancode, action, mods: integer); cdecl;
  begin
    if action = GLFW_PRESS then
    begin
      if key = GLFW_KEY_ESCAPE then
        getGLFW.glfwSetWindowShouldClose(window, True)
      else if key = GLFW_KEY_N then
      begin
        // Crea nuova finestra
        if window_count < MAX_WINDOWS then
        begin
          Windows[window_count].title := 'Finestra ' + IntToStr(window_count + 1);
          Windows[window_count].color[0] := Random;
          Windows[window_count].color[1] := Random;
          Windows[window_count].color[2] := Random;
          Windows[window_count].angle := 0.0;

          Windows[window_count].window := getGLFW.glfwCreateWindow(300, 300, PChar(Windows[window_count].title), nil, Windows[0].window  // Condivide contesto con la prima finestra
            );

          if Windows[window_count].window = nil then
          begin
            WriteLn('Errore creazione finestra aggiuntiva');
            Exit;
          end;

          getGLFW.glfwSetKeyCallback(Windows[window_count].window, @KeyCallback);
          Inc(window_count);
          WriteLn('Creata ', Windows[window_count - 1].title);
        end;
      end;
    end;
  end;

  // ============================================================================
  // Disegna contenuto per una singola finestra
  // ============================================================================
  procedure DrawWindow(win: PGLFWwindow; t: double);
  var
    i: integer;
    angle: single;
  begin
    with getOpenGL do
    begin
      // Trova l'indice della finestra
      for i := 0 to window_count - 1 do
        if Windows[i].window = win then
        begin
          angle := Windows[i].angle;

          glClearColor(Windows[i].color[0], Windows[i].color[1], Windows[i].color[2], 1.0);
          glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

          glMatrixMode(GL_MODELVIEW);
          glLoadIdentity;

          glRotatef(angle, 0.0, 0.0, 1.0);

          // Triangolo rotante
          glBegin(GL_TRIANGLES);
          glColor3f(1.0, 0.0, 0.0);
          glVertex2f(0.0, 0.8);
          glColor3f(0.0, 1.0, 0.0);
          glVertex2f(-0.8, -0.8);
          glColor3f(0.0, 0.0, 1.0);
          glVertex2f(0.8, -0.8);
          glEnd;

          // Aggiorna angolo per prossima volta
          Windows[i].angle := angle + 30.0 * single(t) * 0.1;
          Break;
        end;
    end;
  end;

  // ============================================================================
  // Callback ridimensionamento (viewport)
  // ============================================================================
  procedure Reshape(window: PGLFWwindow; w, h: integer); cdecl;
  begin
    with getOpenGL do
    begin
      glViewport(0, 0, w, h);
    end;
  end;

  // ============================================================================
  // Main
  // ============================================================================
var
  i: integer;
  t: double;
  all_closed: boolean = True;
begin
  with getGLFW do
  begin
    glfwSetErrorCallback(@ErrorCallback);

    if not glfwInit then
    begin
      WriteLn(ErrOutput, 'Errore inizializzazione GLFW');
      Halt(1);
    end;

    // Prima finestra (principale)
    Windows[0].title := 'Finestra Principale';
    Windows[0].color[0] := 0.2;
    Windows[0].color[1] := 0.3;
    Windows[0].color[2] := 0.4;
    Windows[0].angle := 0.0;

    Windows[0].window := glfwCreateWindow(400, 400, PChar(Windows[0].title), nil, nil);
    if Windows[0].window = nil then
    begin
      glfwTerminate;
      Halt(1);
    end;

    glfwMakeContextCurrent(Windows[0].window);
    glfwSwapInterval(1);

    glfwSetFramebufferSizeCallback(Windows[0].window, @Reshape);
    glfwSetKeyCallback(Windows[0].window, @KeyCallback);

    glfwGetFramebufferSize(Windows[0].window, Width, Height);
    Reshape(Windows[0].window, Width, Height);

    // Inizializza OpenGL
    with getOpenGL do
    begin
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_CULL_FACE);
    end;

    window_count := 1;

    WriteLn('Premi N per creare una nuova finestra');
    WriteLn('Premi ESC in una finestra per chiuderla');
    WriteLn('Premi SPAZIO per pausa/riprendi');

    while True do
    begin
      t := glfwGetTime();

      // Aggiorna e disegna tutte le finestre aperte
      for i := 0 to window_count - 1 do
      begin
        if not glfwWindowShouldClose(Windows[i].window) then
        begin
          glfwMakeContextCurrent(Windows[i].window);
          DrawWindow(Windows[i].window, t);
          glfwSwapBuffers(Windows[i].window);
        end;
      end;

      glfwPollEvents;

      // Controlla se tutte le finestre sono chiuse

      for i := 0 to window_count - 1 do
        if not glfwWindowShouldClose(Windows[i].window) then
          all_closed := False;

      if all_closed then Break;
    end;

    // Cleanup
    for i := 0 to window_count - 1 do
      if Windows[i].window <> nil then
        glfwDestroyWindow(Windows[i].window);

    glfwTerminate;
  end;
end.
