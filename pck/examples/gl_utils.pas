unit gl_utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, pax.gl, typinfo,
  fpImage, fpReadPNG, fpReadJPEG, fpReadBMP;

type
  TShaderType = (
    stVertex,
    stTessControl,
    stTessEvaluation,
    stGeometry,
    stFragment
    );

  { TShaderProgram }

  TShaderProgram = record
  private
    procedure ClearShaders;
  public
    id: GLuint;
    shaders: array[TShaderType] of GLuint;
    log: string;

    procedure Init;
    procedure Clear;

    function CompileShader(const ASource: string; AType: TShaderType): GLuint;
    function CompileTessellationControl(const ASource: string): GLuint;
    function CompileTessellationEvaluation(const ASource: string): GLuint;
    function CompileVertex(const ASource: string): GLuint;
    function CompileFragment(const ASource: string): GLuint;
    function CompileGeometry(const ASource: string): GLuint;

    function LinkProgram: GLuint;

    procedure Use; inline;
    procedure Unuse; inline;

    function IsValid: boolean; inline;
    function LinkStatus: GLint;
    function GetProgramiv(pname: GLenum): GLint;
    function GetShaderiv(shader: GLuint; pname: GLenum): GLint;

    function GetCompileStatus(shader: GLuint): GLint;
  end;


  { TTexture }

  TTexture = record
  public
    ID: GLuint;
    Width, Height: integer;
    InternalFormat: GLenum;
    Format: GLenum;
    DataType: GLenum;
    Mipmaps: boolean;
    log: string;

    procedure Init;
    procedure Clear;

    function LoadFromFile(const FileName: string; GenerateMipmaps: boolean = True): boolean;
    procedure LoadFromMemory(const aData: Pointer; W, H: integer; aFormat: GLenum = GL_RGBA; aDataType: GLenum = GL_UNSIGNED_BYTE; aInternalFormat: GLenum = GL_RGBA8; GenerateMipmaps: boolean = False);
    procedure Bind(aUnit: GLuint = 0);
    procedure Unbind(aUnit: GLuint = 0);

    procedure SetFilter(MinFilter, MagFilter: GLenum);
    procedure SetWrap(S, T: GLenum);

    function IsValid: boolean; inline;
    procedure CreateCheckerboard(Size: integer = 64; SquareSize: integer = 8);
    procedure DestroyTexture;
  end;

implementation

{ TShaderProgram }

procedure TShaderProgram.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
  log := '';
end;

procedure TShaderProgram.Clear;
begin
  ClearShaders;

  if id <> 0 then
  begin
    GetOpenGL.glDeleteProgram(id);
    id := 0;
  end;

  log := '';
end;

procedure TShaderProgram.ClearShaders;
var
  t: TShaderType;
begin
  for t in TShaderType do
    if shaders[t] <> 0 then
    begin
      GetOpenGL.glDeleteShader(shaders[t]);
      shaders[t] := 0;
    end;
end;

function TShaderProgram.CompileShader(const ASource: string; AType: TShaderType): GLuint;
const
  GL_SHADER_TYPES: array[TShaderType] of GLenum = (
    GL_VERTEX_SHADER,
    GL_TESS_CONTROL_SHADER,
    GL_TESS_EVALUATION_SHADER,
    GL_GEOMETRY_SHADER,
    GL_FRAGMENT_SHADER
    );
var
  SrcPtr: PGLchar;
  Success: GLint;
  InfoLog: string = '';
  Len: GLint;
begin
  with GetOpenGL do
  begin
    Result := 0;

    if ASource = '' then
    begin
      log := Format('Cannot compile empty %s shader', [GetEnumName(TypeInfo(TShaderType), Ord(AType))]);
      Exit;
    end;

    Result := glCreateShader(GL_SHADER_TYPES[AType]);
    if Result = 0 then
    begin
      log := 'glCreateShader failed';
      Exit;
    end;

    SrcPtr := PGLchar(ASource);
    glShaderSource(Result, 1, @SrcPtr, nil);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @Success);
    if Success = GL_FALSE then
    begin
      glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @Len);
      if Len > 0 then
      begin
        SetLength(InfoLog, Len);
        glGetShaderInfoLog(Result, Len, nil, PGLchar(InfoLog));
        InfoLog := Trim(InfoLog);
      end;
      log := Format('Shader compile failed (%s):%s%s', [GetEnumName(TypeInfo(TShaderType), Ord(AType)), LineEnding, InfoLog]);
      glDeleteShader(Result);
      Result := 0;
      Exit;
    end;

    shaders[AType] := Result;
    log := '';
  end;
end;

function TShaderProgram.CompileTessellationControl(const ASource: string): GLuint;
begin
  Result := CompileShader(ASource, stTessControl);
end;

function TShaderProgram.CompileTessellationEvaluation(const ASource: string): GLuint;
begin
  Result := CompileShader(ASource, stTessEvaluation);
end;

function TShaderProgram.CompileVertex(const ASource: string): GLuint;
begin
  Result := CompileShader(ASource, stVertex);
end;

function TShaderProgram.CompileFragment(const ASource: string): GLuint;
begin
  Result := CompileShader(ASource, stFragment);
end;

function TShaderProgram.CompileGeometry(const ASource: string): GLuint;
begin
  Result := CompileShader(ASource, stGeometry);
end;

function TShaderProgram.LinkProgram: GLuint;
var
  Success: GLint;
  InfoLog: string = '';
  Len: GLint;
begin
  with GetOpenGL do
  begin
    Result := 0;
    log := '';
    //ClearShaders;
    Result := glCreateProgram();
    if Result = 0 then
    begin
      log := 'glCreateProgram failed';
      Exit;
    end;

    if shaders[stVertex] <> 0 then glAttachShader(Result, shaders[stVertex]);
    if shaders[stFragment] <> 0 then glAttachShader(Result, shaders[stFragment]);
    if shaders[stGeometry] <> 0 then glAttachShader(Result, shaders[stGeometry]);
    if shaders[stTessControl] <> 0 then glAttachShader(Result, shaders[stTessControl]);
    if shaders[stTessEvaluation] <> 0 then glAttachShader(Result, shaders[stTessEvaluation]);

    glLinkProgram(Result);

    glGetProgramiv(Result, GL_LINK_STATUS, @Success);

    if Success = GL_FALSE then
    begin
      glGetProgramiv(Result, GL_INFO_LOG_LENGTH, @Len);
      if Len > 0 then
      begin
        SetLength(InfoLog, Len);
        glGetProgramInfoLog(Result, Len, nil, PGLchar(InfoLog));
        InfoLog := Trim(InfoLog);
      end;
      log := Format('Program link failed:%s%s', [LineEnding, InfoLog]);
      glDeleteProgram(Result);
      Result := 0;
    end;

    id := Result;
  end;
end;

procedure TShaderProgram.Use;
begin
  GetOpenGL.glUseProgram(id);
end;

procedure TShaderProgram.Unuse;
begin
  GetOpenGL.glUseProgram(0);
end;

function TShaderProgram.IsValid: boolean;
begin
  Result := (id <> 0) and (LinkStatus = GL_TRUE);
end;

function TShaderProgram.LinkStatus: GLint;
begin
  Result := GetProgramiv(GL_LINK_STATUS);
end;

function TShaderProgram.GetProgramiv(pname: GLenum): GLint;
begin
  Result := 0;
  if id = 0 then Exit;
  GetOpenGL.glGetProgramiv(id, pname, @Result);
end;

function TShaderProgram.GetShaderiv(shader: GLuint; pname: GLenum): GLint;
begin
  Result := 0;
  if shader = 0 then Exit;
  GetOpenGL.glGetShaderiv(shader, pname, @Result);
end;

function TShaderProgram.GetCompileStatus(shader: GLuint): GLint;
begin
  Result := GetShaderiv(shader, GL_COMPILE_STATUS);
end;


procedure TTexture.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
  ID := 0;
end;

procedure TTexture.DestroyTexture;
begin
  if ID <> 0 then
  begin
    GetOpenGL.glDeleteTextures(1, @ID);
    ID := 0;
  end;
end;

procedure TTexture.Clear;
begin
  DestroyTexture;
  Width := 0;
  Height := 0;
  Mipmaps := False;
end;


procedure TTexture.LoadFromMemory(const aData: Pointer; W, H: integer; aFormat: GLenum = GL_RGBA; aDataType: GLenum = GL_UNSIGNED_BYTE; aInternalFormat: GLenum = GL_RGBA8; GenerateMipmaps: boolean = False);
begin
  Clear;  // resetta eventuali texture precedenti

  if (aData = nil) or (W <= 0) or (H <= 0) then
  begin
    log := 'Dati non validi per LoadFromMemory';
    Exit;
  end;

  Width := W;
  Height := H;
  Format := aFormat;
  DataType := aDataType;
  InternalFormat := aInternalFormat;

  with GetOpenGL do
  begin
    if ID = 0 then
      glGenTextures(1, @ID);

    if ID = 0 then
    begin
      log := 'glGenTextures fallito in LoadFromMemory';
      Exit;
    end;

    glBindTexture(GL_TEXTURE_2D, ID);

    glTexImage2D(GL_TEXTURE_2D, 0, InternalFormat,
      Width, Height, 0,
      Format, DataType, aData);

    // Parametri di default ragionevoli
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    if GenerateMipmaps then
    begin
      glGenerateMipmap(GL_TEXTURE_2D);
      Mipmaps := True;
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    end;

    glBindTexture(GL_TEXTURE_2D, 0);  // buona pratica
  end;
end;

function TTexture.LoadFromFile(const FileName: string; GenerateMipmaps: boolean = True): boolean;
var
  Reader: TFPCustomImageReader;
  Img: TFPMemoryImage;          // ← Usiamo TFPMemoryImage per accesso diretto
  Mem: TMemoryStream;
  x, y, PixelIdx: integer;
  Pixels: array of byte;
  Col: TFPColor;
begin
  Result := False;
  Clear;

  if not FileExists(FileName) then
  begin
    log := 'File non trovato: ' + FileName;
    Exit;
  end;

  // Creiamo l'immagine in memoria
  Img := TFPMemoryImage.Create(0, 0);
  try
    // Scegliamo il reader in base all'estensione
    if Pos('.png', LowerCase(FileName)) > 0 then
      Reader := TFPReaderPNG.Create
    else if Pos('.jpg', LowerCase(FileName)) > 0 then
      Reader := TFPReaderJPEG.Create
    else if Pos('.bmp', LowerCase(FileName)) > 0 then
      Reader := TFPReaderBMP.Create
    else
    begin
      log := 'Formato immagine non supportato: ' + FileName;
      Exit;
    end;

    try
      Mem := TMemoryStream.Create;
      try
        Mem.LoadFromFile(FileName);
        Mem.Position := 0;
        Img.LoadFromStream(Mem, Reader);
      finally
        Mem.Free;
      end;

      Width := Img.Width;
      Height := Img.Height;

      if (Width <= 0) or (Height <= 0) then
      begin
        log := 'Immagine vuota o non valida: ' + FileName;
        Exit;
      end;

      // Prepariamo array RGBA per OpenGL (8 bit per canale)
      SetLength(Pixels, Width * Height * 4);
      PixelIdx := 0;

      // Accesso ai pixel tramite Colors[x,y]
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          Col := Img.Colors[x, y];

          Pixels[PixelIdx + 0] := Col.red shr 8;   // R (da 0..65535 → 0..255)
          Pixels[PixelIdx + 1] := Col.green shr 8;   // G
          Pixels[PixelIdx + 2] := Col.blue shr 8;   // B
          Pixels[PixelIdx + 3] := Col.alpha shr 8;   // A (se non presente, alpha=65535 → 255)

          Inc(PixelIdx, 4);
        end;

      // Caricamento in OpenGL
      with GetOpenGL do
      begin
        glGenTextures(1, @ID);
        if ID = 0 then
        begin
          log := 'glGenTextures fallito';
          Exit;
        end;

        glBindTexture(GL_TEXTURE_2D, ID);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,
          Width, Height, 0,
          GL_RGBA, GL_UNSIGNED_BYTE, @Pixels[0]);

        // Parametri default ragionevoli
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        if GenerateMipmaps then
        begin
          glGenerateMipmap(GL_TEXTURE_2D);
          Mipmaps := True;
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        end;

        glBindTexture(GL_TEXTURE_2D, 0);
      end;

      Result := True;
      log := SysUtils.Format('Texture caricata da file: %dx%d', [Width, Height]);
    finally
      Reader.Free;
    end;
  finally
    Img.Free;
  end;
end;

procedure TTexture.CreateCheckerboard(Size: integer = 64; SquareSize: integer = 8);
const
  BLACK: array[0..3] of byte = (40, 40, 40, 255);
  WHITE: array[0..3] of byte = (220, 220, 220, 255);
var
  Data: array of byte;
  x, y, i: integer;
  color: pbyte;
begin
  Width := Size;
  Height := Size;

  SetLength(Data, Size * Size * 4);

  i := 0;
  for y := 0 to Size - 1 do
  begin
    for x := 0 to Size - 1 do
    begin
      if ((x div SquareSize) + (y div SquareSize)) mod 2 = 0 then
        color := @WHITE[0]
      else
        color := @BLACK[0];

      Data[i + 0] := color[0];  // R
      Data[i + 1] := color[1];  // G
      Data[i + 2] := color[2];  // B
      Data[i + 3] := color[3];  // A
      Inc(i, 4);
    end;
  end;

  LoadFromMemory(@Data[0], Size, Size, GL_RGBA, GL_UNSIGNED_BYTE, GL_RGBA8, True);
end;

procedure TTexture.Bind(aUnit: GLuint);
begin
  if ID = 0 then Exit;
  with GetOpenGL do
  begin
    glActiveTexture(GL_TEXTURE0 + aUnit);
    glBindTexture(GL_TEXTURE_2D, ID);
  end;
end;

procedure TTexture.Unbind(aUnit: GLuint);
begin
  with GetOpenGL do
  begin
    glActiveTexture(GL_TEXTURE0 + aUnit);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

procedure TTexture.SetFilter(MinFilter, MagFilter: GLenum);
begin
  if ID = 0 then Exit;
  with GetOpenGL do
  begin
    glBindTexture(GL_TEXTURE_2D, ID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, MagFilter);
  end;
end;

procedure TTexture.SetWrap(S, T: GLenum);
begin
  if ID = 0 then Exit;
  with GetOpenGL do
  begin
    glBindTexture(GL_TEXTURE_2D, ID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, S);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, T);
  end;
end;

function TTexture.IsValid: boolean;
begin
  Result := (ID <> 0) and (Width > 0) and (Height > 0);
end;

end.
