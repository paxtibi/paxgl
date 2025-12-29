unit LinMath;

interface

uses
  Math;

const
  M_PI: double = 3.14159265358979323846;

type
  TSingleArray = array[0..MaxInt div SizeOf(single) - 1] of single;
  PSingleArray = ^TSingleArray;

  TVec2 = array[0..1] of single;
  TVec3 = array[0..2] of single;
  TVec4 = array[0..3] of single;
  TMat4x4 = array[0..3] of TVec4;
  TQuat = TVec4;
  PVec3 = ^TVec3;

procedure Vec2_Add(out r: TVec2; const a, b: TVec2); inline;
procedure Vec2_Sub(out r: TVec2; const a, b: TVec2); inline;
procedure Vec2_Scale(out r: TVec2; const v: TVec2; s: single); inline;
function Vec2_Mul_Inner(const a, b: TVec2): single; inline;
function Vec2_Len(const v: TVec2): single; inline;
procedure Vec2_Norm(out r: TVec2; const v: TVec2); inline;
procedure Vec2_Min(out r: TVec2; const a, b: TVec2); inline;
procedure Vec2_Max(out r: TVec2; const a, b: TVec2); inline;
procedure Vec2_Dup(out r: TVec2; const src: TVec2); inline;

procedure Vec3_Add(out r: TVec3; const a, b: TVec3); inline;
procedure Vec3_Sub(out r: TVec3; const a, b: TVec3); inline;
procedure Vec3_Scale(out r: TVec3; const v: TVec3; s: single); inline;
function Vec3_Mul_Inner(const a, b: TVec3): single; inline;
function Vec3_Len(const v: TVec3): single; inline;
procedure Vec3_Norm(out r: TVec3; const v: TVec3); inline;
procedure Vec3_Min(out r: TVec3; const a, b: TVec3); inline;
procedure Vec3_Max(out r: TVec3; const a, b: TVec3); inline;
procedure Vec3_Dup(out r: TVec3; const src: TVec3); inline;

procedure Vec3_Mul_Cross(out r: TVec3; const a, b: TVec3); inline;
procedure Vec3_Reflect(out r: TVec3; const v, n: TVec3); inline;

procedure Vec4_Add(out r: TVec4; const a, b: TVec4); inline;
procedure Vec4_Sub(out r: TVec4; const a, b: TVec4); inline;
procedure Vec4_Scale(out r: TVec4; const v: TVec4; s: single); inline;
function Vec4_Mul_Inner(const a, b: TVec4): single; inline;
function Vec4_Len(const v: TVec4): single; inline;
procedure Vec4_Norm(out r: TVec4; const v: TVec4); inline;
procedure Vec4_Min(out r: TVec4; const a, b: TVec4); inline;
procedure Vec4_Max(out r: TVec4; const a, b: TVec4); inline;
procedure Vec4_Dup(out r: TVec4; const src: TVec4); inline;

procedure Vec4_Mul_Cross(out r: TVec4; const a, b: TVec4); inline;
procedure Vec4_Reflect(out r: TVec4; const v, n: TVec4); inline;

procedure Mat4x4_Identity(out M: TMat4x4); inline;
procedure Mat4x4_Dup(out M: TMat4x4; const N: TMat4x4); inline;
procedure Mat4x4_Row(out r: TVec4; const M: TMat4x4; i: integer); inline;
procedure Mat4x4_Col(out r: TVec4; const M: TMat4x4; i: integer); inline;
procedure Mat4x4_Transpose(out M: TMat4x4; const N: TMat4x4);
procedure Mat4x4_Add(out M: TMat4x4; const a, b: TMat4x4); inline;
procedure Mat4x4_Sub(out M: TMat4x4; const a, b: TMat4x4); inline;
procedure Mat4x4_Scale(out M: TMat4x4; const a: TMat4x4; k: single); inline;
procedure Mat4x4_Scale_Aniso(out M: TMat4x4; const a: TMat4x4; x, y, z: single); inline;
procedure Mat4x4_Mul(out M: TMat4x4; const a, b: TMat4x4);
procedure Mat4x4_Mul_Vec4(out r: TVec4; const M: TMat4x4; const v: TVec4); inline;
procedure Mat4x4_Translate(out T: TMat4x4; x, y, z: single); inline;
procedure Mat4x4_Translate_In_Place(var M: TMat4x4; x, y, z: single);
procedure Mat4x4_From_Vec3_Mul_Outer(out M: TMat4x4; const a, b: TVec3);
procedure Mat4x4_Rotate(out R: TMat4x4; const M: TMat4x4; x, y, z, angle: single);
procedure Mat4x4_Rotate_X(out Q: TMat4x4; const M: TMat4x4; angle: single);
procedure Mat4x4_Rotate_Y(out Q: TMat4x4; const M: TMat4x4; angle: single);
procedure Mat4x4_Rotate_Z(out Q: TMat4x4; const M: TMat4x4; angle: single);
procedure Mat4x4_Invert(out T: TMat4x4; const M: TMat4x4);
procedure Mat4x4_Orthonormalize(out R: TMat4x4; const M: TMat4x4);
procedure Mat4x4_Frustum(out M: TMat4x4; l, r, b, t, n, f: single);
procedure Mat4x4_Ortho(out M: TMat4x4; l, r, b, t, n, f: single);
procedure Mat4x4_Perspective(out m: TMat4x4; y_fov, aspect, n, f: single);
procedure Mat4x4_Look_At(out m: TMat4x4; const eye, center, up: TVec3);

procedure Quat_Identity(out q: TQuat); inline;
procedure Quat_Mul(out r: TQuat; const p, q: TQuat);
procedure Quat_Conj(out r: TQuat; const q: TQuat); inline;
procedure Quat_Rotate(out r: TQuat; angle: single; const axis: TVec3);
procedure Quat_Mul_Vec3(out r: TVec3; const q: TQuat; const v: TVec3);
procedure Mat4x4_From_Quat(out M: TMat4x4; const q: TQuat);
procedure Mat4x4o_Mul_Quat(out R: TMat4x4; const M: TMat4x4; const q: TQuat);
procedure Quat_From_Mat4x4(out q: TQuat; const M: TMat4x4);
procedure Mat4x4_Arcball(out R: TMat4x4; const M: TMat4x4; const _a, _b: TVec2; s: single);

implementation

// Helper generici

procedure Vec_Add_Generic(out r; const a, b; n: integer); inline;
var
  i: integer;
begin
  for i := 0 to n - 1 do
    PSingleArray(@r)^[i] := PSingleArray(@a)^[i] + PSingleArray(@b)^[i];
end;

procedure Vec_Sub_Generic(out r; const a, b; n: integer); inline;
var
  i: integer;
begin
  for i := 0 to n - 1 do
    PSingleArray(@r)^[i] := PSingleArray(@a)^[i] - PSingleArray(@b)^[i];
end;

procedure Vec_Scale_Generic(out r; const v; s: single; n: integer); inline;
var
  i: integer;
begin
  for i := 0 to n - 1 do
    PSingleArray(@r)^[i] := PSingleArray(@v)^[i] * s;
end;

function Vec_Mul_Inner_Generic(const a, b; n: integer): single; inline;
var
  i: integer;
  p: single;
begin
  p := 0.0;
  for i := 0 to n - 1 do
    p += PSingleArray(@a)^[i] * PSingleArray(@b)^[i];
  Result := p;
end;

function Vec_Len_Generic(const v; n: integer): single; inline;
begin
  Result := Sqrt(Vec_Mul_Inner_Generic(v, v, n));
end;

procedure Vec_Norm_Generic(out r; const v; n: integer); inline;
var
  len: single;
begin
  len := Vec_Len_Generic(v, n);
  if len > 1e-8 then
    Vec_Scale_Generic(r, v, 1.0 / len, n)
  else
    FillChar(r, n * SizeOf(single), 0);
end;

procedure Vec_Min_Generic(out r; const a, b; n: integer); inline;
var
  i: integer;
begin
  for i := 0 to n - 1 do
    if PSingleArray(@a)^[i] < PSingleArray(@b)^[i] then
      PSingleArray(@r)^[i] := PSingleArray(@a)^[i]
    else
      PSingleArray(@r)^[i] := PSingleArray(@b)^[i];
end;

procedure Vec_Max_Generic(out r; const a, b; n: integer); inline;
var
  i: integer;
begin
  for i := 0 to n - 1 do
    if PSingleArray(@a)^[i] > PSingleArray(@b)^[i] then
      PSingleArray(@r)^[i] := PSingleArray(@a)^[i]
    else
      PSingleArray(@r)^[i] := PSingleArray(@b)^[i];
end;

procedure Vec_Dup_Generic(out r; const src; n: integer); inline;
begin
  Move(src, r, n * SizeOf(single));
end;

// Vec2 - rimossi parametri inutili nelle inline
procedure Vec2_Add(out r: TVec2; const a, b: TVec2); inline;
begin
  Vec_Add_Generic(r, a, b, 2);
end;

procedure Vec2_Sub(out r: TVec2; const a, b: TVec2); inline;
begin
  Vec_Sub_Generic(r, a, b, 2);
end;

procedure Vec2_Scale(out r: TVec2; const v: TVec2; s: single); inline;
begin
  Vec_Scale_Generic(r, v, s, 2);
end;

function Vec2_Mul_Inner(const a, b: TVec2): single; inline;
begin
  Result := Vec_Mul_Inner_Generic(a, b, 2);
end;

function Vec2_Len(const v: TVec2): single; inline;
begin
  Result := Vec_Len_Generic(v, 2);
end;

procedure Vec2_Norm(out r: TVec2; const v: TVec2); inline;
begin
  Vec_Norm_Generic(r, v, 2);
end;

procedure Vec2_Min(out r: TVec2; const a, b: TVec2); inline;
begin
  Vec_Min_Generic(r, a, b, 2);
end;

procedure Vec2_Max(out r: TVec2; const a, b: TVec2); inline;
begin
  Vec_Max_Generic(r, a, b, 2);
end;

procedure Vec2_Dup(out r: TVec2; const src: TVec2); inline;
begin
  Vec_Dup_Generic(r, src, 2);
end;

// Vec3
procedure Vec3_Add(out r: TVec3; const a, b: TVec3); inline;
begin
  Vec_Add_Generic(r, a, b, 3);
end;

procedure Vec3_Sub(out r: TVec3; const a, b: TVec3); inline;
begin
  Vec_Sub_Generic(r, a, b, 3);
end;

procedure Vec3_Scale(out r: TVec3; const v: TVec3; s: single); inline;
begin
  Vec_Scale_Generic(r, v, s, 3);
end;

function Vec3_Mul_Inner(const a, b: TVec3): single; inline;
begin
  Result := Vec_Mul_Inner_Generic(a, b, 3);
end;

function Vec3_Len(const v: TVec3): single; inline;
begin
  Result := Vec_Len_Generic(v, 3);
end;

procedure Vec3_Norm(out r: TVec3; const v: TVec3); inline;
begin
  Vec_Norm_Generic(r, v, 3);
end;

procedure Vec3_Min(out r: TVec3; const a, b: TVec3); inline;
begin
  Vec_Min_Generic(r, a, b, 3);
end;

procedure Vec3_Max(out r: TVec3; const a, b: TVec3); inline;
begin
  Vec_Max_Generic(r, a, b, 3);
end;

procedure Vec3_Dup(out r: TVec3; const src: TVec3); inline;
begin
  Vec_Dup_Generic(r, src, 3);
end;

procedure Vec3_Mul_Cross(out r: TVec3; const a, b: TVec3); inline;
begin
  r[0] := a[1] * b[2] - a[2] * b[1];
  r[1] := a[2] * b[0] - a[0] * b[2];
  r[2] := a[0] * b[1] - a[1] * b[0];
end;

procedure Vec3_Reflect(out r: TVec3; const v, n: TVec3); inline;
var
  p: single;
begin
  p := 2.0 * Vec3_Mul_Inner(v, n);
  r[0] := v[0] - p * n[0];
  r[1] := v[1] - p * n[1];
  r[2] := v[2] - p * n[2];
end;

// Vec4
procedure Vec4_Add(out r: TVec4; const a, b: TVec4); inline;
begin
  Vec_Add_Generic(r, a, b, 4);
end;

procedure Vec4_Sub(out r: TVec4; const a, b: TVec4); inline;
begin
  Vec_Sub_Generic(r, a, b, 4);
end;

procedure Vec4_Scale(out r: TVec4; const v: TVec4; s: single); inline;
begin
  Vec_Scale_Generic(r, v, s, 4);
end;

function Vec4_Mul_Inner(const a, b: TVec4): single; inline;
begin
  Result := Vec_Mul_Inner_Generic(a, b, 4);
end;

function Vec4_Len(const v: TVec4): single; inline;
begin
  Result := Vec_Len_Generic(v, 4);
end;

procedure Vec4_Norm(out r: TVec4; const v: TVec4); inline;
begin
  Vec_Norm_Generic(r, v, 4);
end;

procedure Vec4_Min(out r: TVec4; const a, b: TVec4); inline;
begin
  Vec_Min_Generic(r, a, b, 4);
end;

procedure Vec4_Max(out r: TVec4; const a, b: TVec4); inline;
begin
  Vec_Max_Generic(r, a, b, 4);
end;

procedure Vec4_Dup(out r: TVec4; const src: TVec4); inline;
begin
  Vec_Dup_Generic(r, src, 4);
end;

procedure Vec4_Mul_Cross(out r: TVec4; const a, b: TVec4); inline;
begin
  r[0] := a[1] * b[2] - a[2] * b[1];
  r[1] := a[2] * b[0] - a[0] * b[2];
  r[2] := a[0] * b[1] - a[1] * b[0];
  r[3] := 1.0;
end;

procedure Vec4_Reflect(out r: TVec4; const v, n: TVec4); inline;
var
  p: single;
begin
  p := 2.0 * Vec4_Mul_Inner(v, n);
  r[0] := v[0] - p * n[0];
  r[1] := v[1] - p * n[1];
  r[2] := v[2] - p * n[2];
  r[3] := v[3] - p * n[3];
end;

// Matrici

procedure Mat4x4_Identity(out M: TMat4x4); inline;
var
  i: integer;
begin
  FillChar(M, SizeOf(M), 0);
  for i := 0 to 3 do M[i][i] := 1.0;
end;

procedure Mat4x4_Dup(out M: TMat4x4; const N: TMat4x4); inline;
begin
  Move(N, M, SizeOf(TMat4x4));
end;

procedure Mat4x4_Row(out r: TVec4; const M: TMat4x4; i: integer); inline;
var
  k: integer;
begin
  for k := 0 to 3 do r[k] := M[k][i];
end;

procedure Mat4x4_Col(out r: TVec4; const M: TMat4x4; i: integer); inline;
begin
  r := M[i];
end;

procedure Mat4x4_Transpose(out M: TMat4x4; const N: TMat4x4);
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      M[i][j] := N[j][i];
end;

procedure Mat4x4_Add(out M: TMat4x4; const a, b: TMat4x4); inline;
var
  i: integer;
begin
  for i := 0 to 3 do Vec4_Add(M[i], a[i], b[i]);
end;

procedure Mat4x4_Sub(out M: TMat4x4; const a, b: TMat4x4); inline;
var
  i: integer;
begin
  for i := 0 to 3 do Vec4_Sub(M[i], a[i], b[i]);
end;

procedure Mat4x4_Scale(out M: TMat4x4; const a: TMat4x4; k: single); inline;
var
  i: integer;
begin
  for i := 0 to 3 do Vec4_Scale(M[i], a[i], k);
end;

procedure Mat4x4_Scale_Aniso(out M: TMat4x4; const a: TMat4x4; x, y, z: single); inline;
begin
  Vec4_Scale(M[0], a[0], x);
  Vec4_Scale(M[1], a[1], y);
  Vec4_Scale(M[2], a[2], z);
  Vec4_Dup(M[3], a[3]);
end;

procedure Mat4x4_Mul(out M: TMat4x4; const a, b: TMat4x4);
var
  temp: TMat4x4;
  c, r, k: integer;
begin
  FillChar(temp, SizeOf(temp), 0);
  for c := 0 to 3 do
    for r := 0 to 3 do
      for k := 0 to 3 do
        temp[c][r] += a[k][r] * b[c][k];
  Mat4x4_Dup(M, temp);
end;

procedure Mat4x4_Mul_Vec4(out r: TVec4; const M: TMat4x4; const v: TVec4); inline;
var
  i, j: integer;
begin
  FillChar(r, SizeOf(r), 0);
  for j := 0 to 3 do
    for i := 0 to 3 do
      r[j] += M[i][j] * v[i];
end;

procedure Mat4x4_Translate(out T: TMat4x4; x, y, z: single); inline;
begin
  Mat4x4_Identity(T);
  T[3][0] := x;
  T[3][1] := y;
  T[3][2] := z;
end;

procedure Mat4x4_Translate_In_Place(var M: TMat4x4; x, y, z: single);
var
  t, row: TVec4;
  i: integer;
begin
  t := [x, y, z, 0.0];
  for i := 0 to 3 do
  begin
    Mat4x4_Row(row, M, i);
    M[3][i] += Vec4_Mul_Inner(row, t);
  end;
end;

procedure Mat4x4_From_Vec3_Mul_Outer(out M: TMat4x4; const a, b: TVec3);
var
  i, j: integer;
begin
  FillChar(M, SizeOf(M), 0);
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i][j] := a[i] * b[j];
end;

procedure Mat4x4_Rotate(out R: TMat4x4; const M: TMat4x4; x, y, z, angle: single);
var
  s, c, len: single;
  u: TVec3;
  mT, mS, mC: TMat4x4;
begin
  s := Sin(angle);
  c := Cos(angle);
  u := [x, y, z];
  len := Vec3_Len(u);

  if len > 1e-4 then
  begin
    Vec3_Norm(u, u);
    Mat4x4_From_Vec3_Mul_Outer(mT, u, u);

    FillChar(mS, SizeOf(mS), 0);
    mS[0][1] := u[2];
    mS[0][2] := -u[1];
    mS[1][0] := -u[2];
    mS[1][2] := u[0];
    mS[2][0] := u[1];
    mS[2][1] := -u[0];
    Mat4x4_Scale(mS, mS, s);

    Mat4x4_Identity(mC);
    Mat4x4_Sub(mC, mC, mT);
    Mat4x4_Scale(mC, mC, c);

    Mat4x4_Add(mT, mT, mC);
    Mat4x4_Add(mT, mT, mS);
    mT[3][3] := 1.0;

    Mat4x4_Mul(R, M, mT);
  end
  else
    Mat4x4_Dup(R, M);
end;

procedure Mat4x4_Rotate_X(out Q: TMat4x4; const M: TMat4x4; angle: single);
var
  s, c: single;
  R: TMat4x4;
begin
  s := Sin(angle);
  c := Cos(angle);
  R[0] := [1.0, 0.0, 0.0, 0.0];
  R[1] := [0.0, c, s, 0.0];
  R[2] := [0.0, -s, c, 0.0];
  R[3] := [0.0, 0.0, 0.0, 1.0];
  Mat4x4_Mul(Q, M, R);
end;

procedure Mat4x4_Rotate_Y(out Q: TMat4x4; const M: TMat4x4; angle: single);
var
  s, c: single;
  R: TMat4x4;
begin
  s := Sin(angle);
  c := Cos(angle);
  R[0] := [c, 0.0, -s, 0.0];
  R[1] := [0.0, 1.0, 0.0, 0.0];
  R[2] := [s, 0.0, c, 0.0];
  R[3] := [0.0, 0.0, 0.0, 1.0];
  Mat4x4_Mul(Q, M, R);
end;

procedure Mat4x4_Rotate_Z(out Q: TMat4x4; const M: TMat4x4; angle: single);
var
  s, c: single;
  R: TMat4x4;
begin
  s := Sin(angle);
  c := Cos(angle);
  R[0] := [c, s, 0.0, 0.0];
  R[1] := [-s, c, 0.0, 0.0];
  R[2] := [0.0, 0.0, 1.0, 0.0];
  R[3] := [0.0, 0.0, 0.0, 1.0];
  Mat4x4_Mul(Q, M, R);
end;

procedure Mat4x4_Invert(out T: TMat4x4; const M: TMat4x4);
var
  s, c: array[0..5] of single;
  idet: single;
begin
  s[0] := M[0][0] * M[1][1] - M[1][0] * M[0][1];
  s[1] := M[0][0] * M[1][2] - M[1][0] * M[0][2];
  s[2] := M[0][0] * M[1][3] - M[1][0] * M[0][3];
  s[3] := M[0][1] * M[1][2] - M[1][1] * M[0][2];
  s[4] := M[0][1] * M[1][3] - M[1][1] * M[0][3];
  s[5] := M[0][2] * M[1][3] - M[1][2] * M[0][3];

  c[0] := M[2][0] * M[3][1] - M[3][0] * M[2][1];
  c[1] := M[2][0] * M[3][2] - M[3][0] * M[2][2];
  c[2] := M[2][0] * M[3][3] - M[3][0] * M[2][3];
  c[3] := M[2][1] * M[3][2] - M[3][1] * M[2][2];
  c[4] := M[2][1] * M[3][3] - M[3][1] * M[2][3];
  c[5] := M[2][2] * M[3][3] - M[3][2] * M[2][3];

  idet := 1.0 / (s[0] * c[5] - s[1] * c[4] + s[2] * c[3] + s[3] * c[2] - s[4] * c[1] + s[5] * c[0]);

  T[0][0] := (M[1][1] * c[5] - M[1][2] * c[4] + M[1][3] * c[3]) * idet;
  T[0][1] := (-M[0][1] * c[5] + M[0][2] * c[4] - M[0][3] * c[3]) * idet;
  T[0][2] := (M[3][1] * s[5] - M[3][2] * s[4] + M[3][3] * s[3]) * idet;
  T[0][3] := (-M[2][1] * s[5] + M[2][2] * s[4] - M[2][3] * s[3]) * idet;

  T[1][0] := (-M[1][0] * c[5] + M[1][2] * c[2] - M[1][3] * c[1]) * idet;
  T[1][1] := (M[0][0] * c[5] - M[0][2] * c[2] + M[0][3] * c[1]) * idet;
  T[1][2] := (-M[3][0] * s[5] + M[3][2] * s[2] - M[3][3] * s[1]) * idet;
  T[1][3] := (M[2][0] * s[5] - M[2][2] * s[2] + M[2][3] * s[1]) * idet;

  T[2][0] := (M[1][0] * c[4] - M[1][1] * c[2] + M[1][3] * c[0]) * idet;
  T[2][1] := (-M[0][0] * c[4] + M[0][1] * c[2] - M[0][3] * c[0]) * idet;
  T[2][2] := (M[3][0] * s[4] - M[3][1] * s[2] + M[3][3] * s[0]) * idet;
  T[2][3] := (-M[2][0] * s[4] + M[2][1] * s[2] - M[2][3] * s[0]) * idet;

  T[3][0] := (-M[1][0] * c[3] + M[1][1] * c[1] - M[1][2] * c[0]) * idet;
  T[3][1] := (M[0][0] * c[3] - M[0][1] * c[1] + M[0][2] * c[0]) * idet;
  T[3][2] := (-M[3][0] * s[3] + M[3][1] * s[1] - M[3][2] * s[0]) * idet;
  T[3][3] := (M[2][0] * s[3] - M[2][1] * s[1] + M[2][2] * s[0]) * idet;
end;

procedure Mat4x4_Orthonormalize(out R: TMat4x4; const M: TMat4x4);
var
  proj: single;
  h: TVec3;
  x, y, z: TVec3;
begin
  Mat4x4_Dup(R, M);

  x := [R[0][0], R[0][1], R[0][2]];
  y := [R[1][0], R[1][1], R[1][2]];
  z := [R[2][0], R[2][1], R[2][2]];

  Vec3_Norm(z, z);

  proj := Vec3_Mul_Inner(y, z);
  Vec3_Scale(h, z, proj);
  Vec3_Sub(y, y, h);
  Vec3_Norm(y, y);

  proj := Vec3_Mul_Inner(x, z);
  Vec3_Scale(h, z, proj);
  Vec3_Sub(x, x, h);

  proj := Vec3_Mul_Inner(x, y);
  Vec3_Scale(h, y, proj);
  Vec3_Sub(x, x, h);
  Vec3_Norm(x, x);

  R[0][0] := x[0];
  R[0][1] := x[1];
  R[0][2] := x[2];
  R[1][0] := y[0];
  R[1][1] := y[1];
  R[1][2] := y[2];
  R[2][0] := z[0];
  R[2][1] := z[1];
  R[2][2] := z[2];
end;

procedure Mat4x4_Frustum(out M: TMat4x4; l, r, b, t, n, f: single);
begin
  FillChar(M, SizeOf(M), 0);
  M[0][0] := 2.0 * n / (r - l);
  M[1][1] := 2.0 * n / (t - b);
  M[2][0] := (r + l) / (r - l);
  M[2][1] := (t + b) / (t - b);
  M[2][2] := -(f + n) / (f - n);
  M[2][3] := -1.0;
  M[3][2] := -2.0 * (f * n) / (f - n);
end;

procedure Mat4x4_Ortho(out M: TMat4x4; l, r, b, t, n, f: single);
begin
  FillChar(M, SizeOf(M), 0);
  M[0][0] := 2.0 / (r - l);
  M[1][1] := 2.0 / (t - b);
  M[2][2] := -2.0 / (f - n);
  M[3][0] := -(r + l) / (r - l);
  M[3][1] := -(t + b) / (t - b);
  M[3][2] := -(f + n) / (f - n);
  M[3][3] := 1.0;
end;

procedure Mat4x4_Perspective(out m: TMat4x4; y_fov, aspect, n, f: single);
var
  a: single;
begin
  a := 1.0 / Tan(y_fov / 2.0);
  FillChar(m, SizeOf(m), 0);
  m[0][0] := a / aspect;
  m[1][1] := a;
  m[2][2] := -(f + n) / (f - n);
  m[2][3] := -1.0;
  m[3][2] := -(2.0 * f * n) / (f - n);
end;

procedure Mat4x4_Look_At(out m: TMat4x4; const eye, center, up: TVec3);
var
  f, s, u: TVec3;
begin
  Vec3_Sub(f, center, eye);
  Vec3_Norm(f, f);

  Vec3_Mul_Cross(s, f, up);
  Vec3_Norm(s, s);

  Vec3_Mul_Cross(u, s, f);

  m[0] := [s[0], u[0], -f[0], 0.0];
  m[1] := [s[1], u[1], -f[1], 0.0];
  m[2] := [s[2], u[2], -f[2], 0.0];
  m[3] := [0.0, 0.0, 0.0, 1.0];

  Mat4x4_Translate_In_Place(m, -eye[0], -eye[1], -eye[2]);
end;

// Quaternioni

procedure Quat_Identity(out q: TQuat); inline;
begin
  q[0] := 0.0;
  q[1] := 0.0;
  q[2] := 0.0;
  q[3] := 1.0;
end;

procedure Quat_Mul(out r: TQuat; const p, q: TQuat);
var
  px, py, pz, pw: single;
  qx, qy, qz, qw: single;
  tmp, w: TVec3;
begin
  // Estrai componenti per evitare problemi con TVec4
  px := p[0];
  py := p[1];
  pz := p[2];
  pw := p[3];
  qx := q[0];
  qy := q[1];
  qz := q[2];
  qw := q[3];

  // tmp = cross(p.xyz, q.xyz)
  tmp[0] := py * qz - pz * qy;
  tmp[1] := pz * qx - px * qz;
  tmp[2] := px * qy - py * qx;

  // w = p.xyz * q.w
  w[0] := px * qw;
  w[1] := py * qw;
  w[2] := pz * qw;
  Vec3_Add(tmp, tmp, w);

  // w = q.xyz * p.w
  w[0] := qx * pw;
  w[1] := qy * pw;
  w[2] := qz * pw;
  Vec3_Add(tmp, tmp, w);

  r[0] := tmp[0];
  r[1] := tmp[1];
  r[2] := tmp[2];
  r[3] := pw * qw - (px * qx + py * qy + pz * qz);
end;

procedure Quat_Conj(out r: TQuat; const q: TQuat); inline;
begin
  r[0] := -q[0];
  r[1] := -q[1];
  r[2] := -q[2];
  r[3] := q[3];
end;

procedure Quat_Rotate(out r: TQuat; angle: single; const axis: TVec3);
var
  axis_norm: TVec3;
  s, c: single;
begin
  Vec3_Norm(axis_norm, axis);
  s := Sin(angle / 2.0);
  c := Cos(angle / 2.0);
  Vec3_Scale(PVec3(@r)^, axis_norm, s);
  r[3] := c;
end;

procedure Quat_Mul_Vec3(out r: TVec3; const q: TQuat; const v: TVec3);
var
  t, q_xyz, u: TVec3;
begin
  q_xyz := [q[0], q[1], q[2]];
  u := q_xyz;

  Vec3_Mul_Cross(t, q_xyz, v);
  Vec3_Scale(t, t, 2.0);

  Vec3_Mul_Cross(u, q_xyz, t);
  Vec3_Scale(t, t, q[3]);

  Vec3_Add(r, v, t);
  Vec3_Add(r, r, u);
end;

procedure Mat4x4_From_Quat(out M: TMat4x4; const q: TQuat);
var
  a, b, c, d, a2, b2, c2, d2: single;
begin
  a := q[3];
  b := q[0];
  c := q[1];
  d := q[2];
  a2 := a * a;
  b2 := b * b;
  c2 := c * c;
  d2 := d * d;

  M[0] := [a2 + b2 - c2 - d2, 2 * (b * c + a * d), 2 * (b * d - a * c), 0.0];
  M[1] := [2 * (b * c - a * d), a2 - b2 + c2 - d2, 2 * (c * d + a * b), 0.0];
  M[2] := [2 * (b * d + a * c), 2 * (c * d - a * b), a2 - b2 - c2 + d2, 0.0];
  M[3] := [0.0, 0.0, 0.0, 1.0];
end;

procedure Mat4x4o_Mul_Quat(out R: TMat4x4; const M: TMat4x4; const q: TQuat);
var
  v: TVec3;
begin
  v := [M[0][0], M[0][1], M[0][2]];
  Quat_Mul_Vec3(v, q, v);
  R[0] := [v[0], v[1], v[2], M[0][3]];

  v := [M[1][0], M[1][1], M[1][2]];
  Quat_Mul_Vec3(v, q, v);
  R[1] := [v[0], v[1], v[2], M[1][3]];

  v := [M[2][0], M[2][1], M[2][2]];
  Quat_Mul_Vec3(v, q, v);
  R[2] := [v[0], v[1], v[2], M[2][3]];

  R[3] := [0.0, 0.0, 0.0, M[3][3]];
end;

procedure Quat_From_Mat4x4(out q: TQuat; const M: TMat4x4);
var
  trace, r: single;
  i: integer;
  perm: array[0..4] of integer = (0, 1, 2, 0, 1);
  p: PInteger;
begin
  trace := M[0][0] + M[1][1] + M[2][2];
  if trace > 0.0 then
  begin
    r := Sqrt(1.0 + trace);
    q[3] := r / 2.0;
    r := 0.5 / r;
    q[0] := (M[1][2] - M[2][1]) * r;
    q[1] := (M[2][0] - M[0][2]) * r;
    q[2] := (M[0][1] - M[1][0]) * r;
  end
  else
  begin
    r := 0.0;
    p := @perm[0];
    for i := 0 to 2 do
      if M[i][i] > r then
      begin
        r := M[i][i];
        p := @perm[i];
      end;

    r := Sqrt(1.0 + M[p[0]][p[0]] - M[p[1]][p[1]] - M[p[2]][p[2]]);

    if r < 1e-6 then
    begin
      q[0] := 1.0;
      q[1] := 0.0;
      q[2] := 0.0;
      q[3] := 0.0;
      Exit;
    end;

    q[p[0]] := r / 2.0;
    q[p[1]] := (M[p[0]][p[1]] + M[p[1]][p[0]]) / (2.0 * r);
    q[p[2]] := (M[p[0]][p[2]] + M[p[2]][p[0]]) / (2.0 * r);
    q[3] := (M[p[1]][p[2]] - M[p[2]][p[1]]) / (2.0 * r);
  end;
end;

procedure Mat4x4_Arcball(out R: TMat4x4; const M: TMat4x4; const _a, _b: TVec2; s: single);
var
  a, b: TVec2;
  z_a, z_b: single;
  a3, b3, axis: TVec3;
  angle: single;
begin
  a := _a;
  b := _b;

  if Vec2_Len(a) < 1.0 then
    z_a := Sqrt(1.0 - Vec2_Mul_Inner(a, a))
  else
  begin
    Vec2_Norm(a, a);
    z_a := 0.0;
  end;

  if Vec2_Len(b) < 1.0 then
    z_b := Sqrt(1.0 - Vec2_Mul_Inner(b, b))
  else
  begin
    Vec2_Norm(b, b);
    z_b := 0.0;
  end;

  a3 := [a[0], a[1], z_a];
  b3 := [b[0], b[1], z_b];

  Vec3_Mul_Cross(axis, a3, b3);
  angle := ArcCos(Vec3_Mul_Inner(a3, b3)) * s;

  Mat4x4_Rotate(R, M, axis[0], axis[1], axis[2], angle);
end;

end.
