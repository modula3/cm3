(* $Id$ *)

MODULE Main;
IMPORT Matrix, LRVector, Tridiagonal;

VAR
  a : ARRAY [0..2] OF ARRAY [0..2] OF LONGREAL;
  d := a[0];
  e := a[0];
BEGIN
  a[0,0]:= 102.0d0;
  a[0,1]:= 107.0d0;
  a[0,2]:=   2.0d0;
  
  a[1,0]:=   5.0d0;
  a[1,1]:= 232.0d0;
  a[1,2]:= 172.0d0;
  
  a[2,0]:=  12.0d0;
  a[2,1]:=  22.0d0;
  a[2,2]:=  42.0d0;
  
  Tridiagonal.Reduce(a,d,e);
  Tridiagonal.QLi(d,e,a);

END Main.
