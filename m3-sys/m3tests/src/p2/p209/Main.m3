MODULE Main;
IMPORT IO;

VAR a : ARRAY [0..0] OF CHAR := ARRAY OF CHAR {'a'};
VAR b : ARRAY [0..0] OF CHAR := ARRAY OF CHAR {'b'};
VAR c : ARRAY [0..0] OF CHAR := ARRAY OF CHAR {'c'};
VAR d : ARRAY [0..0] OF CHAR := ARRAY OF CHAR {'d'};

CONST i : ARRAY [0..0] OF CHAR = ARRAY OF CHAR {'i'};
CONST j : ARRAY [0..0] OF CHAR = ARRAY OF CHAR {'j'};

BEGIN
  IO.PutChar(a[0]); IO.Put(", "); 
  IO.PutChar(b[0]); IO.Put(", "); 
  IO.PutChar(c[0]); IO.Put(", "); 
  IO.PutChar(d[0]); IO.Put("\n"); 

  a[0] := 'e';
  b[0] := 'f';
  c[0] := 'g';
  d[0] := 'h';
  IO.PutChar(a[0]); IO.Put(", "); 
  IO.PutChar(b[0]); IO.Put(", "); 
  IO.PutChar(c[0]); IO.Put(", "); 
  IO.PutChar(d[0]); IO.Put("\n"); 

  IO.PutChar(i[0]); IO.Put(", "); 
  IO.PutChar(j[0]); IO.Put("\n"); 

  IO.Put("OK\n");
END Main.
