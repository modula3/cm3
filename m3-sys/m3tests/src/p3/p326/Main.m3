(* A nested procedure passed as a value captures a variable that only it
   uses, from two levels out, and a record VALUE formal of the procedure
   it is nested in: the enclosing procedure captures them too (B35). *)

MODULE Main;
IMPORT IO, Fmt;
TYPE Proc = PROCEDURE (x: INTEGER;  args: REFANY);
     Xform = RECORD scale, off: INTEGER END;
VAR sum := 0;
PROCEDURE Each (READONLY a: ARRAY OF INTEGER;  p: PROCEDURE (x: INTEGER)) =
  BEGIN FOR i := FIRST (a) TO LAST (a) DO p (a[i]) END END Each;
PROCEDURE Add (x: INTEGER;  <*UNUSED*> args: REFANY) = BEGIN INC (sum, x) END Add;
PROCEDURE Apply (t: Xform;  x: INTEGER): INTEGER = BEGIN RETURN x * t.scale + t.off END Apply;
PROCEDURE FlatMapAll (rectProc: Proc;  args: REFANY) =
  PROCEDURE Recurse (depth: INTEGER;  transform: Xform) =
    BEGIN
      PROCEDURE Mapper (r: INTEGER) =
        BEGIN rectProc (Apply (transform, r), args) END Mapper;
      BEGIN
        IF rectProc # NIL THEN Each (ARRAY OF INTEGER {1, 2, 3}, Mapper) END;
      END;
      IF depth > 0 THEN
        Recurse (depth - 1, Xform {transform.scale * 10, transform.off + 1});
      END;
    END Recurse;
  BEGIN
    Recurse (2, Xform {1, 0});
  END FlatMapAll;
BEGIN
  FlatMapAll (Add, NIL);
  IO.Put (Fmt.Int (sum) & "\n");
END Main.
