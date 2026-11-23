(* Dereferencing NIL at an offset past the first page: the compiler must
   check for NIL explicitly, since the access need not fault in hardware. *)
MODULE Main;
TYPE Big = REF RECORD pad: ARRAY [0..99999] OF INTEGER;  far: INTEGER END;
VAR b: Big := NIL;
BEGIN
  b.far := 7;
END Main.
