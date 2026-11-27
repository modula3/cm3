(* Stores of negative values into signed packed fields and elements
   leave their neighbours alone (the MSIR store did not mask the value). *)

MODULE Main;
IMPORT IO, Fmt;
TYPE R = RECORD a: BITS 3 FOR [-4..3]; b: BITS 3 FOR [0..7]; c: BITS 2 FOR [0..3] END;
     A = ARRAY [0..7] OF BITS 2 FOR [-2..1];
     W = RECORD x: BITS 5 FOR [0..31]; y: BITS 21 FOR [-1000000..1000000]; z: BITS 6 FOR [0..63] END;
VAR r: R;  a: A;  w: W;  v := -1;  m4 := -4;  big := -999999;
BEGIN
  r.b := 5; r.c := 2; r.a := v;
  IO.Put("r " & Fmt.Int(r.a) & " " & Fmt.Int(r.b) & " " & Fmt.Int(r.c) & "\n");
  r.a := m4; IO.Put("r " & Fmt.Int(r.a) & " " & Fmt.Int(r.b) & " " & Fmt.Int(r.c) & "\n");
  FOR k := 0 TO 7 DO a[k] := 1 END;
  FOR k := 0 TO 7 BY 2 DO a[k] := v END;
  IO.Put("a"); FOR k := 0 TO 7 DO IO.Put(" " & Fmt.Int(a[k])) END; IO.Put("\n");
  w.x := 17; w.z := 42; w.y := big;
  IO.Put("w " & Fmt.Int(w.x) & " " & Fmt.Int(w.y) & " " & Fmt.Int(w.z) & "\n");
END Main.
