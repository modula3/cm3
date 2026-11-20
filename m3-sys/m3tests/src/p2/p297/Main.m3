MODULE Main;
IMPORT Stdio, Wr, Fmt;

(* Test subscript applied directly to an array constructor. *)

PROCEDURE Say(t : TEXT) = <*FATAL ANY*>
  BEGIN Wr.PutText(Stdio.stdout, t & "\n"); Wr.Flush(Stdio.stdout) END Say;

PROCEDURE PickConst(f : BOOLEAN) : INTEGER =
  BEGIN RETURN ARRAY BOOLEAN OF INTEGER{7, 99}[f] END PickConst;

PROCEDURE PickVarLt(f : BOOLEAN; v : INTEGER) : INTEGER =
    VAR Result := ARRAY BOOLEAN OF INTEGER{v, 8} [f];
  BEGIN
    RETURN Result
  END PickVarLt;
  
PROCEDURE PickVarRt(f : BOOLEAN; v : INTEGER) : INTEGER =
    VAR Result := ARRAY BOOLEAN OF INTEGER{9, v} [f];
  BEGIN
    RETURN Result
  END PickVarRt;
  
BEGIN
  Say("const elements, [FALSE]    expect 7  -> " & Fmt.Int(PickConst(FALSE)));
  Say("const elements, [TRUE]     expect 99 -> " & Fmt.Int(PickConst(TRUE)));
  
  Say("var element,lt  [FALSE,43] expect 43  -> " & Fmt.Int(PickVarLt(FALSE, 43)));
  Say("var element,lt  [TRUE,43]  expect 8 -> " & Fmt.Int(PickVarLt(TRUE, 43)));
  
  Say("var element,rt  [FALSE,44] expect 9  -> " & Fmt.Int(PickVarRt(FALSE, 44)));
  Say("var element,rt  [TRUE,44]  expect 44 -> " & Fmt.Int(PickVarRt(TRUE, 44)));
END Main.

