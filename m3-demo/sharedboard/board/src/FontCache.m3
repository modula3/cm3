(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE FontCache;

IMPORT Font, FontTbl, IO, Stdio;

VAR table := NEW (FontTbl.Default).init (100);
VAR debug := 0;

PROCEDURE Get (READONLY name: TEXT): Font.T =
  VAR font: Font.T;
  BEGIN
    IF debug=1 THEN
      IO.Put (name, Stdio.stderr);
    END;
    IF NOT table.get (name, font) THEN 
      font := Font.FromName (ARRAY [0..0] OF TEXT {name});
      EVAL table.put (name, font);
    END;
    RETURN font;
  END Get;

BEGIN
END FontCache.
