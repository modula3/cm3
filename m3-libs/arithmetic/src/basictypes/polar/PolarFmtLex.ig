GENERIC INTERFACE PolarFmtLex(P, RF);
(*Copyright (c) 1996, m3na project*)

FROM FmtLexSupport IMPORT Precedence;
(*
FROM NADefinitions IMPORT Error;
*)

(*==========================*)
TYPE
  T = P.T;
  FmtStyle = RECORD elemStyle := RF.FmtStyle{};  END;
  TexStyle = RECORD elemStyle := RF.TexStyle{};  END;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}): TEXT;
(*as "POLAR{radius:=<r>; angle:=<r>}"*)

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.sum): TEXT;

(*==========================*)
END PolarFmtLex.
