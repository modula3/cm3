GENERIC MODULE ResidueClassFmtLex(RF);
(* Arithmetic for Modula-3, see doc for details *)

FROM FmtLexSupport IMPORT Precedence;

<* UNUSED *>
CONST
  Module = "ResidueClassFmtLex.";

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}; ): TEXT =
  BEGIN
    RETURN "ResidueClass{r:=" & RF.Fmt(x.r, style.elemStyle) & "," & "d:="
             & RF.Fmt(x.d, style.elemStyle) & "}";
  END Fmt;

PROCEDURE Tex (             READONLY x     : T;
                            READONLY style       := TexStyle{};
               <* UNUSED *>          within      := Precedence.Sum; ):
  TEXT =
  VAR t := "[" & RF.Tex(x.r, style.elemStyle, Precedence.Sum) & "]";
  BEGIN
    IF TexFlag.ShowDivisor IN style.flags THEN
      t := t & "_{" & RF.Tex(x.d, style.elemStyle, Precedence.Sum) & "}";
    END;
    RETURN t;
  END Tex;

BEGIN
END ResidueClassFmtLex.
