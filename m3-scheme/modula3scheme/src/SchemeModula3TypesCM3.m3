(* $Id$ *)

MODULE SchemeModula3TypesCM3 EXPORTS SchemeModula3Types;
FROM SchemeUtils IMPORT Stringify;
IMPORT Scheme;
IMPORT SchemeObject, SchemeLongReal, SchemeChar;
IMPORT Text;

PROCEDURE ToScheme_LONGINT(i : REFANY) : SchemeObject.T =
  BEGIN 
    RETURN SchemeLongReal.FromLR(FLOAT(NARROW(i,REF LONGINT)^,
                                       LONGREAL)) 
  END ToScheme_LONGINT;

PROCEDURE ToScheme_WIDECHAR(i : REFANY) : SchemeObject.T =
  BEGIN 
    RETURN SchemeChar.Character(
               Text.GetChar(
                   Text.FromWideChar(NARROW(i,REF WIDECHAR)^),0))
  END ToScheme_WIDECHAR;

PROCEDURE ToModula_LONGINT(c : SchemeObject.T) : REFANY RAISES { Scheme.E }=
  BEGIN
    WITH flt = SchemeLongReal.FromO(c) DO
      IF flt < FLOAT(FIRST(LONGINT),LONGREAL) OR flt > FLOAT(LAST(LONGINT),LONGREAL) THEN
        RAISE Scheme.E("LONGINT out of range : " & Stringify(c))
      END;

      WITH int = ROUND(flt,LONGINT) DO
        IF FLOAT(int,LONGREAL) # flt THEN
          RAISE Scheme.E("Not a LONGINT : " & Stringify(c))
        END;
        WITH res = NEW(REF LONGINT) DO
          res^ := int;
          RETURN res
        END
      END
    END
  END ToModula_LONGINT;

PROCEDURE ToModula_WIDECHAR(c : SchemeObject.T) : REFANY RAISES { Scheme.E } =
  BEGIN 
    WITH ch = SchemeChar.Char(c),
         res = NEW(REF WIDECHAR) DO
      res^ := Text.GetWideChar(Text.FromChar(ch),0);
      RETURN res
    END
  END ToModula_WIDECHAR;

BEGIN END SchemeModula3TypesCM3.
