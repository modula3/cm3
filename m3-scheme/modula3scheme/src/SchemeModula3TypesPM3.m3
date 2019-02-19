(* $Id$ *)

MODULE SchemeModula3TypesPM3 EXPORTS SchemeModula3Types;
FROM SchemeUtils IMPORT Stringify;
IMPORT Scheme;
IMPORT SchemeObject, SchemeString, SchemeBoolean, SchemeLongReal, SchemeChar;

PROCEDURE ToScheme_LONGINT(i : REFANY) : SchemeObject.T =
  BEGIN 
    <*ASSERT FALSE*>
  END ToScheme_LONGINT;

PROCEDURE ToScheme_WIDECHAR(i : REFANY) : SchemeObject.T =
  BEGIN 
    <*ASSERT FALSE*>
  END ToScheme_WIDECHAR;

PROCEDURE ToModula_LONGINT(c : SchemeObject.T) : REFANY RAISES { Scheme.E }=
  BEGIN
    <*ASSERT FALSE*>
  END ToModula_LONGINT;

PROCEDURE ToModula_WIDECHAR(c : SchemeObject.T) : REFANY RAISES { Scheme.E } =
  BEGIN 
    <*ASSERT FALSE*>
  END ToModula_WIDECHAR;

BEGIN END SchemeModula3TypesPM3.
