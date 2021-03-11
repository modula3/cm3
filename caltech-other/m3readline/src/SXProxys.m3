(* $Id$ *)

MODULE SXProxys EXPORTS VarUI;
IMPORT IntSXProxy, TextSXProxy, BoolSXProxy, LongRealSXProxy AS LRSXProxy;

IMPORT Fmt, Scan, Lex, FloatMode;
IMPORT SXConversion;

REVEAL
  IntSXProxy.Super = IntSXProxy.SuperClass BRANDED OBJECT OVERRIDES
    toText := IntToText;
    fromText := IntFromText;
  END;

PROCEDURE IntToText(<*UNUSED*>p : IntSXProxy.Super; int : INTEGER) : TEXT =
  BEGIN RETURN Fmt.Int(int) END IntToText;

PROCEDURE IntFromText(<*UNUSED*>p : IntSXProxy.Super; txt : TEXT) : INTEGER 
  RAISES { SXConversion.Error } =
  BEGIN
    TRY
      RETURN Scan.Int(txt)
    EXCEPT
      Lex.Error, FloatMode.Trap => 
      RAISE SXConversion.Error("Can't convert \"" & txt & "\" to INTEGER")
    END
  END IntFromText;

(**********************************************************************)

REVEAL
  LRSXProxy.Super = LRSXProxy.SuperClass BRANDED OBJECT OVERRIDES
    toText := LRToText;
    fromText := LRFromText;
  END;

PROCEDURE LRToText(<*UNUSED*>p : LRSXProxy.Super; LR : LONGREAL) : TEXT =
  BEGIN RETURN Fmt.LongReal(LR) END LRToText;

PROCEDURE LRFromText(<*UNUSED*>p : LRSXProxy.Super; txt : TEXT) : LONGREAL 
  RAISES { SXConversion.Error } =
  BEGIN
    TRY
      RETURN Scan.LongReal(txt)
    EXCEPT
      Lex.Error, FloatMode.Trap => 
      RAISE SXConversion.Error("Can't convert \"" & txt & "\" to LONGREAL")
    END
  END LRFromText;


(**********************************************************************)

REVEAL
  TextSXProxy.Super = TextSXProxy.SuperClass BRANDED OBJECT OVERRIDES
    toText := TextToText;
    fromText := TextFromText;
  END;

PROCEDURE TextToText(<*UNUSED*>p : TextSXProxy.Super; text : TEXT) : TEXT =
  BEGIN 
    IF text = NIL THEN RETURN "****NIL****" ELSE RETURN text END
  END TextToText;

PROCEDURE TextFromText(<*UNUSED*>p : TextSXProxy.Super; txt : TEXT) : TEXT =
  BEGIN
    RETURN txt
  END TextFromText;

(**********************************************************************)


REVEAL
  BoolSXProxy.Super = BoolSXProxy.SuperClass BRANDED OBJECT OVERRIDES
    toText := BoolToText;
    fromText := BoolFromText;
  END;

PROCEDURE BoolToText(<*UNUSED*>p : BoolSXProxy.Super; bool : BOOLEAN) : TEXT =
  BEGIN RETURN Fmt.Bool(bool) END BoolToText;

PROCEDURE BoolFromText(<*UNUSED*>p : BoolSXProxy.Super; txt : TEXT) : BOOLEAN 
  RAISES { SXConversion.Error } =
  BEGIN
    TRY
      RETURN Scan.Bool(txt)
    EXCEPT
      Lex.Error => 
      RAISE SXConversion.Error("Can't convert \"" & txt & "\" to BOOLEAN")
    END
  END BoolFromText;


BEGIN END SXProxys.
