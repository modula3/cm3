(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 11:20:52 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE Xmacro;

IMPORT Xt, M3toC, Cstring;
FROM Ctypes IMPORT short, unsigned_short, char_star;

(*-----------------*)
PROCEDURE AddrVal(value:ADDRESS):Xt.ArgVal =
BEGIN
  RETURN LOOPHOLE(value,Xt.ArgVal);
END AddrVal;

(*-----------------*)
PROCEDURE CharVal(value:CHAR):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
    (* does this work on big-endian machines?? *)
  END CharVal;

(**** not portable to 64 bit machines *****
PROCEDURE CharVal(value:CHAR):Xt.ArgVal=
  VAR tmp: RECORD pad1:short; pad2:CHAR; val:CHAR; END;
  BEGIN
    tmp.pad1:=0; tmp.pad2:='\000'; tmp.val:=value;
    RETURN LOOPHOLE(tmp, Xt.ArgVal);
  END CharVal;
****************************************)

(*-----------------*)
PROCEDURE IntVal(value:INTEGER):Xt.ArgVal=
BEGIN
  RETURN LOOPHOLE(value,Xt.ArgVal);
END IntVal;

(*-----------------*)
PROCEDURE ShortVal(value:short):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
  END ShortVal;

(**** not portable to 64 bit machines *****
PROCEDURE ShortVal(value:short):Xt.ArgVal=
  VAR tmp: RECORD pad:short; val:short; END;
  BEGIN
    tmp.pad:=0; tmp.val:=value;
    RETURN LOOPHOLE(tmp, Xt.ArgVal);
  END ShortVal;
****************************************)

(*-----------------*)
PROCEDURE TextVal(value:TEXT):Xt.ArgVal=
BEGIN
  RETURN LOOPHOLE(M3toC.TtoS(value), Xt.ArgVal);
END TextVal;

(*-----------------*)
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
  END UShortVal;

(**** not portable to 64 bit machines *****
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal=
  VAR tmp: RECORD pad:short; val:unsigned_short; END;
  BEGIN
    tmp.pad:=0; tmp.val:=value;
    RETURN LOOPHOLE(tmp,Xt.ArgVal);
  END UShortVal;
****************************************)

(*-----------------*)
PROCEDURE XtNewString(str:char_star):char_star =
VAR
  len:CARDINAL;
  newstr:char_star;
BEGIN
  len:=Cstring.strlen(str);
  newstr:=LOOPHOLE(Xt.Malloc(len+1),char_star);
  EVAL Cstring.strncpy(newstr,str,len);  
  RETURN newstr;  
END XtNewString;

(*-----------------*)
BEGIN
END Xmacro.


