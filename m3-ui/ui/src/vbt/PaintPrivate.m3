(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Jul  8 17:06:44 PDT 1994 by msm      *)
(*      modified on Mon Feb 24 13:57:26 PST 1992 by muller   *)
(*      modified on Tue Oct 22 21:24:27 PDT 1991 by gnelson  *)
<*PRAGMA LL*>

UNSAFE MODULE PaintPrivate;

PROCEDURE CommandLength(cptr: CommandPtr): INTEGER =
  BEGIN
    IF cptr.command <= PaintCommand.TrapCom THEN
      RETURN ComSize[cptr.command]
    ELSE
      WITH varSzPtr = LOOPHOLE(cptr, VarSzPtr) DO
        RETURN varSzPtr^.szOfRec
      END
    END
  END CommandLength;
  
VAR 
  x := 1;
  p := LOOPHOLE(ADR(x), UNTRACED REF CHAR);
  
BEGIN
  HostByteOrder := VAL(ORD(p^), ByteOrder)
END PaintPrivate.
