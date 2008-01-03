(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 09:02:10 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QVal;

IMPORT M3Buf, QValue, QVTbl, QVSeq;
FROM Quake IMPORT Error, Machine, ID;

TYPE
  T = QValue.T;

PROCEDURE ToTag   (m: Machine; READONLY t: T): TEXT           RAISES {Error};
PROCEDURE ToBool  (m: Machine; READONLY t: T): BOOLEAN        RAISES {Error};
PROCEDURE ToInt   (m: Machine; READONLY t: T): INTEGER        RAISES {Error};
PROCEDURE ToText  (m: Machine; READONLY t: T): TEXT           RAISES {Error};
PROCEDURE ToID    (m: Machine; READONLY t: T): ID             RAISES {Error};
PROCEDURE ToTable (m: Machine; READONLY t: T): QVTbl.T        RAISES {Error};
PROCEDURE ToArray (m: Machine; READONLY t: T): QVSeq.T        RAISES {Error};
PROCEDURE ToProc  (m: Machine; READONLY t: T): QValue.Proc    RAISES {Error};
PROCEDURE ToBuf   (m: Machine; READONLY t: T;  buf: M3Buf.T)  RAISES {Error};

END QVal.
