(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubLibPrivate.i3 *)
(* Last modified on Mon Feb 14 13:03:59 PST 1994 by wobber      *)
(*      modified on Fri Feb  5 10:14:33 PST 1993 by owicki      *)
(*      modified on Tue Dec  8 10:22:26 1992 by gnelson     *)

INTERFACE StubLibPrivate;

IMPORT NetObj, Rd, Wr, Thread;
FROM StubLib IMPORT Conn, DataRep, Int32;

PROCEDURE OutObject(c: Conn; o: NetObj.T)
    RAISES {Wr.Failure, Thread.Alerted};
   
PROCEDURE InObject(c: Conn; tc := -1): NetObj.T
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
            
PROCEDURE OutText(c: Conn; text: TEXT)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a "TEXT" in native format. *)

PROCEDURE OutTexts(c: Conn; texts: REF ARRAY OF TEXT)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a "REF ARRAY OF TEXT" in native format. *)

PROCEDURE InText(c: Conn; rep: DataRep): TEXT
    RAISES {NetObj.Error, Rd.Failure,
            Thread.Alerted};
(* Unmarshal a "TEXT". *)

PROCEDURE InTexts(
    c: Conn; rep: DataRep): REF ARRAY OF TEXT   
    RAISES {NetObj.Error, Rd.Failure,
            Thread.Alerted};
(* Unmarshal a "REF ARRAY OF TEXT". *)


(* byte-swap support *)

PROCEDURE NativeEndian(rep: DataRep): BOOLEAN;

PROCEDURE Swap32(i: Int32) : Int32;


END StubLibPrivate.
