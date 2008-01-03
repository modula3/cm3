(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Voucher.i3 *)
(* Last modified on Mon Aug 30 15:46:28 PDT 1993 by wobber  *)
(*      modified on Sat Dec  5 16:16:31 1992 by gnelson *)

(* The "Voucher" interface defines a single type "Voucher.T" which 
   is a network object which refers to a concrete reader or writer. 
   This object is marshalled as the network representation of a "Rd.T" 
   or "Wr.T" during normal argument or result transmission.  The 
   destination can initiate the network transfer of the reader's 
   source or the writer's target by invoking the appropriate {\it claim}
   method. *)

INTERFACE Voucher;
   
IMPORT NetObj, Rd, Wr, Thread;

TYPE
  T = NetObj.T OBJECT METHODS
    claimRd() : Rd.T
      RAISES {NetObj.Error, Thread.Alerted};
    claimWr() : Wr.T
      RAISES {NetObj.Error, Thread.Alerted};
  END;

(* The "claimRd" method returns a reader whose source is equal to 
that of the original concrete reader.  The "claimWr" method returns 
a writer whose target is that of the remote concrete writer.

The surrogate stream position equals the initial position of the
original reader or writer at the time it was marshalled.  The surrogate
is not seekable.

Marshaling of readers is implemented as follows: "StubLib.OutRef" marshals
a concrete reader as a network object (a "Voucher.T") containing a
"claimRd" method that reads any unread data from "rd" and writes
it to the network in such a way that it appears as the source of
the invoker's message reader.  "StubLib.OutRef" marshals this like
any other network object. "StubLib.InRef" unmarshals the voucher object.
If it is non-"NIL", "StubLib.InRef" invokes its "claimRd" method.  The
resulting network message reader is returned to the client.  The
implementation for network writers is similar.  *)

END Voucher.

