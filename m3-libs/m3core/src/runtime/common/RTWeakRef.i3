(*| Copyright 1992 Digital Equipment Corporation. *)
(*| Distributed only by permission. *)

(*| Last modified on Wed Nov 24 09:25:31 PST 1993 by kalsow  *)
(*|      modified on Wed May  5 15:18:26 PDT 1993 by mjordan *)
(*|      modified on Fri Feb 26 13:56:08 PST 1993 by jdd *)

(* "RTWeakRef" is a private interface. *)

INTERFACE RTWeakRef;

(* This is the low-level ("runtime") interface for weak refs.  This
   interface is used by the higher-level WeakRef.  In fact, the following
   abstractions are currently the same *)

TYPE WeakRef =  
  RECORD
    byte: ARRAY [0..7] OF BITS 8 FOR [0..255]
  END;

PROCEDURE WeakRefFromRef (r: REFANY; p: WeakRefCleanUpProc := NIL):
  WeakRef;

PROCEDURE WeakRefToRef (READONLY w: WeakRef): REFANY;

TYPE WeakRefCleanUpProc = PROCEDURE (READONLY w: WeakRef; r: REFANY);

END RTWeakRef.
