(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: RefListDebug.i3                                       *)
(* Last modified on Tue Aug  6 17:02:21 PDT 1996 by detlefs    *)

INTERFACE RefListDebug;

IMPORT RT0;

(* Returns the number of "RefList.T"'s that can be reached from the
   object "r", assuming no sharing of "RefList.T"'s.  The
   "topTypeCode" is the typecode of objects containing the most
   pointers to "RefList.T"'s; "nForTopType" is the number of such
   pointers. *)
PROCEDURE Do(r: REFANY;
             VAR (*OUT*) topTypeCode, nForTopType: INTEGER): INTEGER;

(* Returns the address and module name of the global variable from
   which the most RefList.T's are reachable, along with the number of
   such RefList.T's. *)
   
PROCEDURE DoMods(VAR (*OUT*) modName: RT0.String;
                 VAR (*OUT*) addr: INTEGER): INTEGER;

END RefListDebug.


