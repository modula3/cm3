(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Mar 10 18:59:06 1992 by steveg   *)
(*      modified on Mon Feb 24 13:58:00 PST 1992 by muller   *)
(*      modified on Sat Dec 21 17:45:24 PST 1991 by gnelson  *)
(*      modified on Fri Sep 13  1:42:24 PDT 1991 by msm      *)

<*PRAGMA LL*>

(* A "ScrnCursor.T" is a handle on a cursor shape that is valid for some
   particular screentype, called the {\it owner} of the handle.  Some
   handles have names; others are anonymous.  A named handle is valid
   forever.  The cursor referenced by an anonymous handle will be
   garbage-collected when all handles to it have been dropped.  *)

INTERFACE ScrnCursor;

IMPORT TrestleComm, Cursor;

EXCEPTION Failure;

VAR DontCare: T;

TYPE Raw = Cursor.Raw;

(* See the "Cursor" interface for the raw representation of a cursor 
   shape as a pair of bitmaps, color information, and hotspot offset.

\subsubsection{Obtaining handles from the oracle} *)

TYPE
  Oracle = Private OBJECT (*CONST*)
    width, height: INTEGER;
  METHODS
    <* LL.sup <= VBT.mu *>
    load(READONLY r: Raw; nm: TEXT := NIL): T 
      RAISES {TrestleComm.Failure};
    list(pat: TEXT; maxResults: CARDINAL := 1)
      : REF ARRAY OF TEXT 
      RAISES {TrestleComm.Failure};
    lookup(name: TEXT): T RAISES {TrestleComm.Failure};
    builtIn(cs: Cursor.Predefined): T;
  END;
  Private <: ROOT;

(* For a screentype "st", the field "st.cursor" is an "Oracle" that
   produces cursors owned by "st":

   The integers "st.cursor.width" and "st.cursor.height" are the 
   dimensions in pixels of the largest cursor image that the screentype "st" 
   supports. Larger images will be cropped; smaller images will be padded.
   
   The method call 

| st.cursor.load(r, nm)

   allocates and returns a cursor handle "c" owned by "st" whose
   contents are equal to "r".  If "nm # NIL", "c" receives the name
   "nm", and any cursor handle owned by "st" that previously had the
   name "nm" becomes anonymous.

   The method call 

| st.cursor.list(pat, maxResults)

   returns the names of all cursors owned by "st" that match the pattern
   "pat".  The list of results may be truncated to length "maxResults".
   A "*" matches any number of characters and a "?" matches a single
   character.

   The method call 
   
| st.cursor.lookup(name)

   return the cursor handle owned by "st" with the given name, or "NIL"
   if no cursor has this name.

   The method call

| st.cursor.builtIn(cs)

   returns the screen-dependent cursor valid for "st" that corresponds
   to the predefined screen-independent cursor "Cursor.T{cs}" .
   
   The locking level for all methods is "LL.sup <= VBT.mu". *)

(* \subsubsection{The handle object} *)

TYPE
  T <: Public; 
  Public = OBJECT (*CONST*) 
    id: INTEGER 
  METHODS 
    <* LL.sup <= VBT.mu *>
    localize(): Raw 
      RAISES {TrestleComm.Failure, Failure};
    unload() RAISES {TrestleComm.Failure};
  END;

(* If "cs" is a "ScrnCursor.T", then "cs.id" is an identifier whose
    interpretation depends on the screentype that owns "cs".  The method
    call "cs.localize()" returns a raw cursor equal to the one on which
    "cs" is a handle, and the method call "cs.unload()" causes "cs"
    to become anonymous.  *)

END ScrnCursor.

