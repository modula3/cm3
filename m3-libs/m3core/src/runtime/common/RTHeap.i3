(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Tue Nov  9 12:17:25 PST 1993 by mcjones    *)
(*|      modified on Tue Jun  8 11:32:19 PDT 1993 by kalsow     *)
(*|      modified on Sun Feb 21 14:28:54 PST 1993 by jdd        *)
(*|      modified on Wed Jul  3 04:15:39 1991 by muller         *)

(* "RTHeap" provides access to the layout of data on the heap.
   \index{heap}
   \index{storage allocator}
   \index{allocator}
*)

(* Each referent on the heap, and the heap data record for each object, is
   represented as a contiguous sequence of ``data bytes''.  Referents and
   data records may also contain other ``non-data'' bytes like headers,
   method suite pointers, or open array shapes.

   See "RTType" for related operations on types. *)

INTERFACE RTHeap;

PROCEDURE GetDataAdr(r: REFANY): ADDRESS;
(* If "r" is a traced reference, returns the address of "r^"'s data
   bytes.  If "r" is a traced object, returns the address of the bytes
   of "r"'s data record.  It is a checked runtime error if "r" is
   "NIL".  Note that the address can subsequently change unless object
   mobility is disabled using "RTCollector". *)

PROCEDURE GetDataSize(r: REFANY): CARDINAL;
(* If "r" is a traced reference, returns the number of "r^"'s data
   bytes.  If "r" is a traced object, returns the number of bytes of
   "r"'s data record.  It is a checked runtime error if "r" is "NIL".
   *)

PROCEDURE GetArrayShape(r: REFANY; VAR s: ARRAY OF INTEGER);
(* If "r" is a traced reference to an open array, returns in "s[0 ..  n-1]"
   the size of each dimension of the n-dimensional open array "r^".  If "s"
   is too large, the extra elements are ignored; if it's too small, the
   extra sizes are discarded.  It is a checked runtime error if "r" is
   "NIL".  If "r" is not a reference to an open array, "s" is unchanged. *)

END RTHeap.
