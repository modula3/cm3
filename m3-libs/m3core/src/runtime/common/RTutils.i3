(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Mon Jul 24 10:19:29 PDT 1995 by detlefs    *)
(*|      modified on Fri Nov 18 17:32:30 PST 1994 by kalsow     *)
(*|      modified on Sun Feb 21 14:18:42 PST 1993 by jdd        *)
(*|      modified on Tue Jun 16 10:41:17 PDT 1992 by muller     *)
(*|      modified on Mon Jun  8 11:25:23 PDT 1992 by meehan     *)

(* "RTutils" provides information on the heap.  This interface is subject
   to change without notice. *)

INTERFACE RTutils;

TYPE
  HeapPresentation = { ByTypecode, ByNumber, ByByteCount };

PROCEDURE Heap (suppressZeros := FALSE;
                presentation := HeapPresentation.ByTypecode;
                byTypeHierarchy := FALSE;
                window := LAST(INTEGER));
(* This prints a table of all the known types and some simple statistics
   about them (count, total size in bytes for all such objects, average
   size).  If "suppressZeros" is true, then nothing will be printed for a
   type that has no instances in the heap.  The "presentation"
   argument controls the order in which types appear: "ByTypeCode"
   indicates that types are printed in ascending typecode order,
   "ByNumber" indicates they are presented in order of decreasing
   count, and "ByByteCount" indicates that they are printed in order
   of decreasing space usage.  The "byTypeHierarchy" controls whether
   a separate report detailing the contents of the heap by the subtype
   relation is printed.  That is, if object types "B" and "C" are (the
   only) subtypes of "A", this report would include a line for "A"
   aggregating the numbers for "B" and "C".  The "window" arguments
   limits how many types are printed.  The table is printed on stderr
   using RTIO. *)

PROCEDURE NewHeap (suppressZeros := TRUE;
                   presentation := HeapPresentation.ByTypecode;
                   byTypeHierarchy := FALSE;
                   window := LAST(INTEGER));
(* This prints only the incremental information since the last call to Heap
   or NewHeap.  The arguments have the same meaning as in "Heap." *)

END RTutils.
