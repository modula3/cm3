(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Tue Nov  9 12:28:51 PST 1993 by mcjones    *)
(*|      modified on Mon Feb 22 09:13:09 PST 1993 by jdd        *)
(*|      modified on Thu Jan 28 15:14:28 PST 1993 by kalsow     *)
(*|      modified on Wed Jul  3 04:15:39 1991 by muller         *)

(* "RTCollector" provides control over the Modula-3 garbage collector.
   \index{collector}
   \index{garbage collector!control over}
*)

INTERFACE RTCollector;

(* The purpose of a garbage collector is to reclaim unreachable
   nodes on the traced heap; most Modula-3 programs could not run
   very long without a collector.  Even so, automatic garbage
   collection has some practical drawbacks.

   \begin{enumerate}
   \item

      The collector might move heap nodes to different addresses.  This is
      usually unnoticable to programs, but can cause problems when programs
      must work with the addresses of heap nodes, since it is not
      guaranteed that "ADR(x^)" is a constant over the lifetime of
      "x^".  There are two main cases when programs must work with
      such addresses.

      \begin{enumerate}
      \item

         To implement hash tables, etc.

      \item

         To pass addresses to procedures written in other languages,
         which is inherently unportable.

      \end{enumerate}

   \item

      Unsafe code can put the traced heap temporarily into an
      inconsistent state.  If the collector happens to run then, it
      might delete nodes that seem unreachable but that in fact are
      accessible.  Of course, unsafe code itself is inherently unportable.

   \end{enumerate}

   This interface allows the program to control the Modula-3 collector
   to avoid such problems, as well as to pass hints to improve
   performance. *)

(*
\paragraph*{Disabling the collector.}

   The collector is initially enabled; the collector can reclaim
   storage, and move nodes in memory.  While the collector is
   disabled, there will be no time spent in the collector.  Allocation
   in the traced heap may proceed normally, although the heap will
   grow without bound.  Nodes unreachable by the Modula-3 rules will
   not be reclaimed, and no nodes will move.  *)

PROCEDURE Disable();
(* Disable the collector. *)

PROCEDURE Enable();
(* Reenable the collector if "Enable" has been called as many times as
   "Disable".  It is a checked runtime error to call "Enable" more times
   than "Disable". *)

(*
\paragraph*{Disabling motion.}

   Disabling motion gives fewer guarantees than disabling the
   collector; while motion is disabled, it is guaranteed only that no
   nodes will move.  Disabling motion is no more expensive than
   disabling the entire collector, and may be cheaper in some
   implementations.  *)

PROCEDURE DisableMotion();
(* Disable motion. While motion is disabled, no nodes will
   move.*)

PROCEDURE EnableMotion();
(* Reenable motion if "EnableMotion" has been called as many times as
   "DisableMotion", and "Enable" has been called as many times as
   "Disable".  It is a checked runtime error to call "EnableMotion"
   more times than "DisableMotion". *)

(*
\paragraph*{Collecting.}

   Calling "Collect" is a hint from the program that now would be a
   good time for a collection (for example, if a large amount of
   storage has become unreachable, or if the program expects to wait
   some time for an external event).
*)

PROCEDURE Collect();
(* Maybe collect now. *)

END RTCollector.

(*
\paragraph*{Implementation notes.}

   This section describes the implementation of the SRC Modula-3
   collector, as a guide to SRC Modula-3 programmers and as an
   indication of how this interface is matched to a particular
   implementation.  Portable programs must not take advantage of
   implementation details of the SRC Modula-3 collector.

   The SRC Modula-3 collector is an incremental, generational,
   conservative mostly-copying collector that uses VM protection on
   heap pages to be notified of certain heap accesses.
   \index{garbage collector!properties of}

   Because the SRC collector is conservative, an inaccessible node may
   be considered reachable if a bit-pattern either on a thread's stack
   or in its registers might be a reference to or into the node.
   Experience to date has not shown accidental node retention to be a
   problem.

   The SRC collector will not collect or move a node while any
   thread's stack or registers contains a reference to or into the
   node.  The SRC Modula-3 system guarantees that this will include
   references passed as value parameters.  This guarantee is useful
   for calling foreign procedures.

   "Disable" completes the current incremental collection, if any, and
   unprotects all heap pages, so that no page faults will occur while
   collection is disabled.  No new collections will start while
   collection is disabled.  The next collection after collection is
   reenabled will be total, as opposed to partial, since unprotecting
   the heap loses generational information.

   "DisableMotion" disables further collections from beginning.
   "DisableMotion" does not finish the current incremental collection,
   since the collector already guarantees that the program will not
   see addresses in the previous space.  No new collections will start
   while motion is disabled, so that the current space will not become
   the previous space.  It is not necessary to unprotect the heap.

   "Collect" completes the current incremental collection, if any, then
   performs a total collection before returning to the caller.

   The "@M3nogc" flag performs an initial call to "Disable".
   \index{{\tt \char'100{}M3nogc} command-line flag}

   The SRC collector also supports additional operations for
   controlling the frequency of collection, disabling and reenabling
   incremental and generational collection, reporting on collector
   performance, and so on.  These operations are accessible through the
   implementation-dependent "RTCollectorSRC" interface.
   \index{RTCollectorSRC interface@{\tt RTCollectorSRC} interface}
*)
