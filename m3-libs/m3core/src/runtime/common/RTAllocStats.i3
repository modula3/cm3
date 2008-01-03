(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(*| Last modified on Thu May  4 16:25:48 PDT 1995 by kalsow  *)
(*|      modified on Mon Mar  6 23:49:50 PST 1995 by detlefs *)

INTERFACE RTAllocStats;

(* This interface controls the collection of per-call-site
   allocation statistics.  This interface is only useful on
   platforms with a stack walker (ie. DS3100 & ALPHA_OSF),
   on all other platforms these routines are no-ops.

   NOTE: The implementation of this interface uses the "spare"
         fields of REF headers.
*)

FROM RT0 IMPORT Typecode;

VAR siteDepth := 3;
(* The number of procedure frames included in a "site". *)

PROCEDURE EnableTrace (tc: Typecode);
(* Begin recording allocation statistics for type "tc".  It is a
   checked runtime error to pass an improper typecode or one that
   corresponds to an untraced type. *)

PROCEDURE NSites (tc: Typecode): INTEGER;
(* Returns the number of sites recorded for type "tc".  If sites are
   not being recorded for "tc", returns -1. *)

PROCEDURE GetSiteText (tc: Typecode; site, depth: CARDINAL): TEXT;
(* If sites are being traced for type "tc", "site" is a valid tag for
   the type, and "depth" is less than the "siteDepth" when the site
   was captured, return a description of the procedure frame corresponding
   to the captured PC.  Otherwise, return "NIL". *)

END RTAllocStats.

