INTERFACE M3LTypeCodes;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST_AS;
IMPORT M3Context;
IMPORT M3LFingerPrint;


TYPE
  T = RECORD
    types: REF ARRAY OF M3AST_AS.TYPE_SPEC;
    texts: REF ARRAY OF TEXT := NIL;
    fingerprints: REF ARRAY OF M3LFingerPrint.T := NIL;
  END; (* record *)

PROCEDURE Set(
    c: M3Context.T;
    allTypes := FALSE;
    genTexts := TRUE;
    genFingerPrints := TRUE;
    ): T RAISES {};

(* This procedure visits all the units in "c" and computes
   the "tmp_type_code" attribute (see M3AST_TM) for a subset of 
   the "TYPE_SPEC" nodes found in those units. If "allTypes = TRUE", 
   then every "TYPE_SPEC" node is included, otherwise only those
   types on the "UNIT_NORMAL.sm_type_spec_s" sequence, and
   those types reachable from these are included. This
   restricted set corresponds to the set of reference types,
   for which a Modula-3 runtime environment must have
   type codes.

   The result of the procedure is a record containing one, two or three
   arrays, all off the same length. The "types" array contains
   all the (distinct) types for which a value of "tmp_type_code" was 
   computed. Every member of the "types" array represents a different 
   type in the sense defined by the Modula-3 structural equivalance
   rules. The value of the "tmp_type_code" attribute is an index
   in the range "0 .. NUMBER(T.types^)-1"

   The "texts" array is only generated if "genTexts = TRUE".
   The "fingerprints" array is only generated if "genFingerPrints = TRUE".
   A member of the "texts" array, "texts[i], is a canonical textual 
   representation for the "TYPE_SPEC" in "types[i]". It will
   be distinct for each value of "i". The interface "M3LTypeToText"
   documents the format of the generated text. A member of the "fingerprints"
   array, "fingerprints[i]", is a unique fingerprint generated from
   the text "texts[i]". 
*)

END M3LTypeCodes.
