INTERFACE FingerPrint;

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

(* Fingerprints are based on irreducible polynomials in GF(2**31)
   A full description of the algorithms used can be found in:

   "Probabilistic Algorithms In Finite Fields"
   Michael O. Rabin
   SIAM Journal on Computing, Volum3 9 (1980), Pages 273-280

   Fingerprints by Random Polynomials
   Michael O. Rabin
   Harvard University Technical Report:  TR 15-81

   The fingerprint values that are produced have 31 significant bits.
   Bit 0 (the least significant bit) is always 0.
*)

PROCEDURE Data(addr: ADDRESS;
               lengthInBytes: INTEGER;
               VAR (* out *) h1, h2: INTEGER) RAISES {};

(* DataSingle should only be used when there a "few" (less than 10,000)
   items to be fingerprinted.  In this case, the chance of two data sets
   having the fingerprint is small.  If you have more elements to
   fingerprint or need to be more certain that there are no finger print
   clashes, use the regular Data procedure.
*)
PROCEDURE DataSingle(addr: ADDRESS;
                     lengthInBytes: INTEGER;
                     VAR (* out *) h: INTEGER) RAISES {};

TYPE
  Byte = [0..255];

(* Before the first call on Incremental, h1 and h2 must be 0.
   After each call, h1 and h2 contain the fingerprint value for
   the bytes seen so far.
*)
PROCEDURE Incremental(byte: Byte;
                      VAR (* in/out *) h1, h2: INTEGER) RAISES {};

(* Before the first call on Incremental, h must be 0.
   After each call, h contains the fingerprint value for
   the bytes seen so far.

   IncrementalSingle should only be used when there a "few" (less than 10,000)
   items to be fingerprinted.  In this case, the chance of two data sets
   having the fingerprint is small.  If you have more elements to
   fingerprint or need to be more certain that there are no finger print
   clashes, use the regular Increment procedure.
*)
PROCEDURE IncrementalSingle(byte: Byte;
                            VAR (* in/out *) h: INTEGER) RAISES {};

PROCEDURE Text(t: TEXT; 
               VAR (* out *) h1, h2: INTEGER) RAISES {};

(* TextSingle should only be used when there a "few" (less than 10,000)
   items to be fingerprinted.  In this case, the chance of two data sets
   having the fingerprint is small.  If you have more elements to
   fingerprint or need to be more certain that there are no finger print
   clashes, use the regular Text procedure.
*)
PROCEDURE TextSingle(t: TEXT; 
                     VAR (* out *) h1: INTEGER) RAISES {};

(* The following incremental fingerprinting procedure are included for
   completeness.  
   They all require that the fingerprint values be initialized to 0 before the
   first call.
   For example, the same fingerprint is derived for the following set of calls:

     Text("abced", h1, h2);

   OR

     h1 := 0; h2 := 0;
     Incremental(ORD('a'), h1, h2);
     Incremental(ORD('b'), h1, h2);
     Incremental(ORD('c'), h1, h2);
     Incremental(ORD('d'), h1, h2);
     Incremental(ORD('3'), h1, h2);

   OR

     Text("abc", h1, h2);
     TextIncremental("de", h1, h2);

   ETC...
*)

PROCEDURE DataIncremental(addr: ADDRESS;
                          lengthInBytes: INTEGER;
                          VAR (* out *) h1, h2: INTEGER) RAISES {};

PROCEDURE DataIncrementalSingle(addr: ADDRESS;
                                lengthInBytes: INTEGER;
                                VAR (* out *) h: INTEGER) RAISES {};

PROCEDURE TextIncremental(text: TEXT;
                          VAR (* out *) h1, h2: INTEGER) RAISES {};

PROCEDURE TextIncrementalSingle(text: TEXT;
                                VAR (* out *) h: INTEGER) RAISES {};

END FingerPrint.
