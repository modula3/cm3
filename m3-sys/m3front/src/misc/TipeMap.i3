(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TipeMap.i3                                            *)
(* Last Modified On Tue Jul  5 14:21:29 PDT 1994 by kalsow     *)

INTERFACE TipeMap;

TYPE
  Op = {
 (* 0*) Stop, Mark, PushPtr, Return,
 (* 4*) Ref, UntracedRef, Proc,
 (* 7*) Real, Longreal, Extended,
 (*10*) Int_Field, Word_Field,
 (*12*) Int_1,  Int_2,  Int_4,  Int_8, 
 (*16*) Word_1, Word_2, Word_4, Word_8,
 (*20*) Set_1,  Set_2,  Set_3,  Set_4,
 (*24*) OpenArray_1, OpenArray_2,
 (*26*) Array_1, Array_2, Array_3, Array_4, Array_5, Array_6, Array_7, Array_8,
 (*34*) Skip_1,  Skip_2,  Skip_3,  Skip_4,  Skip_5,  Skip_6,  Skip_7,  Skip_8,
 (*42*) SkipF_1, SkipF_2, SkipF_3, SkipF_4, SkipF_5, SkipF_6, SkipF_7, SkipF_8,
 (*50*) SkipB_1, SkipB_2, SkipB_3, SkipB_4, SkipB_5, SkipB_6, SkipB_7, SkipB_8
   };

PROCEDURE Start ();
(* begin the construction of a map *)

PROCEDURE Finish (a, b, c, d: TEXT := NIL): INTEGER;
(* finish the map, allocate global constant space for it and emit it.  Returns
   the offset of the generated map in the global constant pool. *)

PROCEDURE Add (offset: INTEGER;  o: Op;  arg: INTEGER);
(* add '(o, arg)' as the description for the bits
   at 'offset' in the current map *)

PROCEDURE GetCursor (): INTEGER;
(* get the current offset in the current map *)

PROCEDURE SetCursor (x: INTEGER);
(* get the current offset in the current map without generating Skips *)

END TipeMap.
