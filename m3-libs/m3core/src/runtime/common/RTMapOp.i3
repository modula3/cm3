(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jun 20 10:52:36 PDT 1994 by kalsow     *)

(* An RTMapOp.T is an instruction of the virtual machine used by
   RTTypeMap and RTHeapMap to locate fields of runtime values.

   An RTTypeMap.T is a pointer to a byte stream that defines a program
   for a simple virtual machine.  By executing that program each
   scalar field of a runtime value can be located.
*)

INTERFACE RTMapOp;

IMPORT RTTypeMap;

TYPE
  T = {
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

TYPE
  Kind = RTTypeMap.Kind;
  Byte = BITS 8 FOR [0..255];
  BP   = UNTRACED REF Byte;
  IP   = UNTRACED REF INTEGER;
  AP   = UNTRACED REF ADDRESS;
  PC   = UNTRACED REF T;

CONST (* type of scalar field identified (Proc ==> none) *)
  OpKind = ARRAY T OF Kind {
    Kind.Proc,         (* Stop *)
    Kind.Proc,         (* Mark *)
    Kind.Proc,         (* PushPtr *)
    Kind.Proc,         (* Return *)
    Kind.Ref,          (* Ref *)
    Kind.UntracedRef,  (* UntracedRef *)
    Kind.Proc,         (* Proc *)
    Kind.Real,         (* Real *)
    Kind.Longreal,     (* Longreal *)
    Kind.Extended,     (* Extended *)
    Kind.Int_Field,    (* Int_Field *)
    Kind.Word_Field,   (* Word_Field *)
    Kind.Int_1,        (* Int_1 *)
    Kind.Int_2,        (* Int_2 *)
    Kind.Int_4,        (* Int_4 *)
    Kind.Int_8,        (* Int_8 *)
    Kind.Word_1,       (* Word_1 *)
    Kind.Word_2,       (* Word_2 *)
    Kind.Word_4,       (* Word_4 *)
    Kind.Word_8,       (* Word_8 *)
    Kind.Set,          (* Set_1 *)
    Kind.Set,          (* Set_2 *)
    Kind.Set,          (* Set_3 *)
    Kind.Set,          (* Set_4 *)
    Kind.Proc,         (* OpenArray_1 *)
    Kind.Proc,         (* OpenArray_2 *)
    Kind.Proc,         (* Array_1 *)
    Kind.Proc,         (* Array_2 *)
    Kind.Proc,         (* Array_3 *)
    Kind.Proc,         (* Array_4 *)
    Kind.Proc,         (* Array_5 *)
    Kind.Proc,         (* Array_6 *)
    Kind.Proc,         (* Array_7 *)
    Kind.Proc,         (* Array_8 *)
    Kind.Proc,         (* Skip_1 *)
    Kind.Proc,         (* Skip_2 *)
    Kind.Proc,         (* Skip_3 *)
    Kind.Proc,         (* Skip_4 *)
    Kind.Proc,         (* Skip_5 *)
    Kind.Proc,         (* Skip_6 *)
    Kind.Proc,         (* Skip_7 *)
    Kind.Proc,         (* Skip_8 *)
    Kind.Proc,         (* SkipF_1 *)
    Kind.Proc,         (* SkipF_2 *)
    Kind.Proc,         (* SkipF_3 *)
    Kind.Proc,         (* SkipF_4 *)
    Kind.Proc,         (* SkipF_5 *)
    Kind.Proc,         (* SkipF_6 *)
    Kind.Proc,         (* SkipF_7 *)
    Kind.Proc,         (* SkipF_8 *)
    Kind.Proc,         (* SkipB_1 *)
    Kind.Proc,         (* SkipB_2 *)
    Kind.Proc,         (* SkipB_3 *)
    Kind.Proc,         (* SkipB_4 *)
    Kind.Proc,         (* SkipB_5 *)
    Kind.Proc,         (* SkipB_6 *)
    Kind.Proc,         (* SkipB_7 *)
    Kind.Proc          (* SkipB_8 *)
  };

(* Note: all operands are little-endian, non-negative integers *)

CONST (* # of bytes of operand in the map *)
  OpArgBytes = ARRAY T OF [0..8] {
    0,            (* Stop *)
    0,            (* Mark *)
    0,            (* PushPtr *)
    0,            (* Return *)
    0,            (* Ref *)
    0,            (* UntracedRef *)
    0,            (* Proc *)
    0,            (* Real *)
    0,            (* Longreal *)
    0,            (* Extended *)
    2,            (* Int_Field      - bit offset, bit size *)
    2,            (* Word_Field     - bit offset, bit size *)
    0,            (* Int_1 *)
    0,            (* Int_2 *)
    0,            (* Int_4 *)
    0,            (* Int_8 *)
    0,            (* Word_1 *)
    0,            (* Word_2 *)
    0,            (* Word_4 *)
    0,            (* Word_8 *)
    1,            (* Set_1         - n = number of bytes *)
    2,            (* Set_2 *)
    3,            (* Set_3 *)
    4,            (* Set_4 *)
    1,            (* OpenArray_1   - n = number of open dimensions *)
    2,            (* OpenArray_2 *)
    1,            (* Array_1       - n = number of elements *)
    2,            (* Array_2 *)
    3,            (* Array_3 *)
    4,            (* Array_4 *)
    5,            (* Array_5 *)
    6,            (* Array_6 *)
    7,            (* Array_7 *)
    8,            (* Array_8 *)
    0,            (* Skip_1 *)
    0,            (* Skip_2 *)
    0,            (* Skip_3 *)
    0,            (* Skip_4 *)
    0,            (* Skip_5 *)
    0,            (* Skip_6 *)
    0,            (* Skip_7 *)
    0,            (* Skip_8 *)
    1,            (* SkipF_1       - n = number of bytes to skip forward *)
    2,            (* SkipF_2 *)
    3,            (* SkipF_3 *)
    4,            (* SkipF_4 *)
    5,            (* SkipF_5 *)
    6,            (* SkipF_6 *)
    7,            (* SkipF_7 *)
    8,            (* SkipF_8 *)
    1,            (* SkipB_1       - n = number of bytes to skip backward *)
    2,            (* SkipB_2 *)
    3,            (* SkipB_3 *)
    4,            (* SkipB_4 *)
    5,            (* SkipB_5 *)
    6,            (* SkipB_6 *)
    7,            (* SkipB_7 *)
    8             (* SkipB_8 *)
  };

CONST (* # of bytes occupied in the referent *)
  OpSize = ARRAY T OF CARDINAL {
    0,                    (* Stop *)
    0,                    (* Mark *)
    0,                    (* PushPtr      -> ADRSIZE (ADDRESS) *)
    0,                    (* Return *)
    ADRSIZE (ADDRESS),    (* Ref *)
    ADRSIZE (ADDRESS),    (* UntracedRef *)
    ADRSIZE (ADDRESS),    (* Proc *)
    ADRSIZE (REAL),       (* Real *)
    ADRSIZE (LONGREAL),   (* Longreal *)
    ADRSIZE (EXTENDED),   (* Extended *)
    0,                    (* Int_Field *)
    0,                    (* Word_Field *)
    1 * ADRSIZE (Byte),   (* Int_1 *)
    2 * ADRSIZE (Byte),   (* Int_2 *)
    4 * ADRSIZE (Byte),   (* Int_4 *)
    8 * ADRSIZE (Byte),   (* Int_8 *)
    1 * ADRSIZE (Byte),   (* Word_1 *)
    2 * ADRSIZE (Byte),   (* Word_2 *)
    4 * ADRSIZE (Byte),   (* Word_4 *)
    8 * ADRSIZE (Byte),   (* Word_8 *)
    0,                    (* Set_1       -> n * Byte *)
    0,                    (* Set_2       -> n * Byte *)
    0,                    (* Set_3       -> n * Byte *)
    0,                    (* Set_4       -> n * Byte *)
    0,                    (* OpenArray_1 -> ADDRESS + n * INTEGER *)
    0,                    (* OpenArray_2 -> ADDRESS + n * INTEGER *)
    0,                    (* Array_1     -> n * element size *)
    0,                    (* Array_2     -> n * element size *)
    0,                    (* Array_3     -> n * element size *)
    0,                    (* Array_4     -> n * element size *)
    0,                    (* Array_5     -> n * element size *)
    0,                    (* Array_6     -> n * element size *)
    0,                    (* Array_7     -> n * element size *)
    0,                    (* Array_8     -> n * element size *)
    1 * ADRSIZE (Byte),   (* Skip_1 *)
    2 * ADRSIZE (Byte),   (* Skip_2 *)
    3 * ADRSIZE (Byte),   (* Skip_3 *)
    4 * ADRSIZE (Byte),   (* Skip_4 *)
    5 * ADRSIZE (Byte),   (* Skip_5 *)
    6 * ADRSIZE (Byte),   (* Skip_6 *)
    7 * ADRSIZE (Byte),   (* Skip_7 *)
    8 * ADRSIZE (Byte),   (* Skip_8 *)
    0,                    (* SkipF_1     -> n * Byte *)
    0,                    (* SkipF_2     -> n * Byte *)
    0,                    (* SkipF_3     -> n * Byte *)
    0,                    (* SkipF_4     -> n * Byte *)
    0,                    (* SkipF_5     -> n * Byte *)
    0,                    (* SkipF_6     -> n * Byte *)
    0,                    (* SkipF_7     -> n * Byte *)
    0,                    (* SkipF_8     -> n * Byte *)
    0,                    (* SkipB_1     -> -n * Byte *)
    0,                    (* SkipB_2     -> -n * Byte *)
    0,                    (* SkipB_3     -> -n * Byte *)
    0,                    (* SkipB_4     -> -n * Byte *)
    0,                    (* SkipB_5     -> -n * Byte *)
    0,                    (* SkipB_6     -> -n * Byte *)
    0,                    (* SkipB_7     -> -n * Byte *)
    0                     (* SkipB_8     -> -n * Byte *)
  };

PROCEDURE GetInt (VAR pc: ADDRESS;  size: [1..8]): INTEGER;
(* Read and return an arbitrarly aligned, non-negative, 'size' byte,
   little-endian integer starting at address 'pc' and increment 'pc'
   by ADRSIZE (INTEGER). *)

TYPE
  StackElt = RECORD pc: ADDRESS;  count: INTEGER END;
  Stack    = RECORD top: INTEGER := 0;  data: ARRAY [0..127] OF StackElt  END;

PROCEDURE Push (VAR s: Stack;  a: ADDRESS;  b: INTEGER);
(* push "(a, b)" on "s". *)

END RTMapOp.

(*---------------------------------------------------------------------------*)

(* The machine's execution on a value at address "x" with an array of opcodes
   at address "pc" is defined by the following psuedo-code.

   The following abbreviations are used:

        z:X   -- LOOPHOLE (z, UNTRACED REF BITS 8*X FOR INTEGER)^
        z:IP  -- LOOPHOLE (z, UNTRACED REF INTEGER)^
        z:AP  -- LOOPHOLE (z, UNTRACED REF ADDRESS)^
        z++T  -- INC (z, ADRSIZE (T))
        z++i  -- INC (z, i * ADRSIZE (BYTE))

   LOOP
     op := pc^;  pc ++ Op;
     CASE op OF
     | Stop =>        stop and return the current value of 'x'.
     | Mark =>        push (pc := pc, count := -1).
     | PushPtr =>     push (pc := x, count := -1);  x := x:AP
     | Return =>      x := pop().pc;  x ++ ADDRESS
     | Ref =>         -- a traced reference is at 'x'    --  x ++ ADDRESS;
     | UntracedRef => -- an untraced reference is at 'x' --  x ++ ADDRESS;
     | Proc =>        -- a procedure is at 'x'           --  x ++ ADDRESS;
     | Real =>        -- a REAL is at 'x'                --  x ++ REAL;
     | Longreal =>    -- a LONGREAL is at 'x'            --  x ++ LONGREAL;
     | Extended =>    -- an EXTENDED is at 'x'           --  x ++ EXTENDED;
     | Int_1 =>       -- a one byte, signed integer      --  x ++ 1
     | Int_2 =>       -- a two byte, signed integer      --  x ++ 2
     | Int_4 =>       -- a four byte, signed integer     --  x ++ 4
     | Int_8 =>       -- an eight byte, signed integer   --  x ++ 8
     | Word_1 =>      -- a one byte, unsigned integer    --  x ++ 1
     | Word_2 =>      -- a two byte, unsigned integer    --  x ++ 2
     | Word_4 =>      -- a four byte, unsigned integer   --  x ++ 4
     | Word_8 =>      -- an eight byte, unsigned integer --  x ++ 8
     | Set_N =>       -- an n=pc:N byte SET is at 'x'    --  x ++ n
     | Skip_N =>      x ++ N;
     | SkipF_N =>     x ++ pc:N ;  pc ++ N;
     | SkipB_N =>     x ++ -(pc:N) ;  pc ++ N;
     | Int_Field =>   n := pc^;  m := (pc+1)^;  pc ++ 2
                      -- an 'm'-bit signed integer begins 'n' bits beyond 'x'--
     | Word_Field =>  n := pc^;  m := (pc+1)^;  pc ++ 2
                      -- an 'm'-bit unsigned int begins 'n' bits beyond 'x'--
     | Array_N =>     WITH z = <top of stack> DO
                        IF z.count = -1 THEN z.count := pc:N END;
                        DEC (z.count);
                        pc ++ N;
                        IF (z.count > 0) THEN  pc := z.pc  ELSE  pop()  END;
                      END;
     | OpenArray_N => VAR n := pc:N;  elts := x:AP;  cnt := 1;
                      BEGIN
                        pc ++ N;   x ++ ADDRESS;
                        FOR j := 1 TO n DO cnt := cnt * x:IP; x ++ INTEGER END;
                        FOR j := 1 TO cnt DO elts := Visit (elts, pc) END;
                        RETURN elts;
                      END;
     END;
   END;

*)

