(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:58:06 PST 1992 by muller   *)
(*      modified on Thu Dec 12  2:27:34 PST 1991 by gnelson  *)
(*      modified on Thu Apr 12 16:45:18 PDT 1990 by steveg   *)
<*PRAGMA LL*>

INTERFACE ScrnPaintOp;

IMPORT TrestleComm, PaintOp;

(* A "ScrnPaintOp.T" is a painting operation that is valid for
   some particular screentype.

   If "op" is a "ScrnPaintOp.T" valid for screentype "st", then "op"
   maps a source pixel "s" and destination pixel "d" to a result pixel
   "op(d, s)".  It will be used in a painting operation that sets "d
   := op(d, s)".  Both "d" and "op(d, s)" have type "st", and "s" either
   has type "st" or "st.bits".  (The type "st.bits" is the screentype
   for one-bit deep sources that can be used with "st".) For example,
   in a copy operation, "s" has type "st", while in painting a bitmap,
   "s" has type "st.bits".
   
   A "ScrnPaintOp.Oracle" is meaningful only as the "op" field of some 
   screentype "st".  It provides methods to generate "ScreenPaintOp.T"s
   that are valid for "st". 
   
   A {\it tint} is a paintop that is independent of "s".  If "op" is
   a tint, we write "op(d)" instead of "op(d, s)".  (Even in the case
   of a tint, the type of "s" must be "st.bits"; otherwise the result
   of applying the tint is undefined.)  *)

(* \subsubsection{Obtaining handles from the oracle}  *)

TYPE 
  Pixel = INTEGER;
  Oracle = Private OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      opaque(pix: Pixel): T 
        RAISES {Failure, TrestleComm.Failure};
      bgfg(bg, fg: T): T 
        RAISES {Failure, TrestleComm.Failure};
      swap(p,q: Pixel): T 
        RAISES {Failure, TrestleComm.Failure};
      transparent(): T 
        RAISES {Failure, TrestleComm.Failure};
      copy(): T 
        RAISES {Failure, TrestleComm.Failure}; 
      builtIn(op: PaintOp.Predefined): T;
    END;
    Private <: ROOT;
  
EXCEPTION Failure; 

(* For a screentype "st", the field "st.op" is an "Oracle" whose methods
   satisfy the following specifications:
   
   The method call 

| op := st.op.opaque(pix)

   sets "op" to a tint such that "op(p) = pix" for any "p". The method call
   
| op := st.op.bgfg(bg, fg)

   sets "op" to a tint such that "op(p, 0) = bg(p)" and "op(p, 1)
   = fg(p)", for any "p", if "bg" and "fg" are tints. The method call 

| op := st.op.swap(p, q)

   sets "op" to a tint such that "op(p)=q", "op(q)=p", and for any
   "x", "op(op(x))=x". The method call

| op := st.op.transparent()

   sets "op" to a tint such that "op(p) = p" for any "p". The method call 

| op := st.op.copy()

   sets "op" to a painting operation such that "op(p, q) = q" for any
   "p" and "q". The method call 

| st.op.builtIn(op)

   returns the operation valid for "st" that corresponds to the
   predefined screen-independent operation "PaintOp.T{op}".

   The exception "Failure" is raised if the screentype cannot provide the 
   requested painting operation. For all the methods, "LL.sup <= VBT.mu". *)
      
TYPE
  PlaneWiseOracle = Oracle OBJECT
    METHODS <* LL.sup <= VBT.mu *>
      planewise(
          READONLY mask: ARRAY OF BOOLEAN;
          op1, op2: T): T
        RAISES {Failure, TrestleComm.Failure};
  END;

(* If a screentype's "op" oracle is a "PlaneWiseOracle" (which you can
   test with "TYPECASE"), then you can use its "planewise" method to
   define painting operations by their effects on each bit position
   of the destination pixel.  Let "p[i]" denote bit "i" of pixel "p".
   Assuming "NUMBER(mask) = st.depth", the method call

| op := st.op.planewise(mask, bitOps)

sets "op" so that for "d" and "s" of screentype "st" and "i" in
"[0..st.depth-1]", 

| IF mask[i] THEN
|    op(d, s)[i] = op1(d[i], s[i])
| ELSE
|    op(d, s)[i] = op2(d[i], s[i])
| END

The method may raise "Failure" if it does not support a particular 
combination of "op1", "op2", and "mask".

The convenience procedure "ConstructPlanewiseOp" can be used to construct a painting operation from an array of boolean functions represented by the
enumeration by "BitOp": *)

TYPE
  BitOp = {
    Zero,           (* 0 *)
    And,            (* dest AND src *)
    NotAnd,         (* (NOT dest) AND src *)
    Src,            (* src *)
    AndNot,         (* dest and (NOT src) *)
    Dest,           (* dest *)
    Xor,            (* dest XOR src *)
    Or,             (* dest OR src *)
    Nor,            (* (NOT dest) AND (NOT src) *)
    Equal,          (* dest XOR (NOT src) *)
    Invert,         (* NOT dest *)
    NotOr,          (* (NOT dest) OR src *)
    NotSrc,         (* NOT src *)
    OrNot,          (* dest OR (NOT src) *)
    Nand,           (* (NOT dest) OR (NOT src) *)
    One};           (* 1 *)

PROCEDURE ConstructPlanewiseOp(
  pwo: PlaneWiseOracle;
  READONLY bitOps: ARRAY OF BitOp): T 
RAISES {Failure, TrestleComm.Failure}; 
<* LL.sup <= VBT.mu *>
(* Return the painting operation that applies "bitOp[i]" to plane "i" of
   the source and destination. *)
   
(* If "NUMBER(bitOps) = st.depth" then "ConstructPlanewiseOp" uses "pwo" to
construct and return an operation "op" such that for "s" and
"d" of screentype "st" and "i" in "[0 .. st.depth-1]", 

|  op(d, s)[i] = bitOps[i](d[i], s[i])

The procedure may raise "Failure" if the screentype does not support
a particular array "bitOps". *)

(* \subsubsection{The handle object} *)

TYPE 
  T <: Public; 
  Public = OBJECT id: INTEGER; pix: Pixel := -1 END;

(* If "p" is a "T", then "p.id" is an identifier whose interpretation
   depends on the screentype.  If "p" was created by a call
   "st.op.opaque(pix)", then "p.pix = pix"; otherwise "p.pix = -1". *)

END ScrnPaintOp.

          
    
