(* Copyright (C) 1997, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Thu Mar 27 10:20:42 PST 1997 by heydon        *)
(* Last modified on Tue Dec  2 09:29:07 PST 1997 by heydon  *)

(* A BitVector.T is an unbounded vector of Boolean values. There
   is no limit to the size of a bit vector, but the default
   implementation does not represent sparse vectors any more
   efficiently than dense ones. Bit vectors are useful for
   representing sets of small, non-negative integers. *)

INTERFACE BitVector;

IMPORT Word;

CONST Brand = "BitVector-1.0";

TYPE
  T <: Public;
  Public = OBJECT METHODS
    (* initialization / reset all bits *)
    init(sizeHint: CARDINAL := 0; freeMem := FALSE): T;

    (* copy *)
    copy(): T;

    (* size *)
    empty(): BOOLEAN;
    size(): CARDINAL;
    cardinality(): CARDINAL;

    (* reading/writing individual bits *)
    read(i: CARDINAL): BOOLEAN;
    write(i: CARDINAL; val: BOOLEAN): BOOLEAN;
    set(i: CARDINAL): BOOLEAN;
    reset(i: CARDINAL): BOOLEAN;

    (* reading/writing consecutive groups of bits *)
    readSub(start, len: CARDINAL): Word.T;
    writeSub(start, len: CARDINAL; val: Word.T);
    writeInterval(lo, hi: CARDINAL; val: BOOLEAN);
    setInterval(lo, hi: CARDINAL);
    resetInterval(lo, hi: CARDINAL);

    (* determine least unset bit *)
    leastUnsetExcept(except: T := NIL; setIt := TRUE): CARDINAL;
    leastUnset(setIt := TRUE): CARDINAL;

    (* destructive bit-wise operations *)
    and(bv: T): T;
    or(bv: T): T;
    xor(bv: T): T;
    minus(bv: T): T;
  END;

TYPE
  Iterator <: PublicIter;
  PublicIter = OBJECT METHODS
    init(bv: T): Iterator;
    reset();
    next(VAR (*OUT*) res: CARDINAL): BOOLEAN;
  END;

PROCEDURE Equal(bv1, bv2: T): BOOLEAN;
(* Return "TRUE" iff "bv1" and "bv2" contain the same collection
   of set bits. *)

PROCEDURE Subset(bv1, bv2: T): BOOLEAN;
(* Return "TRUE" iff the collection of set bits in "bv1" is a
   subset of the bits set in "bv2". *)

PROCEDURE ProperSubset(bv1, bv2: T): BOOLEAN;
(* Return "TRUE" iff the collection of set bits in "bv1" is a
   proper subset of the bits set in "bv2". *)

PROCEDURE And(bv1, bv2: T): T;
PROCEDURE Or(bv1, bv2: T): T;
PROCEDURE Xor(bv1, bv2: T): T;
PROCEDURE Minus(bv1, bv2: T): T;
(* Allocate and return a new bit vector that is the bitwise AND, OR,
   exclusive-OR, and difference, respectively, of the bit vectors
   "bv1" and "bv2". *)

PROCEDURE Hash(bv: T): Word.T;
(* Return a hash value for "bv". *)

END BitVector.

(*
\subsection{BitVector.T methods}

First, two definitions. The \emph{size} of a bit vector is one
plus the index of its most significant set bit. The \emph{index}
of a bit within a bit vector is its position in the vector; the
index of the least significant bit is 0.

The expression "NEW(T)" evaluates to a new, empty bit vector.
To create a new bit vector whose expected size is "sizeHint",
use the "init" method: the expression "NEW(T).init(sizeHint)"
evaluates to a new, empty bit vector whose expected size is
"sizeHint". 

The "init" method may also be called on an existing bit vector
to reset all of its bits. The call "bv.init(sizeHint, freeMem)"
resets all of the bits of the bit vector "bv", and allocates
enough memory for it to have an expected size of "sizeHint".
If "freeMem" is false and the bit vector can already store
"sizeHint" bits, no new memory is allocated.

The call "bv.copy()" returns a new (deep) copy of the bit vector
"bv".

The call "bv.empty()" returns "TRUE" if and only if the bit vector
"bv" has no set bits. This method takes time proportional to the
bit vector's size in the worst case.

The call "bv.size()" returns an upper bound on the size of the
bit vector "bv". This method takes constant time. By definition
of the size of a bit vector, all set bits in the bit vector have
indices strictly less than the value returned by the "size" method.

The call "bv.cardinality()" returns the number of set bits in the
bit vector "bv". This method takes time proportional to the
bit vector's size in the worst case.

The call "bv.read(i)" returns "TRUE" if and only if the bit at
index "i" of "bv" is set. This method takes constant time.

The call "bv.write(i, val)" sets the bit at index "i" of "bv" to
the value "val", and returns its original value. The calls
"bv.set(i)" and "bv.reset(i)" are equivalent to the calls
"bv.write(i, TRUE)" and "bv.write(i, FALSE)", respectively. All
three methods take constant time.

The call "bv.readSub(start, len)" returns the "len" bits of the
bit vector "bv" starting at index "start" in the low-order "len"
bits of the result. "len" is required to be at most
"BITSIZE(Word.T)". The call "bv.writeSub(start, len, val)"
writes the low-order "len" bits of "val" into the bit vector
"bv" at position "start". Again, "len" is required to be at most
"BITSIZE(Word.T)". Both the "readSub" and "writeSub" methods
require constant time.

The call "bv.writeInterval(lo, hi, val)" sets all bits of the
bit vector "bv" with indices in the closed interval "[lo, hi]"
to the value "val". The calls "bv.setInterval(lo, hi)" and
"bv.resetInterval(lo, hi)" are equivalent to "bv.writeInterval"
calls passing a "val" or "TRUE" and "FALSE", respectively.
All three of these methods take time proportional to the size of
the interval "[lo, hi]".

The call "bv.leastUnsetExcept(except, setIt)" returns the unset
bit in "bv" with smallest index that is not also set in the bit
vector "except". If "except" is NIL, then it is treated like an
empty bit vector. If "setIt" is true, the bit with the returned
index is also set. The call "bv.leastUnset(setIt)" is equivalent
to "bv.leastUnsetExcept(NIL, setIt)".

The calls "bv1.and(bv2)", "bv1.or(bv2)", "bv1.xor(bv2)", and
"bv1.minus(bv2)" destructively set the bit vector "bv1" to the
bitwise AND, OR, exclusive-OR, and difference, respectively, of
the bit vectors "bv1" and "bv2". *)

(*
\subsection{BitVector.Iterator Methods}

The expression "NEW(Iterator).init(bv)" evaluates to an object
for iterating over the set bits of the bit vector "bv".

If "it" is of type "Iterator", then "it.next(i)" sets "i" to the
index of the next set bit in the iterator's bit vector if one
exists, and returns "TRUE". Otherwise, "i" is unchanged and the
method returns "FALSE". The results of the "next" method are
undefined if the bit vector on which the iterator was
initialized has been modified since the iterator was created. *)

(*
\subsection{Synchronization}

All operations on objects of type "BitVector.T" are unmonitored.
The read-only methods are "size", "read", "readSub", and "hash".
Although the "copy", "empty", "cardinality", "leastUnsetExcept",
and "leastUnset" methods may seem like they are read-only, they
may actually have benevolent side-effects on the bit vector.
Those bit vectors passed as arguments to the "and", "or", "xor",
and "minus" methods are never written. Neither are the bit
vectors passed to the "Subset", "And", "Or", "Xor", and "Minus"
procedures; the "Equal" and "ProperSubset" procedures may have
benevolent side-effects on their arguments. *)

(*
\subsection{Implementing Subtypes}

To implement a subtype of a "BitVector.T", see the "BitVectorRep" 
interface, which reveals the representations of both "BitVector.T"
and "BitVector.Iterator" objects. *)

