(* Copyright 1992 by Digital Equipment Corporation.            *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Sun Sep 25 18:48:42 PDT 1994 by heydon     *)
(*      modified on Thu Nov 18 16:14:53 PST 1993 by mcjones    *)
(*      modified on Tue Oct  5 11:35:26 PDT 1993 by gnelson    *)
(*      modified on Thu Aug 19 15:06:11 PDT 1993 by kalsow     *)

(* A "Fingerprint.T" is a 64-bit checksum.  This interface
   provides procedures that can be used to fingerprint
   text strings or more general data structures, such as 
   graphs.
   \index{checksum}

   The interface is based on the original idea of M. O. Rabin
   \cite{Rabin}, as refined by Andrei Broder \cite{Broder}. *)

INTERFACE Fingerprint;

CONST Brand = "Fingerprint";

TYPE T = RECORD
    byte: ARRAY [0..7] OF BITS 8 FOR [0..255]
  END;

PROCEDURE FromText(txt: TEXT): T;
(* Return the fingerprint of "txt". *)

PROCEDURE Combine(READONLY fp1, fp2: T): T;
(* Return the fingerprint of the ordered 
   pair "(fp1, fp2)". *)

CONST Zero = T{ARRAY [0..7] OF BITS 8 FOR [0..255] {0, ..}};

VAR (*CONST*) OfEmpty: T;
(* The fingerprint of the empty text. *)

(* The following procedure, "FromChars", provides two
   additional features.  First, it takes an array of
   characters instead of a "TEXT", which can save
   on allocations.  Second, it can be used to compute
   the fingerprint of a sequence incrementally,
   a buffer at a time, since it accepts the checksum of
   the previous text together with a new buffer full
   of text and computes the checksum of the whole text.
*)

PROCEDURE FromChars
  (READONLY buff: ARRAY OF CHAR; READONLY fp: T): T;
(* Return the fingerprint of "t & Text.FromChars(buff)",
   where "t" is the text whose fingerprint is "fp". *)

(* The last two procedures in the interface allow you to
   use fingerprints as the key type in a generic table. *)

PROCEDURE Equal(READONLY fp1, fp2: T): BOOLEAN;
(* Return "fp1 = fp2".  *)
   
PROCEDURE Hash(READONLY fp: T): INTEGER;
(* Return a hash code for "fp". *)

END Fingerprint.

(* \paragraph{The probabilistic guarantee.}

The fingerprint module produces a provably secure checksum.
To explain exactly what this means requires a few definitions.

Define a {\it nest} to be a text string or an ordered 
pair of two nests.  The fingerprint "FP(x)" of a nest "x" 
is defined as follows:

| FP(x) = FromText(x) `if "x" is a text`
| FP(x) = Combine(FP(y), FP(z)) `if "x" is a pair "(y, z)"`.

Two nests "x" and "y" {\it collide} if "x # y" but 
"FP(x) = FP(y)".  (Two texts are equal if they
are "Text.Equal", and two pairs are equal if their
corresponding components are equal.  We assume that
nests are finite and non-circular.)

A nest "x" is a {\it subnest} of "y" if it occurs anywhere
in "y"; that is, if it equals "y" or if "y" is an ordered
pair and "x" is a subnest of one of "y"'s components.

Define the {\it length} of a nest to be the sum of the
lengths of all the distinct texts that occur anywhere
inside it, and the {\it size} of a nest to be the number 
of distinct subnests that it has. For example, the 
length of the nest

| (("a", "b"), ("a", "b"))

is two, since the only texts that occur inside it are 
"a" and "b", whose lengths sum to two.
The size of the nest is four, since its distinct 
subnests are itself, the pair ("a", "b"), and the
texts "a" and "b".

The fingerprint module contains a magic number that
was chosen on 12 December 1986 by flipping a quarter
128 times in Andrei Broder's office at SRC.  The checksum 
produced by the package is a function of this magic number.

The probabilistic guarantee for the fingerprint algorithm
is that for any nest "S", even one produced by an adversary
who knows everything about the algorithm except the magic
number, the probability that the 1986 coin-flipping produced 
a magic number such that some pair of subnests of "S" collide 
is at most

|    (length(S) * size(S)) / 2^62.

From this basic guarantee you can compute an upper bound
on the probability of a collision in your application.
For example, if two texts "t1" and "t2" collide, then
the nest "(t1,  t2)" contains two colliding subnests.
The odds against this are at least "2^62" to "N * 3",
where "N" is the total length of the two texts.  For
example, if the total length is a million characters,
the collision probability is at most

|    (10^6 * 3) / 2^62

This is less than one in a trillion.  

Similarly, given a thousand texts each of length a thousand, 
considering the linear list of all of them as a nest and
applying the guarantee, we conclude that the probability
that some pair collide is at most

|    (10^6 * 2 * 10^3) / 2^62

which is less than one in "2^31", or less than one in "10^9".

Of course these are probabilities with respect to a random
coin-flipping that has already happened and is therefore not
random anymore.  If you were present in Andrei's office, 
or if you look at the magic number in the implementation, you 
can easily construct a small nest that contains a collision.  The 
probabilistic guarantee is valid only if the structure 
you are fingerprinting is independent of the coin-flipping event.  
For example, it would not really be a good idea to fingerprint 
the text of the module "Fingerprint.m3", since that text contains the
magic number as a constant, and therefore the probabilistic
guarantee says nothing about the quality of its fingerprint.

\paragraph{Example applications.}

Fingerprints are useful in many aspects of computer systems.  For 
example, to determine if two long files stored on different computer 
systems are identical, it is not necessary to transfer the entire 
file from one system to another: it suffices to fingerprint the files 
and transfer and compare the fingerprints.  (Assuming that the
probabilistic guarantee is good enough for your application.)

Fingerprints are also a key technology for achieving type safety in 
distributed programming.  Within a single address space, the compiler 
and linker can ensure that the value of every variable is consistent 
with its type.  In a distributed computation, where values in one 
program are reduced to bit sequences and sent over the network to 
become values of variables in another program, the compiler cannot 
perform this check: whatever the compiler does, a programmer could 
erroneously change the type in one of the programs and recompile and 
execute it.  Some kind of runtime check is required when the
value is transferred.  The simplest check is to send the type 
of the value along with the value itself, and then to check the type 
when the value is received.  But types can be quite complicated in 
modern programming languages, and it would be inefficient to 
communicate types by sending a full description of their structure 
over the wire.  Fingerprints provide the answer: the sending program 
computes a fingerprint of the type, and the receiving program
compares the fingerprint with the fingerprint of the receiving
variable.  Fingerprints play essentially the same role in
making persistent storage typesafe.  The SRC Modula-3 runtime
provides an interface for converting between typecodes and
type fingerprints, for exactly this purpose.

\paragraph{Fingerprinting general data structures.}

The "Combine" function makes it convenient to fingerprint
many data structures.  For example, consider a directed acyclic
graph (DAG) in which each node "nd" has a text label "lbl(nd)" and
"deg(nd)" neighbor nodes "nd[1]", ..., "nd[deg(nd)]".
Such a graph represents an expression in which a
node "nd" of degree zero represents a constant value named
by "lbl(nd)", and a node "nd" of degree greater than
zero represents an expression with root operator
"lbl(nd)" and arguments "nd[1]", ..., "nd[deg(nd)]".

One way to find common subexpressions is to compute
a fingerprint "F(nd)" for every node "nd" by the
following rule:

| PROCEDURE F(nd): T =
|   VAR res := FromText(lbl(nd)); BEGIN
|     FOR i := 1 TO deg(nd) DO
|       res := Combine(res, F(nd[i]))
|     END;
|     RETURN res
|   END F;

(If the DAG is not a tree, the program as written
will recompute the fingerprint of nodes with multiple parents,
possibly many times.  To avoid this, you can easily modify 
the program to record the fingerprint in the node, so that 
the total computation time is proportional to the size of the 
graph.) 

The procedure "F" has the property that with high 
probability, two nodes have the same fingerprint
if and only if they represent common subexpressions. 
This is a consequence of the probabilistic guarantee 
together with the observation that "f(a1, ..., an)" and
"g(b1, ..., bm)" are common subexpressions if and 
only if the nests 

|  ( ... ((f, a1),  a2), ... an)
|  ( ... ((g, b1),  b2), ... bm)

are equal.  

Other data structures, such as cyclic graphs, can be 
fingerprinted with more elaborate strategies based on the 
same idea.  When designing fingerprinting algorithms for
other data structures, it is important to remember that
"Combine" is neither commutative nor associative.

\paragraph{Pitfalls.}

The original fingerprint interface offered at SRC did not include the
procedure "Combine".  The Vesta configuration management project built
a system that cached intermediate results for large software builds.
Abstractly, this is a special case of the common subexpression problem
mentioned previously, and the project used fingerprints as keys in the
cache.  It is instructive to learn what happened.

You might think that a simple way to solve the common subexpression 
problem without "Combine" would be to fingerprint the texts that result from 
printing the expressions represented by the nodes of the DAG.  
But if the DAG is not a tree, this is a serious error, since the 
length of the strings produced by printing a DAG can grow geometrically 
with its size, and therefore the probabilistic 
guarantee becomes useless even for quite small DAGs.  

Avoiding this error, the Vesta group computed the fingerprint
of a node by concatenating the node's label with the {\it fingerprints}
of its children---treating these fingerprints as 8-byte texts---
and fingerprinted the resulting text.  With this strategy, the
number of texts fingerprinted is proportional to the number of
nodes of the DAG, and the total length of these texts is
proportional to the number of edges of the DAG.  Thus the method 
appears efficient and sound.  

Alas, the method is not sound.  Recall that the probabilistic
guarantee is valid only if the strings being 
fingerprinted are independent of the magic number.  But 
fingerprints themselves are dependent on the magic number, so the
probabalistic guarantee is invalid whenever fingerprints
are fingerprinted.  The Vesta group was soon debugging 
an unexpected collision.

The moral is simple: the procedure "Combine" is a convenience, but it 
is also much more than a convenience.  It should be the only way
that you ever generate a fingerprint from another fingerprint.
In particular, never treat a fingerprint as text to be passed to 
"FromText".  *)
