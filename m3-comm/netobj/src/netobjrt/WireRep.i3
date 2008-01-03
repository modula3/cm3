(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* WireRep.i3 *)
(* Last modified on Sun Sep 25 18:44:29 PDT 1994 by heydon     *)
(*      modified on Mon Jul 19 14:46:12 PDT 1993 by wobber     *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki     *)

(* The "WireRep" defines the network representation of network objects 
   and provides procedures to generate and manipulate values of this 
   type. *)
   
INTERFACE WireRep;

IMPORT SpaceID, Word;

CONST Brand = "WireRep";

TYPE T = RECORD byte: ARRAY [0..15] OF BITS 8 FOR [0..255]; END;

CONST NullT = T {byte := ARRAY [0..15] OF BITS 8 FOR [0..255] {0, ..}};
CONST SpecialT = T {byte := ARRAY [0..15] OF BITS 8 FOR [0..255]
                         {255, 255, 255, 255, 255, 255, 255, 255, 0, ..}};


(* A "WireRep.T" is a value which identifies a concrete network object. 
   In general, each "T" corresponds to one and only one real object 
   in a network.  Furthermore, each "T" is identifiable as having 
   been generated relative to a specific "SpaceID.T".   However, there 
   are two well-known values which are exceptions to this rule.  The 
   value "NullT" corresponds to the "NIL" network object, and the 
   value "SpecialT" corresponds a {\it special object} which is a 
   distinguished concrete object in every address space.  This special 
   object is private to the implementation of the network object 
   runtime. *)

PROCEDURE New() : T;

(* Generates a new, unique "WireRep.T" value.  "GetSpaceID(New()" is
   equal to "SpaceID.Mine()". *)

PROCEDURE GetSpaceID(t: T) : SpaceID.T;

(* Returns the "SpaceID.T" associated with the argument "WireRep.T". *)


(* the following are for generic tables involving WireRep.T's *)

PROCEDURE Equal(t1, t2: T) : BOOLEAN;
PROCEDURE Hash(t: T) : Word.T;

END WireRep.

