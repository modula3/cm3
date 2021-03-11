(* $Id$ *)

GENERIC INTERFACE Fifo(Elem);

(* Copyright (c) 2005-2006, Generation Capital Ltd.  All rights reserved. *)
(* Author: Mika Nystrom <mika@gcapltd.com> *)

(* fifo of Elem.T; 
   Elem must export
   
   Equal(a, b : Elem.T) : BOOLEAN;
   where the mode of a, b can be VALUE, VAR, or READONLY.

   Equal is only called by member, so if you don't use the member
   method, you can use a declaration like 
   
   CONST Equal : PROCEDURE (a, b : Elem.T) : BOOLEAN = NIL;

   and a text constant
   Elem.Brand
*)

(* ALL METHODS ARE UNMONITORED, You have to lock things yourself! *)

TYPE 
  T <: Public;

  Public = MUTEX OBJECT METHODS
    init() : T;
    (* must be called on a new fifo; can be called to re-initialize
       an existing fifo, too (empty it) *)

    put(t : Elem.T);
    (* add new element to fifo *)

    get() : Elem.T;
    (* delete first-added element from fifo, returning it *)

    peek() : Elem.T;
    (* return first-added element, without deleting it *)

    empty() : BOOLEAN;
    (* return TRUE if fifo empty *)

    member(t : Elem.T) : BOOLEAN;
    (* is t a member of fifo, under equality as def'd by Elem.Equal *)

    size() : CARDINAL;
    (* return # of members of fifo *)

    iterate() : Iterator;
    (* iterate through fifo without changing it *)
  END;

TYPE 
  Iterator <: PubIterator;

  PubIterator = OBJECT METHODS
    next(VAR e : Elem.T) : BOOLEAN;
  END;

CONST Brand = Elem.Brand & " Fifo";

END Fifo.
