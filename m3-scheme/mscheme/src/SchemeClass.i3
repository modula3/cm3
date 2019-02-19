(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
INTERFACE SchemeClass;
IMPORT Scheme, SchemeInputPort, Wr, SchemePair;
IMPORT Wx, RefSeq;

REVEAL Scheme.T <: Private;

TYPE
  Private = Scheme.Public OBJECT
    input : SchemeInputPort.T;
    output : Wr.T;
    
    freePairs : SchemePair.T := NIL;
    (* this is a list of free pairs that can be used "freely" by the
       interpreter for recycling memory w/o GC *)

    wx : Wx.T := NIL; (* this is a wx for use by string-append etc *)
    refseq : RefSeq.T := NIL;
  END;

(* recycling routines for cons cells *)
PROCEDURE GetCons(t : Scheme.T) : SchemePair.T;

PROCEDURE ReturnCons(t : Scheme.T; cons : SchemePair.T);

END SchemeClass.
