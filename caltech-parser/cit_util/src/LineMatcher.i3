(* $Id$ *)

INTERFACE LineMatcher;
IMPORT RegEx, Rd, AtomList;

(* 
   Copyright (c) 2007,  Generation Capital Ltd.  All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>

   LineMatcher is an object-oriented interface for matching lines of
   text in a file.  It works via an internal iterator, a Visitor.
   As long as the Visitor returns TRUE on a visit, it will continue
   visiting.  Once the Visitor returns FALSE, it will stop visiting,
   but it can be resumed by calling execute again.  Once execute returns
   FALSE, no more visiting will happen.

   Default is a default implementation for which an implementor must 
   provide a nextLine method.

   Reader is such an implementation that works directly on Modula-3 
   stream readers.  The reader has the extra feature that if an Rd.Failure
   exception occurs, it will call rdFailure, if the visitor is of the
   right type.  Once rdFailure returns, the LineMatcher will also return
   FALSE as Rd.Failure is assumed to be unrecoverable.  If the Visitor
   is not an RdVisitor, the program will be aborted.
*)

TYPE
  T = OBJECT METHODS
    execute(pat : RegEx.Pattern; visitor : Visitor) : BOOLEAN;
    (* returns TRUE if there may be more to visit *)
  END;

  Visitor = OBJECT METHODS
    visit(line : TEXT) : BOOLEAN;
    (* return FALSE to suspend visiting *)
  END;

  (* ------------ Default implementation ---------------- *)

  Default <: PubDefault;
  
  PubDefault = T OBJECT METHODS
    linesRead() : CARDINAL;

    nextLine(VAR line : TEXT; visitor : Visitor) : BOOLEAN;
    (* override this to provide the next line, returns NIL if no more *)
  END;
  
  (* ------------ implementation for readers --------------- *)

  Reader <: PubReader;

  PubReader = Default OBJECT METHODS
    init(rd : Rd.T) : Reader;
  END;

  RdVisitor = Visitor OBJECT METHODS
    rdFailure(err : AtomList.T); (* called on Rd.Failure *)
  END;
    
CONST Brand = "LineMatcher";

END LineMatcher.
