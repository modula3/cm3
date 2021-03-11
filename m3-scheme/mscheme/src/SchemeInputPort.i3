(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeInputPort;
IMPORT Rd, SchemeObject, SchemeBoolean, SchemeSymbol;
FROM Scheme IMPORT E;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T; warnText : TEXT := NIL) : T;

    readChar() : SchemeObject.T RAISES { E };
    peekChar() : SchemeObject.T RAISES { E };
    pushChar(ch : INTEGER) : INTEGER;
    popChar() : INTEGER;
    peekCh() : INTEGER RAISES { E } ;
    read() : SchemeObject.T RAISES { E };

    close() : SchemeBoolean.T RAISES { E }; (* Norvig has Scheme.Object *)

    getCh() : INTEGER RAISES { E } ;     (* java style Reader.read *)
                           (* override this if desired, see ChEOF *)
  END;

  (* overriders need to override the methods in 
     SchemeInputPortClass.Private as well *)

CONST ChEOF = -1;

PROCEDURE IsEOF(x : SchemeObject.T) : BOOLEAN;

CONST Brand = "SchemeInputPort";

VAR (* CONST *) EOF : SchemeSymbol.T;

CONST CaseInsensitive = FALSE;
  (* change this to TRUE to make all symbols case insensitive (and forced
     to lower case) *)
    
END SchemeInputPort.
