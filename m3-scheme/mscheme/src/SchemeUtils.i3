(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeUtils;
FROM Scheme IMPORT Object, Symbol, Vector, E;
IMPORT Scheme, SchemeInputPort;
IMPORT Wr, Wx;
IMPORT SchemePair;
IMPORT SchemeString;
IMPORT RefSeq, RefPairSeq;

TYPE String = SchemeString.T;

TYPE Pair = SchemePair.T;

(* in JScheme, SchemeUtils is an abstract class but it has no non-static
   methods... see Peter Norvig's comments on this, about "unqualifying"
   names.  It's just a Java hack that's not relevant for Modula-3. *)

PROCEDURE CheckNonNil(x : Object) : Object RAISES { E };
 
PROCEDURE Str(x : Object) : String RAISES { E };

PROCEDURE Sym(x : Object) : Symbol RAISES { E };

PROCEDURE Vec(x : Object) : Vector RAISES { E };

PROCEDURE InPort(x : Object; interp : Scheme.T) : SchemeInputPort.T RAISES { E };

PROCEDURE OutPort(x : Object; interp : Scheme.T) : Wr.T RAISES { E };

PROCEDURE Error(message : TEXT) : Object RAISES { E };

PROCEDURE Warn(message : TEXT) : Object RAISES { E } ;

PROCEDURE First(x : Object) : Object;
PROCEDURE Rest(x : Object) : Object;
PROCEDURE Second(x : Object) : Object;
PROCEDURE Third(x : Object) : Object;
PROCEDURE Fourth(x : Object) : Object;
PROCEDURE Fifth(x : Object) : Object;

PROCEDURE PedanticFirst(x : Object) : Object RAISES { E };
PROCEDURE PedanticRest(x : Object) : Object RAISES { E };

PROCEDURE SetFirst(x, y : Object) : Object RAISES { E };

PROCEDURE SetRest(x, y : Object) : Object RAISES { E };

PROCEDURE List1(x : Object; t : Scheme.T := NIL) : Pair;
PROCEDURE List2(x, y : Object; t : Scheme.T := NIL) : Pair;
PROCEDURE List3(x, y, z : Object; t : Scheme.T := NIL) : Pair;
PROCEDURE List4(x, y, z, t : Object; s : Scheme.T := NIL) : Pair;
PROCEDURE ListStar(x : Object; t : Scheme.T := NIL) : Object;

PROCEDURE MakeList(READONLY a : ARRAY OF Object; t : Scheme.T := NIL) : Pair;

PROCEDURE Cons(a, b : Object; interp : Scheme.T := NIL) : Pair;

PROCEDURE Reverse(x : Object) : Object;

PROCEDURE Equal(x, y : Object; stack : RefPairSeq.T := NIL) : BOOLEAN;

PROCEDURE Eqv(x, y : Object) : BOOLEAN;

PROCEDURE Length(x : Object) : CARDINAL;

PROCEDURE Nth(x : Object; n : CARDINAL) : Object;

PROCEDURE ListToString(chars: Object) : String RAISES { E };

PROCEDURE ListToVector(objs : Object) : Vector RAISES { E };

PROCEDURE Write(x : Object; 
                port : Wr.T; 
                quoted : BOOLEAN;
                interp : Scheme.T := NIL;
                flush := TRUE) : Object 
  RAISES { E };

PROCEDURE VectorToList(x : Object; t : Scheme.T := NIL) : Pair RAISES { E };

PROCEDURE P(msg : TEXT; x : Object) : Object; (* for debugging *)

PROCEDURE Stringify(x : Object) : TEXT RAISES { E };
PROCEDURE StringifyQ(x : Object; quoted : BOOLEAN) : TEXT RAISES { E };
PROCEDURE StringifyB(x : Object; quoted : BOOLEAN; buf : Wx.T; seen : RefSeq.T := NIL) RAISES { E };

PROCEDURE StringifyT(x : Object) : TEXT RAISES { E };

PROCEDURE DebugFormat(x : Object) : TEXT;
  (* for debugging, something not really needed in the Java version since
     everything has a .toString there 

     call this sparingly, as it doesn't print Scheme structures properly
  *)

PROCEDURE SetWarningsAreErrors(to : BOOLEAN);

END SchemeUtils.
