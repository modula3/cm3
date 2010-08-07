(*--------------------------------------------------------------------------*)
INTERFACE StdDepGraphNode;

CONST Brand = "StdDepGraphNode 0.1";

(*--------------------------------------------------------------------------*)
TYPE 
  T = OBJECT
    mName  : TEXT;
    mCmd   : TEXT;
    mHit   : BOOLEAN;
    mUpd   : BOOLEAN;
    mPhony : BOOLEAN;
  METHODS
    init(n : TEXT; c : TEXT := NIL; pseudo := FALSE) : T := Init;
    reset() := Reset;
    name() : TEXT := Name;
    seen() := Seen;
    touch() := Touch;
    visited() : BOOLEAN := Visited;
    updated() : BOOLEAN := Updated;
    phony() : BOOLEAN := Phony;
    action() : TEXT := Action;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE New(n : TEXT; c : TEXT := NIL; pseudo := FALSE) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; n : TEXT; c : TEXT := NIL; pseudo := FALSE) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE Reset(self : T);
  (* Post: NOT self.visited() AND NOT self.updated() *)

(*--------------------------------------------------------------------------*)
PROCEDURE Touch(self : T);
  (* Post: self.updated() *)

(*--------------------------------------------------------------------------*)
PROCEDURE Seen(self : T);
  (* Post: self.visited() *)

(*--------------------------------------------------------------------------*)
PROCEDURE Name(self : T) : TEXT;
  (* Return the associated file name. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Action(self : T) : TEXT;
  (* Return the associated action. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Visited(self : T) : BOOLEAN;
  (* Self has been visited. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Updated(self : T) : BOOLEAN;
  (* Self has been updated. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Phony(self : T) : BOOLEAN;
  (* Self is phony. *)

(*--------------------------------------------------------------------------*)
(* Hash, Equal and Compare are demanded by the generic DiGraph object type. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Hash (fa : T) : CARDINAL; 

(*--------------------------------------------------------------------------*)
PROCEDURE Equal (fa1, fa2 : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE Compare (fa1, fa2 : T) : [-1..1];

END StdDepGraphNode.


