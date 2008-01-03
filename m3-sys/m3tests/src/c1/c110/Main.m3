(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Credit Zalman Stern for this one *)


MODULE foo EXPORTS Main;

TYPE
    A = OBJECT a: INTEGER; METHODS init() RAISES ANY := InitA; END;
    AB = A OBJECT b: INTEGER METHODS init() RAISES ANY := InitAB; END;

PROCEDURE InitA( self: A ) RAISES ANY =
BEGIN
    self.a := LAST( INTEGER );
END InitA;

PROCEDURE InitAB( self: AB ) RAISES ANY =
BEGIN
    self.b := LAST( INTEGER );
    A.init( self );
END InitAB;

VAR
    ab: AB;

BEGIN
    ab := NEW( AB );
    ab.init();
END foo.

