(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ESet.i3                                               *)
(* Last modified on Fri Jun 24 09:20:43 PDT 1994 by kalsow     *)

INTERFACE ESet;

IMPORT M3, Value, Scope, CG;

TYPE
  T = M3.ExSet;

PROCEDURE ParseRaises (): T;
PROCEDURE ParseFails  (existing: T): T;

PROCEDURE Hash (t: T): INTEGER;
PROCEDURE Declare (t: T);
PROCEDURE GetAddress (t: T;  VAR base: CG.Var;  VAR offset: INTEGER);

PROCEDURE RaisesAny (t: T): BOOLEAN;
PROCEDURE RaisesNone (t: T): BOOLEAN;

PROCEDURE NewAny (): T;
PROCEDURE NewEmpty (env: Scope.T): T;
PROCEDURE Add (t: T;  READONLY name: M3.QID;   ex: Value.T);

PROCEDURE IsEqual (a, b: T): BOOLEAN;
(* TRUE iff 'a' equals 'b' *)

PROCEDURE IsSubset (a, b: T): BOOLEAN;
(* TRUE iff 'a' is a subset of 'b' *)

(* type checking *)
PROCEDURE TypeCheck (t: T);
PROCEDURE Push (VAR cs: M3.CheckState; ok_to_raise, no_error: T; stop:BOOLEAN);
PROCEDURE Pop  (VAR cs: M3.CheckState; ok_to_raise, no_error: T; stop:BOOLEAN);
PROCEDURE NoteExceptions (VAR cs: M3.CheckState;  t: T);
PROCEDURE NoteException  (VAR cs: M3.CheckState;  v: Value.T);

(* debugging symbols *)
PROCEDURE EmitTypes (t: T): INTEGER;
PROCEDURE EmitNames (t: T);

(* fingerprinting *)
PROCEDURE AddFPTag   (t: T;  VAR x: M3.FPInfo): CARDINAL;
PROCEDURE AddFPEdges (t: T;  VAR x: M3.FPInfo;  n: CARDINAL): CARDINAL;

PROCEDURE Reset ();

END ESet.
