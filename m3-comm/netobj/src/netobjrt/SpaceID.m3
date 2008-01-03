(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SpaceID.m3 *)
(* Last modified on Mon Jul 19 15:04:37 PDT 1993 by wobber *)
(*      modified on Fri Aug  7 15:00:56 PDT 1992 by evers  *)

UNSAFE MODULE SpaceID;

IMPORT Fingerprint, TimeStamp;

VAR myT: T;

PROCEDURE Mine() : T = BEGIN RETURN myT; END Mine;

PROCEDURE ComputeFP() : T =
  VAR ts := TimeStamp.New();
  BEGIN
    RETURN Fingerprint.FromChars(
             LOOPHOLE(ts, ARRAY [0..15] OF CHAR), Fingerprint.OfEmpty);
  END ComputeFP;
   
BEGIN
  myT := ComputeFP();
END SpaceID.
