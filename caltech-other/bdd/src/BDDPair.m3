(* $Id$ *)
MODULE BDDPair;
IMPORT BDD;
IMPORT Word;
(*IMPORT Debug, Fmt;*)

PROCEDURE Equal( a, b : T ) : BOOLEAN = 
  BEGIN RETURN a[0] = b[0] AND a[1] = b[1] END Equal;

PROCEDURE Hash( x : T ) : Word.T =
  VAR
    res : Word.T;
  BEGIN 
    res := Word.Plus(BDD.Hash(x[0]),
                      Word.Rotate(BDD.Hash(x[1]),16));
(*
    Debug.Out(Fmt.Unsigned(res,base:=10));
*)
    RETURN res
  END Hash;

BEGIN END BDDPair.
