(* $Id: KeyPair.ig,v 1.2 2008/12/20 11:02:33 mika Exp $ *)

GENERIC INTERFACE KeyPair(Key1, Key2);
(* generic interface used to construct pairs as table keys *)
IMPORT Word;

CONST 
  Brand = "KeyPair(" & Key1.Brand & "," & Key2.Brand & ")";

TYPE
  T = RECORD k1 : Key1.T; k2 : Key2.T; END;

PROCEDURE Equal(READONLY a , b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];
  (* compare by k1 first, then k2 *)

CONST CompareK1K2 = Compare;
  (* compare by k1 first, then k2 *)

PROCEDURE CompareK2K1(READONLY a, b : T) : [-1..1];
  (* compare by k2 first, then k1 *)

PROCEDURE CompareK1(READONLY a, b : T) : [-1..1];
  (* compare by k1 *)

PROCEDURE CompareK2(READONLY a, b : T) : [-1..1];
  (* compare by k2 *)

END KeyPair.
