INTERFACE RandomBayesDurham;
(*Arithmetic for Modula-3, see doc for details

Abstract: Collection of random number generators
Each returns numbers in range:
     RandomBasic.Min..RandomBasic.Max
which is 0..1, but not including the endpoints.

3/16/96  Harry George  Initial version
*)
IMPORT RandomBasic;

(*==========================*)
TYPE
  (*inspired by NR92 ran0*)
  T <: TPublic;

  TPublic = RandomBasic.TReal OBJECT METHODS init (seed:[1..LAST(INTEGER)]:=1): T; END;

(*==========================*)
END RandomBayesDurham.
