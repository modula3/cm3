GENERIC INTERFACE VectorRep(R, V, VI);
(*Copyright (c) 1996, m3na project *)
FROM NADefinitions IMPORT Error;
(*==========================*)

TYPE T = V.TBody;

PROCEDURE Clear (VAR (*OUT*) z: T);

CONST
  Copy = V.FromArray;

  IsZero = VI.IsZero;
  Equal  = VI.Equal;

  Add = VI.Add;
  Sub = VI.Sub;
  Neg = VI.Neg;

  Scale = VI.Scale;
  Inner = VI.Inner;

  Sum = VI.Sum;

  ArithSeq = VI.ArithSeq;
  GeomSeq  = VI.GeomSeq;

PROCEDURE Apply (READONLY x: T; f: V.ApplyFtn) RAISES {Error};
PROCEDURE Map (READONLY x: T; f: V.MapFtn): V.T RAISES {Error};
PROCEDURE Reduce (READONLY x: T; f: V.ReduceFtn; accu: R.T): R.T
  RAISES {Error};

(*==========================*)
END VectorRep.
