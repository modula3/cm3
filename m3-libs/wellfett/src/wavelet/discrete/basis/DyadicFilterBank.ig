GENERIC INTERFACE DyadicFilterBank(S);

TYPE T = ARRAY [0 .. 1] OF S.T;

PROCEDURE GetComplement (READONLY x: T): T;

END DyadicFilterBank.
