GENERIC INTERFACE DyadicFilterBank(S);

TYPE T = ARRAY [0 .. 1] OF S.T;

PROCEDURE PrimalToDual (READONLY x: T): T;

PROCEDURE DualToPrimal (READONLY x: T): T;

END DyadicFilterBank.
