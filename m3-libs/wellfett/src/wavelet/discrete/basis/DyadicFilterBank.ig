GENERIC INTERFACE DyadicFilterBank(S);

TYPE T = ARRAY [0 .. 1] OF S.T;

CONST PrimalToDual = DualToPrimal;

PROCEDURE DualToPrimal (READONLY x: T): T;

END DyadicFilterBank.
