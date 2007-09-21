GENERIC MODULE VanillaDyadicFilterBank(RT);

PROCEDURE ScaleSqRtTwo (READONLY bank: Pair; ): Pair =
  VAR newBank: Pair;
  BEGIN
    newBank[0, 0] := bank[0, 0].scale(RT.SqRtTwo);
    newBank[0, 1] := bank[0, 1].scale(RT.SqRtTwo);
    newBank[1, 0] := bank[1, 0].scale(RT.SqRtTwo);
    newBank[1, 1] := bank[1, 1].scale(RT.SqRtTwo);
    RETURN newBank;
  END ScaleSqRtTwo;

PROCEDURE Reverse (READONLY bank: Pair; ): Pair =
  VAR revBank: Pair;
  BEGIN
    revBank[0, 0] := bank[0, 0].reverse();
    revBank[0, 1] := bank[0, 1].reverse();
    revBank[1, 0] := bank[1, 0].reverse();
    revBank[1, 1] := bank[1, 1].reverse();
    RETURN revBank;
  END Reverse;


BEGIN
END VanillaDyadicFilterBank.
