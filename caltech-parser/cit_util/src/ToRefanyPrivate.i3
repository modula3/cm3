INTERFACE ToRefanyPrivate;
IMPORT RT0;
IMPORT Word;

TYPE
  HashProc = PROCEDURE (a : REFANY) : Word.T;
  EqualProc = PROCEDURE (a, b : REFANY) : BOOLEAN;

  T = RECORD
    typecode : RT0.Typecode;
    hash : HashProc;
    equal : EqualProc;
  END;

PROCEDURE AddType(type : T);

CONST Brand = "RPCobjectPrivate";

END ToRefanyPrivate.
