
INTERFACE Postgres;

IMPORT Word;

CONST 
  NAMEDATALEN = 16;

TYPE
  Oid = Word.T;

(* => select typname,typelem from pg_type 
   reveals some information about the types. *)
  
CONST

  Bool        : Word.T = 16;
  Bytea       : Word.T = 17;
  Char        : Word.T =  18;
  Char16      : Word.T =  19;
  Char2       : Word.T = 409;
  Char4       : Word.T = 410; 
  Char8       : Word.T = 411;

(* 

  DT??        : Word.T = 16_014;
  Int2        : Word.T = 16_015;
  Int28       : Word.T = 16_016;
  Int4        : Word.T = 16_017;

*)

  SmallInt    : Word.T = 16_015;
  Int         : Word.T = 16_017;
  Text        : Word.T = 16_019;
  VarChar     : Word.T = 16_413;
(*  Char        : Word.T = 16_412; Conflict? *)
  Real        : Word.T = 16_2bc;
  Float       : Word.T = 16_2bc;
  Date        : Word.T = 16_43a;
  Time        : Word.T = 16_43b;
  Timestamp   : Word.T = 702;
  Bpchar      : Word.T = 1042;

END Postgres.
