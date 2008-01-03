INTERFACE Term;

EXCEPTION
  SpecialChar;

CONST
  RawSpecial="\004\003\032";

PROCEDURE GetChar(): CHAR;
PROCEDURE GetCharD(): CHAR;  (* Unhandled SpecialChar Death in Raw Mode *)
PROCEDURE GetCharE(special: TEXT): CHAR RAISES {SpecialChar};
PROCEDURE Wr(s: TEXT);
PROCEDURE WrLn(s: TEXT; flush := FALSE);
PROCEDURE MakeRaw(flag: BOOLEAN);

END Term.
