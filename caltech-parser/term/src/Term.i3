INTERFACE Term;

EXCEPTION
  SpecialChar;

CONST
  RawSpecial="\004\003\032";

PROCEDURE GetChar(): CHAR;
PROCEDURE GetCharD(): CHAR;  (* Unhandled SpecialChar Death in Raw Mode *)
PROCEDURE GetCharE(special: TEXT): CHAR RAISES {SpecialChar};
PROCEDURE Wr(s: TEXT; flush := FALSE);
PROCEDURE WrLn(s: TEXT; flush := FALSE);
PROCEDURE MakeRaw(flag: BOOLEAN);


TYPE
  T <: Public; (* a null terminal *)
  Public = OBJECT
  METHODS
    getChar(): CHAR;
    wr(s: TEXT; ln, flush := FALSE);
  END;

PROCEDURE Default(raw := TRUE): T;
(* stdin & stdout, using the above procedures. *)



END Term.
