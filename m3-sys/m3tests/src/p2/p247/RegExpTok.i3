(* from caltech-parser *)

INTERFACE RegExpTok;

TYPE
  ParseType <: ParseTypePublic;

  ParseTypePublic = OBJECT METHODS
    discard();
    detach(): ParseType;
  END;

END RegExpTok.
