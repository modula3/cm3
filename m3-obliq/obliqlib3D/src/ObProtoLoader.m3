(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 10 16:15:05 PDT 1995 by najork                   *)
(*       Created on Mon May 30 14:28:15 PDT 1994 by najork                   *)


MODULE ObProtoLoader;

IMPORT Bundle, ObLibOnline, ObValue, Obliq, ObliqParser, SynScan, SynWr, 
       SynParse, TextRd;

REVEAL 
  T = Public BRANDED OBJECT
    env    : Obliq.Env;
    parser : SynParse.T;
    bundle : Bundle.T;
  OVERRIDES
    init := Init;
    load := Load;
    get  := Get;
  END;


PROCEDURE Init (self : T; bundle: Bundle.T) : T =
  BEGIN
    self.env    := Obliq.EmptyEnv ();
    self.parser := ObliqParser.New (SynWr.out);
    self.bundle := bundle;

    ObLibOnline.RegisterScanner (self.parser.Scanner ());

    RETURN self;
  END Init;


PROCEDURE Load (self : T; name : TEXT) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH rd = TextRd.New (Bundle.Get (self.bundle, name)) DO
      ObliqParser.ReadFrom (self.parser, "", rd, TRUE, TRUE);
    END;
    LOOP
      TRY
        SynScan.FirstPrompt (self.parser.Scanner());
        WITH phrase = ObliqParser.ParsePhrase (self.parser) DO
          EVAL ObliqParser.EvalPhrase (self.parser, phrase, self.env);
        END;
      EXCEPT
      | ObliqParser.Eof => RETURN;
      END;
    END;
  END Load;


PROCEDURE Get (self : T; qualName : TEXT) : Obliq.Val =
  <* FATAL ObliqParser.Eof, ObValue.Error, ObValue.Exception *>
  BEGIN
    ObliqParser.ReadFrom (self.parser, "", TextRd.New (qualName & ";"), TRUE);
    RETURN Obliq.EvalPhrase (ObliqParser.ParsePhrase (self.parser), self.env);
  END Get;


BEGIN
END ObProtoLoader.
