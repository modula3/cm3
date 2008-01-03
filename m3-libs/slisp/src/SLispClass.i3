(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jul 17 21:45:01 PDT 1993 by steveg                   *)

INTERFACE SLispClass;

IMPORT Rd, IntRefTbl, SLisp, Sx, Wr;

(* for suppliers of SLisp builtin functions *)

TYPE
  Atom = REF RECORD
               val                : SLisp.Sexp    := NIL;
               funDefined                         := FALSE;
               macro              : BOOLEAN       := FALSE;
               funFormals, funBody: SLisp.List    := NIL;
               builtin            : SLisp.Builtin := NIL;
             END;

  Binding = REF RECORD
                  symbol: SLisp.Symbol;
                  atom  : Atom;
                END;

  Frame = REF RECORD
                next    : Frame                := NIL;
                size    : INTEGER              := 0;
                table   : REF ARRAY OF Binding := NIL;
                procName: TEXT                 := "";
                endScope                       := FALSE;
              END;

TYPE
  LookupMode = {CreateLocal, CreateGlobal, LookupOnly};

  TPublic =
    SLisp.PublicT OBJECT
      topFrame : Frame;
      frame    : Frame;
      depth                   := 0;
      underEval: SLisp.List;
      evalStack: SLisp.List;
    METHODS
      lookup (s: SLisp.Symbol; create := LookupMode.CreateGlobal): Atom;
      lookupAtom (at: Atom): SLisp.Symbol;
      pushScope  ();
      popScope   ();
    END;

REVEAL
  SLisp.T <: TPublic;

TYPE
  Range = REF RECORD start, end: INTEGER; form: SLisp.Sexp END;

PROCEDURE ReadToTable (rd: SLisp.Reader; table: IntRefTbl.T): SLisp.Sexp
  RAISES {Rd.EndOfFile, Sx.ReadError};
(* Read the SLisp program from "rd" as in SLisp.Read except also enter
   each list s-expression into "table" with a "Range" record for
   its position *)

PROCEDURE SxToText(sx: REFANY): TEXT RAISES {Sx.PrintError};
PROCEDURE SxPrint (wr       : Wr.T;
                   sx       : REFANY;
                   maxDepth : CARDINAL := LAST(CARDINAL);
                   maxLength: CARDINAL := LAST(CARDINAL)  )
  RAISES {Sx.PrintError};
  (* Sx.Print extended to handle all types of REFs *)
END SLispClass.
