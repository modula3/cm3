(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TokSpec.i3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

INTERFACE TokSpec;
IMPORT Rd;
IMPORT CharRange;
IMPORT TextList;
IMPORT TextIntTbl;
IMPORT IntTextTbl;
TYPE
  T <: Public;
  Public = OBJECT
    tokens: TextList.T;         (* all tokens but characters *)
    charTokens: CharRange.T;    (* characters *)
    varTokens: TextList.T;      (* non-constant tokens *)

    (* %char and %const *)
    constTokens: TextIntTbl.T;  (* name -> code *)
    constTokensR: IntTextTbl.T; (* code -> name *)
    lastConstCode: INTEGER;
  METHODS
    init(): T;
    read(from: Rd.T);  (* e.g. reader for a ".t" file *)
    error(rd: Rd.T; message: TEXT); (* default: FileRdErr.E *)
    fmtVar(form: TEXT): TEXT;
    fmtOrig(tokMN: TEXT): TEXT; (* tokMN=NIL means this is the original intf *)
  END;
END TokSpec.
