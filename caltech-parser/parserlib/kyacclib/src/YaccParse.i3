(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE YaccParse;
IMPORT Rd;
IMPORT RuleList;
IMPORT TokSpec;
IMPORT TextIntTbl;
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(rd: Rd.T; tok: TokSpec.T; name: TEXT): T;
    fmtRules(form: TEXT): TEXT; (* see Rule.Format *)
    fmtTypes(form: TEXT; tokenTypes: BOOLEAN): TEXT;
    getRules(): RuleList.T;
    getCodes(): TextIntTbl.T;
  END;
END YaccParse.
