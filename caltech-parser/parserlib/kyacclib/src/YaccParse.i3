(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: YaccParse.i3,v 1.2 2001-09-19 15:31:35 wagner Exp $ *)

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
