(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: LoadSpec.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE LoadSpec;
IMPORT TextTextTbl;
IMPORT TextIntTbl;
IMPORT Rd;
TYPE
  Info = RECORD
    types: TextTextTbl.T; (* name -> containing interface *)
    procs: TextTextTbl.T; (* name -> procedure form *)
    tokMN, methMN, outMN: TEXT;
    kind: CHAR;
    orig, tokOrig: TEXT;
    allocTypes: TextTextTbl.T;(* types that might be allocated in our module *)
    retType: TextTextTbl.T; (* procname -> return type, yacc only *)
    argCount: TextIntTbl.T; (* procname -> num args (default 0) *)
  END;
  
  T <: Public;
  Public = OBJECT
  METHODS
    init(): T;
    setTarget(mn: TEXT);
    readSpec(from: Rd.T; mn: TEXT; kind: CHAR);
    get(): Info;
  END;

END LoadSpec.
