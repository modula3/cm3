(* Copyright (C) 1996, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar  7 16:16:29 PST 1996 by mhb                      *)

INTERFACE HTMLVBTGRep;

IMPORT RefList, VBT, Split, HTMLVBTG;

TYPE
  HeaderBox <: ROOT;

REVEAL
  HTMLVBTG.Private = HTMLVBTG.Public BRANDED OBJECT
        (* set in init method: *)
        baseURL: TEXT;
        useZippers: BOOLEAN;
        useAlt: BOOLEAN;
        toLoad: RefList.T;
        (* global state; while walking parse tree: *)
        headers: HeaderBox;
        page: Split.T;
        hsplit  : VBT.T;
        ulDepth : INTEGER;
        verbatim: BOOLEAN;        
      END;

END HTMLVBTGRep.
