(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Thu May  4 00:44:23 PDT 2000 by saxe    *)
(*      modified on Wed Jul 10 22:46:41 PDT 1996 by detlefs *)

INTERFACE ContextUndoRec;

IMPORT Clause, AF;

CONST Brand = "ContextUndoRec";

TYPE
  Tag = { Mark, DeleteLiteral, DisposeClause, StartSubProof,
          AddClauseFP, PromoteClause, UnrestrictedNUMatch,
          TightenBounds };

  T = RECORD
    type: Tag;
    c: Clause.T;
    ll, predll: AF.LitList
  END (* RECORD *);

END ContextUndoRec.
