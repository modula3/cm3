(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* ESC error reporting types and utilities. *)

INTERFACE ESCError;

IMPORT Atom;

TYPE
  Severity = {Warning, Error, Info};

  T = OBJECT
    severity: Severity;
    kind: Atom.T;             (* e.g., "deref", "bounds", "post", "pre" *)
    msg: TEXT;
    unitName: Atom.T;         (* containing interface/module *)
    line, col: INTEGER;       (* source position *)
    procName: Atom.T;         (* enclosing procedure, or NIL *)
  END;

PROCEDURE Format(err: T): TEXT;
(* Format an error for display: "unitName(line,col): severity: msg" *)

END ESCError.
