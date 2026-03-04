(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCError;

IMPORT Atom, Fmt;

PROCEDURE Format(err: T): TEXT =
  VAR sev: TEXT;
  BEGIN
    CASE err.severity OF
    | Severity.Warning => sev := "warning";
    | Severity.Error   => sev := "error";
    | Severity.Info    => sev := "info";
    END;
    RETURN Atom.ToText(err.unitName) & "(" & Fmt.Int(err.line) & "," &
           Fmt.Int(err.col) & "): " & sev & ": " & err.msg;
  END Format;

BEGIN
END ESCError.
