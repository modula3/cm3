(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
INTERFACE Trit;

IMPORT Text;

TYPE
  T = { False, Unknown, True };

CONST
  False = T.False;
  Unknown = T.Unknown;
  True = T.True;

PROCEDURE ToText(v: T): Text.T;

END Trit.

