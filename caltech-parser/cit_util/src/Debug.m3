(*                                                                           *)
(*  Debug.m3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)

MODULE Debug;
IMPORT Wr;
FROM Stdio IMPORT stderr;
IMPORT Env, Scan;
IMPORT FloatMode, Lex;

EXCEPTION ABORT;

PROCEDURE Out(t: TEXT; minLevel : CARDINAL) =
  BEGIN
    IF minLevel > level THEN RETURN END;

    TRY
      Wr.PutText(stderr, UnNil(t) & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
  END Out;

PROCEDURE S(t: TEXT; minLevel : CARDINAL) = BEGIN Out(t, minLevel); END S;

PROCEDURE Warning(t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(stderr,"WARNING: " & UnNil(t) & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
  END Warning;

PROCEDURE Error(t: TEXT) =
<*FATAL ABORT*>
  BEGIN
    TRY
      Wr.PutText(stderr,"ERROR: " & UnNil(t) & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
    RAISE ABORT;
  END Error;

PROCEDURE UnNil(text : TEXT) : TEXT =
  BEGIN IF text = NIL THEN RETURN "(NIL)" ELSE RETURN text END END UnNil;

PROCEDURE RaiseLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel > level THEN level := newLevel END
  END RaiseLevel;

PROCEDURE LowerLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel < level THEN level := newLevel END
  END LowerLevel;

PROCEDURE SetLevel(newLevel : CARDINAL) = BEGIN level := newLevel END SetLevel;
  
PROCEDURE GetLevel() : CARDINAL = BEGIN RETURN level END GetLevel;

VAR level := 0;

BEGIN 
  VAR
    debugStr := Env.Get("DEBUGLEVEL");
  BEGIN
    TRY
      IF debugStr # NIL THEN level := Scan.Int(debugStr) END
    EXCEPT
      Lex.Error, FloatMode.Trap => 
        Error("DEBUGLEVEL set to nonsense! \"" & debugStr & "\"")
    END
  END
END Debug.
