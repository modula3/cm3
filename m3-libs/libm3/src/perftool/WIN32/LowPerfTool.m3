(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last Modified On Wed Mar 23 09:29:43 PST 1994 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)

MODULE LowPerfTool;

IMPORT Text, File, Pipe, Process, OSError, RTParams;

PROCEDURE Start (name: TEXT;  VAR r, w: File.T): BOOLEAN=
  VAR
    p: Process.T;
    stdin, stdout, stderr: File.T;
    r_child, w_child, r_self, w_self: Pipe.T := NIL;
    args: ARRAY [0..-1] OF TEXT;
  BEGIN
    TRY
      Pipe.Open (hr := r_child, hw := w_self);
      Pipe.Open (hr := r_self,  hw := w_child);
      Process.GetStandardFileHandles (stdin, stdout, stderr);
      p := Process.Create (name, args, NIL, NIL, r_child, w_child, stderr);
      r_child.close ();
      w_child.close ();
    EXCEPT OSError.E =>
      TRY (* try closing any open pipes *)
        IF (r_child # NIL) THEN r_child.close (); END;
        IF (w_self # NIL)  THEN w_self.close (); END;
        IF (r_self # NIL)  THEN r_self.close (); END;
        IF (w_child # NIL) THEN w_child.close (); END;
      EXCEPT OSError.E => (* SKIP *)
      END;
      RETURN FALSE;
    END;
    r := r_self;
    w := w_self;
    RETURN TRUE;
  END Start;

PROCEDURE StartAndWait (name: TEXT; VAR r, w: File.T): BOOLEAN=
  VAR buf: ARRAY [0..0] OF File.Byte;
  BEGIN
    IF NOT Start (name, r, w) THEN RETURN FALSE; END;
    TRY
      IF r.read (buf) # 1 THEN RETURN FALSE END;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
    RETURN TRUE;
  END StartAndWait;

PROCEDURE ParamStart (param: TEXT;  VAR r, w: File.T): BOOLEAN=
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN Start (param, r, w);
    ELSE 
      RETURN Start (value, r, w);
    END;
  END ParamStart;

PROCEDURE ParamStartAndWait (param: TEXT;  VAR r, w: File.T): BOOLEAN=
  VAR value := RTParams.Value (param);
  BEGIN
    IF value = NIL THEN
      RETURN FALSE;
    ELSIF Text.Length (value) = 0 THEN
      RETURN StartAndWait (param, r, w);
    ELSE 
      RETURN StartAndWait (value, r, w);
    END;
  END ParamStartAndWait;

BEGIN
END LowPerfTool.
