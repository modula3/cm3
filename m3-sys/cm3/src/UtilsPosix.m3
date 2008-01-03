(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE UtilsPosix EXPORTS Utils;

IMPORT Ctypes, Unix, M3toC, Msg, Pathname, Text;

PROCEDURE LinkFile (from, to: TEXT) =
  VAR s_from, s_to: Ctypes.char_star;
  BEGIN
    Remove (to);
    MakeRelative (from, to);
    Msg.Commands ("link -s ", from, " ", to);
    s_from := M3toC.SharedTtoS (from);
    s_to   := M3toC.SharedTtoS (to);
    EVAL Unix.symlink (s_from, s_to);
    M3toC.FreeSharedS (from, s_from);
    M3toC.FreeSharedS (to, s_to);
  END LinkFile;

PROCEDURE MakeRelative (VAR from: TEXT;  to: TEXT) =
  VAR
    from_arcs, to_arcs: Pathname.Arcs;
    from_len, to_len  : INTEGER;
    from_x, to_x      : TEXT;
    common            : INTEGER;
    new_arcs          : Pathname.Arcs;
  BEGIN
    IF NOT Pathname.Absolute (from) OR NOT Pathname.Absolute (to) THEN
      RETURN;  (* bail out *)
    END;

    TRY
      from_arcs := Pathname.Decompose (from);
      to_arcs   := Pathname.Decompose (to);
    EXCEPT Pathname.Invalid =>
      RETURN;  (* bail out *)
    END;

    from_len := from_arcs.size ();
    to_len := to_arcs.size ();
    common := 0;
    WHILE (common < from_len-1) AND (common < to_len-1) DO
      from_x := from_arcs.get (common);
      to_x := to_arcs.get (common);
      IF    (from_x = NIL) AND (to_x = NIL) THEN  (* they're common *)
      ELSIF (from_x = NIL) OR  (to_x = NIL) THEN  EXIT;  (* they're different *)
      ELSIF NOT Text.Equal (from_x, to_x)   THEN  EXIT;  (* they're different *)
      END;
      INC (common);
    END;
    IF (common <= 0) THEN RETURN END;

    new_arcs := NEW (Pathname.Arcs).init (from_len);
    new_arcs.addhi (NIL); (* make a relative path *)
    FOR i := common TO to_len-2   DO new_arcs.addhi (Pathname.Parent);   END;
    FOR i := common TO from_len-1 DO new_arcs.addhi (from_arcs.get (i)); END;

    TRY
      from_x := Pathname.Compose (new_arcs);
    EXCEPT Pathname.Invalid =>
      RETURN; (* bail out *)
    END;

    Msg.Verbose ("\nREWRITE: ",  from,    " -> ",  to,
                 "\n         " & from_x & " -> " & to);
    from := from_x;
  END MakeRelative;

BEGIN
END UtilsPosix.
