(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE UtilsWin32 EXPORTS Utils;

IMPORT Msg, OSError, Fmt, M3File;

PROCEDURE SymbolicOrHardLink (link: PROCEDURE(name1, name2: const_char_star); s_for_sym, from, to: TEXT) =
  VAR s_from, s_to: Ctypes.char_star;
  BEGIN
    MakeRelative (from, to);
    Msg.Commands ("ln ", s_for_sym, from, " ", to);
    s_from := M3toC.SharedTtoS (from);
    s_to   := M3toC.SharedTtoS (to);
    EVAL link(s_from, s_to);
    M3toC.FreeSharedS (from, s_from);
    M3toC.FreeSharedS (to, s_to);
  END SymbolicOrHardLink;

PROCEDURE HardLinkFile (from, to: TEXT) =
  BEGIN
    SymbolicOrHardLink(Unix.link, "", from, to);
  END HardLinkFile;

PROCEDURE SymbolicLinkFile (from, to: TEXT) =
  VAR equal := FALSE;
  BEGIN
    Msg.Commands ("ln -s ", from, " ", to);

    TRY
      equal := M3File.IsEqual (from, to);
    EXCEPT OSError.E =>
      equal := FALSE;
    END;

    TRY
      IF NOT equal THEN M3File.Copy (from, to); END;
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, Fmt.F ("unable to copy \"%s\" to \"%s\"", from, to));
    END;
  END SymbolicLinkFile;

BEGIN
END UtilsWin32.
