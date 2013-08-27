(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE UtilsWin32 EXPORTS Utils;

IMPORT Msg, OSError, Fmt, M3File(*, Unix*)(*bootstrap workaround*);

PROCEDURE HardLinkFile (from, to: TEXT) =
  BEGIN
    SymbolicOrHardLink((*Unix.*)(*bootstrap workaround*)link, "", from, to);
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
