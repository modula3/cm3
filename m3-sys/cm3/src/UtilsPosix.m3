(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE UtilsPosix EXPORTS Utils;

IMPORT Ctypes, Unix, M3toC, Msg;

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

BEGIN
END UtilsPosix.
