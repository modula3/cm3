(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE UtilsPosix EXPORTS Utils;

IMPORT Unix;

PROCEDURE SymbolicLinkFile (from, to: TEXT) =
  BEGIN
    MakeRelative (from, to);
    SymbolicOrHardLink(Unix.symlink, "-s ", from, to);
  END SymbolicLinkFile;

PROCEDURE HardLinkFile (from, to: TEXT) =
  BEGIN
    SymbolicOrHardLink(Unix.link, "", from, to);
  END HardLinkFile;

BEGIN
END UtilsPosix.
