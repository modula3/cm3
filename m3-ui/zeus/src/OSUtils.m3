(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Sep  8 15:29:42 PDT 1995 by najork                   *)
(*       Created on Fri Sep  8 15:26:28 PDT 1995 by najork                   *)

(* An OS-independent version of the original OSUtils.m3, which lives
   on in Postcard, and much of whose implementation was taken from Ted
   Wobber's FileSys.m3 *)


MODULE OSUtils;

IMPORT Atom, AtomList, FS, OSError;

PROCEDURE Delete (path: TEXT) RAISES {FileError} =
  BEGIN
    TRY
      FS.DeleteFile (path);
    EXCEPT
      OSError.E (e) => 
      RAISE FileError (AtomListToText (e));
    END;
  END Delete;
  
PROCEDURE MakeDir (path: TEXT) RAISES {FileError} =
  BEGIN
    TRY
      FS.CreateDirectory (path);
    EXCEPT
      OSError.E (e) => 
      RAISE FileError (AtomListToText (e));
    END;
  END MakeDir;

PROCEDURE AtomListToText (e: AtomList.T): TEXT =
   VAR
    msg := "";
  BEGIN
    FOR i := 0 TO (AtomList.Length (e) - 1) DO
      msg := msg & Atom.ToText (AtomList.Nth (e, i)) & " ";
    END;
    RETURN msg;
  END AtomListToText;

BEGIN
END OSUtils.
