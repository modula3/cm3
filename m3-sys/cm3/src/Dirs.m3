(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Dirs;

IMPORT FS, M3File, OSError, Pathname, Process;
IMPORT Msg, M3Options, M3Path, Wr;

CONST
  ModeVerb = ARRAY M3Options.Mode OF TEXT {
    "building in ", "cleaning ", "shipping from ", "searching from ",
    "computing dependencies in " };

VAR
  built_derived := FALSE;
  to_derived: TEXT := NIL;

PROCEDURE SetUp (target: TEXT) =
  VAR base, parent: TEXT;
  BEGIN
    TRY
      initial := Process.GetWorkingDirectory ();
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to get full directory path: ", initial);
    END;
    base   := Pathname.Last (initial);
    parent := Pathname.Prefix (initial);

    (* figure out where we are... *)

    IF M3Path.IsEqual (base, "src") THEN
      (* we're in the source directory *)
      package    := parent;
      derived    := Pathname.Join (parent, target, NIL);
      source     := initial;
      to_initial := Pathname.Join ("..", "src", NIL);
      to_source  := to_initial;
      to_package := "..";
      to_derived := Pathname.Join ("..", target, NIL);

    ELSIF M3Path.IsEqual (base, target) THEN
      (* we're in the derived directory *)
      package    := parent;
      derived    := initial;
      source     := Pathname.Join (parent, "src", NIL);
      to_initial := ".";
      to_source  := Pathname.Join ("..", "src", NIL);
      to_package := "..";
      to_derived := ".";

    ELSIF M3File.IsDirectory ("src") THEN
      (* we're in the parent directory *)
      package    := initial;
      derived    := Pathname.Join (initial, target, NIL);
      source     := Pathname.Join (initial, "src", NIL);
      to_initial := "..";
      to_source  := Pathname.Join ("..", "src", NIL);
      to_package := "..";
      to_derived := target;

    ELSE
      (* we'll assume we're in the parent (=source) directory *)
      package    := initial;
      derived    := Pathname.Join (initial, target, NIL);
      source     := initial;
      to_initial := "..";
      to_source  := "..";
      to_package := "..";
      to_derived := target;
    END;

    IF NOT M3Path.IsEqual (to_derived, ".") THEN
      (* move to the derived directory *)
      IF NOT M3File.IsDirectory (to_derived) THEN
        built_derived := TRUE;
        MkDir (to_derived);
      END;
      IF M3Options.major_mode = M3Options.Mode.Depend THEN
        Msg.Verbose ("--- ", ModeVerb [M3Options.major_mode], to_derived, 
                     " ---", Wr.EOL);
      ELSE
        Msg.Out ("--- ", ModeVerb [M3Options.major_mode], to_derived, 
                 " ---", Wr.EOL);
      END;
      ChDir (to_derived);
    END;
  END SetUp;

PROCEDURE CleanUp () =
  BEGIN
    IF (to_initial # NIL) THEN ChDir (to_initial); END;
    IF built_derived AND (to_derived # NIL) AND DirIsEmpty (to_derived) THEN
      RmDir (to_derived);
    END;
  END CleanUp;

(*------------------------------------------------------ internal --*)

PROCEDURE ChDir (dir: TEXT) =
  BEGIN
    Msg.Commands ("cd ", dir);
    TRY
      Process.SetWorkingDirectory (dir);
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to move to directory: ", dir);
    END;
  END ChDir;

PROCEDURE MkDir (dir: TEXT) =
  BEGIN
    Msg.Commands ("mkdir ", dir);
    TRY
      FS.CreateDirectory (dir);
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to create directory: ", dir);
    END;
  END MkDir;

PROCEDURE RmDir (dir: TEXT) =
  BEGIN
    Msg.Commands ("rmdir ", dir);
    TRY
      FS.DeleteDirectory (dir);
    EXCEPT OSError.E =>
      (* ignore the failure...*)
      (** Msg.FatalError (ec, "unable to remove directory: ", dir); **)
    END;
  END RmDir;

PROCEDURE DirIsEmpty (dir: TEXT): BOOLEAN =
  VAR iter: FS.Iterator;  name := "";  empty := FALSE;
  BEGIN
    TRY
      iter := FS.Iterate (dir);
      empty := NOT iter.next (name);
      iter.close ();
    EXCEPT OSError.E =>
      empty := FALSE;
    END;
    Msg.Verbose ("is_empty (", dir, ") => ", name);
    RETURN empty;
  END DirIsEmpty;

BEGIN
END Dirs.

