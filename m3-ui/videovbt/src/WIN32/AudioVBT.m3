(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Sep  7 08:32:44 PDT 1995 by najork                   *)
(*       Created on Thu Sep  7 08:32:38 PDT 1995 by najork                   *)


MODULE AudioVBT;

IMPORT Atom, AtomList, Jva, OSError, VBT;

REVEAL T = Public BRANDED OBJECT OVERRIDES init := Init END;

PROCEDURE Init (<*UNUSED*> t            : T;
                <*UNUSED*> ch           : VBT.T;
                <*UNUSED*> source       : TEXT;
                <*UNUSED*> mute         : BOOLEAN;
                <*UNUSED*> ignoreMapping: BOOLEAN;
                <*UNUSED*> volume       : Jva.Volume): T RAISES {OSError.E} =
  VAR 
    msg := AtomList.List1 (Atom.FromText ("JVideo not supported"));
  BEGIN
    RAISE OSError.E (msg);
  END Init;

PROCEDURE SetMute (<*UNUSED*> t: T; <*UNUSED*> mute: BOOLEAN) =
  BEGIN
  END SetMute;

PROCEDURE SetIgnoreMapping (<*UNUSED*> t: T; <*UNUSED*> ignore: BOOLEAN) =
  BEGIN
  END SetIgnoreMapping;

PROCEDURE SetVolume (<*UNUSED*> t: T; <*UNUSED*> volume: Jva.Volume) =
  BEGIN
  END SetVolume;

BEGIN
END AudioVBT.
