(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.           *)
(* See file COPYRIGHT-CMASS for details.                       *)

INTERFACE Quake;

IMPORT Thread;

EXCEPTION
  Error (TEXT);

TYPE
  CodeStream <: REFANY;
  Machine    <: REFANY;
  IDMap      <: REFANY;
  ID          = INTEGER;
  
CONST
  NoID: ID = 0;

PROCEDURE NewIDMap (str2id : PROCEDURE (READONLY x: ARRAY OF CHAR): ID;
                    txt2id : PROCEDURE (t: TEXT): ID;
                    id2txt : PROCEDURE (i: ID): TEXT                   ): IDMap;

PROCEDURE NewMachine (id_map: IDMap): Machine;

PROCEDURE Run (m: Machine;  source_file: TEXT) RAISES {Error, Thread.Alerted};

PROCEDURE Define (m: Machine;  symbol, value: TEXT) RAISES {Error};

PROCEDURE LookUp (m: Machine;  symbol: TEXT): TEXT RAISES {Error};

PROCEDURE Done (m: Machine) RAISES {Error};

END Quake.
