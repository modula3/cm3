(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Type;

IMPORT ID, Node;

TYPE
  T <: Tx; Tx = Node.T OBJECT
    name : ID.T := ID.NoID;
    home : ID.T := ID.NoID;
    uid  : UID  := 0;
    alias: T    := NIL;  (* named types with same UID *)
  END;

TYPE
  UID = INTEGER; (* Int32 would be good enough... *)

CONST
  UnknownKind = '\000';

TYPE
  Info = REF RECORD
    uid         : UID        := 0;
    home        : ID.T       := ID.NoID;
    kind        : CHAR       := UnknownKind;
    info_file   : TEXT       := NIL;
    info_offset : INTEGER    := 0;
    names       : T          := NIL;
  END;

TYPE
  ObjectInfo = REF RECORD
    opaque    : UID := NO_UID;
    concrete  : UID := NO_UID;
    supertype : UID := NO_UID;
    subtypes  : UID := NO_UID;
    next_peer : UID := NO_UID;
  END;

CONST
  NO_UID      = 0;
  INTEGER_UID = 16_195c2a74;
  REFANY_UID  = 16_1c1c45e6;
  ADDRESS_UID = 16_08402063;
  ROOT_UID    = 16_9d8fb489;
  UNROOT_UID  = 16_898ea789;
  NULL_UID    = 16_48ec756e;

CONST
  BuiltinName = "<Modula-3 builtins>";
  BuiltinInfo =
      "$ 0\n"
    & "@**PREDEFINED**\n"
    & "BM3_BUILTIN\n"
    & "?195c2a74 INTEGER\n"
    & "?97e237e2 CARDINAL\n"
    & "?1e59237d BOOLEAN\n"
    & "?08402063 ADDRESS\n"
    & "?56e16863 CHAR\n"
    & "?48e16572 REAL\n"
    & "?94fe32f6 LONGREAL\n"
    & "?9ee024e3 EXTENDED\n"
    & "?48ec756e NULL\n"
    & "?1c1c45e6 REFANY\n"
    & "?00000000 VOID\n"
    & "V9d8fb489 00000000 0 0 0 4\n" (* ROOT = OBJECT END *)
    & "E9d8fb489 ROOT\n"
    & "U898ea789 00000000 0 0 0 4\n" (* UNTRACED ROOT = UNTRACED OBJECT END *)
    & "E898ea789 UNTRACED-ROOT\n"
    & "Y50f86574 1c1c45e6\n" (* TEXT <: REFANY *)
    & "E50f86574 TEXT\n"
    & "Y1541f475 9d8fb489\n" (* MUTEX <: ROOT *)
    & "E1541f475 MUTEX\n"
    ;

PROCEDURE Init ();

END Type.
