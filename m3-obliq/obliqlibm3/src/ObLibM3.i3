(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Jun 10 21:18:37 1998
 *)

INTERFACE ObLibM3;
IMPORT SynLocation, ObValue, TCP, TextRefTbl, IP;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqlibm3 package. *)

(* ============ "tcp" package ============ *)

  TYPE
    ValConnector =
      ObValue.ValAnything BRANDED "ObLibM3.ValConnector" OBJECT
        conn: TCP.Connector;
      OVERRIDES Is := IsConn; Copy := CopyConn;
      END;

  PROCEDURE IsConn(self: ValConnector; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyConn(self: ValConnector; tbl: ObValue.Tbl;
                     loc: SynLocation.T): ObValue.ValAnything
    RAISES {ObValue.Error};

  TYPE
    ValTCP =
      ObValue.ValAnything BRANDED "ObLibM3.ValTCP" OBJECT
        tcp: TCP.T;
      OVERRIDES Is := IsTCP; Copy := CopyTCP;
      END;

  PROCEDURE IsTCP(self: ValTCP; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyTCP(self: ValTCP; tbl: ObValue.Tbl; 
                    loc: SynLocation.T): ObValue.ValAnything 
    RAISES {ObValue.Error};

PROCEDURE NewConnector(conn: TCP.Connector): ObValue.Val;
PROCEDURE NewTCP(tcp: TCP.T): ObValue.Val;
PROCEDURE FromEndPoint (ep: IP.Endpoint): ObValue.Val;
PROCEDURE FromAddress (addr: IP.Address): ObValue.Val;

(* ============ "dict" package ============ *)

  TYPE
    ValDict =
      ObValue.ValAnything BRANDED "ObLibM3.ValDict" OBJECT
        dict: TextRefTbl.T;
      OVERRIDES Is := IsDict; Copy := CopyDict;
      END;

  PROCEDURE IsDict(self: ValDict; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyDict(self: ValDict; tbl: ObValue.Tbl;
                     loc: SynLocation.T): ObValue.ValAnything
    RAISES {ObValue.Error};

  TYPE
    ValIterator =
      ObValue.ValAnything BRANDED "ObLibM3.ValDictIterator" OBJECT
        iterator: TextRefTbl.Iterator;
      OVERRIDES Is := IsIterator; Copy := CopyIterator;
      END;

  PROCEDURE IsIterator(self: ValIterator; 
                       other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyIterator(self: ValIterator; tbl: ObValue.Tbl;
                         loc: SynLocation.T): ObValue.ValAnything
    RAISES {ObValue.Error};

PROCEDURE NewDict(tbl: TextRefTbl.T): ObValue.Val;
PROCEDURE NewIterator(tbl: TextRefTbl.Iterator): ObValue.Val;

END ObLibM3.
