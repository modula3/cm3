(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Aug  8 14:54:38 PDT 1993 by heydon                   *)

INTERFACE MakePackets;

IMPORT AlgGreedy, Packet, Thread, Rd;

TYPE
  Source = {Random, Constant, FromFile};

PROCEDURE SourceFromName(nm: TEXT): Source;
(* Return the source corresponding to the name "nm", which must be one of
   "randomSrc", "constantSrc", or "pktsFromFile". *)

PROCEDURE RandomPkts(alg: AlgGreedy.T; total: CARDINAL): Packet.Array
    RAISES {Thread.Alerted};

PROCEDURE ConstantPkts(alg: AlgGreedy.T; num: CARDINAL): Packet.Array
    RAISES {Thread.Alerted};

PROCEDURE FromFilePkts(alg: AlgGreedy.T; rd: Rd.T): Packet.Array
    RAISES {Packet.BadPkts, Thread.Alerted};

END MakePackets.
