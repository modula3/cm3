(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 20 11:46:57 PDT 1993 by kalsow     *)
(*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      *)

UNSAFE INTERFACE WinRPC;

(* Subset of WNT rpc.h interface needed by TimeStampWin32.m3. *)

TYPE
  UUID = RECORD
           Data1       : BITS 32 FOR INTEGER;
           Data2, Data3: BITS 16 FOR [0 .. 16_FFFF];
           Data4       : ARRAY [0 .. 7] OF BITS 8 FOR [0 .. 255];
         END;

TYPE
<*EXTERNAL UuidCreate:MSCWIN*>
  PROCEDURE UuidCreate (uuid: UNTRACED REF UUID);

END WinRPC.
