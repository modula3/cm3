(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 18:30:53 PDT 1994 by najork                   *)
(*       Created on Tue Jul 26 18:38:56 PDT 1994 by najork                   *)


INTERFACE ObAux;

IMPORT ObCommand, ObValue;

PROCEDURE ErrorToText (packet : ObValue.ErrorPacket) : TEXT;
PROCEDURE ExceptionToText (packet: ObValue.ExceptionPacket) : TEXT;

PROCEDURE Help (self    : ObCommand.T; 
                arg     : TEXT; 
                pkgname : TEXT; 
                m3name  : TEXT := NIL);

END ObAux.
