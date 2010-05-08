(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Socket;
IMPORT Atom;
BEGIN
  FileType    := Atom.FromText("Socket");
  Unreachable := Atom.FromText("Socket.Unreachable");
  PortBusy    := Atom.FromText("Socket.PortBusy");
  NoResources := Atom.FromText("Socket.NoResources");
  Refused     := Atom.FromText("Socket.Refused");
  Timeout     := Atom.FromText("Socket.Timeout");
  ConnLost    := Atom.FromText("Socket.ConnLost");
  Unexpected  := Atom.FromText("Socket.Unexpected");
END Socket.
