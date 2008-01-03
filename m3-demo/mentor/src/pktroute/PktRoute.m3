(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 16 15:20:45 PDT 1993 by heydon                   *)

MODULE PktRoute EXPORTS Main;

IMPORT PktRouteBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact("Packet Routing",
    Rsrc.BuildPath("$PKTROUTEPATH", PktRouteBundle.Get()))
END PktRoute.
