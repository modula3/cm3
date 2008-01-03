(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:54:53 PST 1995 by kalsow  *)
(*      modified on Tue Jul 13 10:40:26 PDT 1993 by mhb     *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

MODULE #(_ALGNAME_)DataView;

IMPORT ZeusDataView, ZeusPanel;

BEGIN
  ZeusPanel.RegisterView (
    ZeusDataView.New, 
    "#(_ALGNAME_) Data View", 
    "#(_ALGNAME_)"); 
END #(_ALGNAME_)DataView.
