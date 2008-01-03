(* Copyright (C) 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE WinMsg;

IMPORT WinUser;

PROCEDURE ToText (msg: INTEGER): TEXT;
(* Returns a string describing the Windows message "msg".
   Returns "NIL" if the message is not known *)

CONST (* Trestle extensions to the standard Windows messages *)
  CREATE_OFFSCREEN_VBT = WinUser.WM_USER + 1;
  RESHAPE_VBT          = WinUser.WM_USER + 2;
  DELETE_VBT           = WinUser.WM_USER + 3;
  SYNC_VBT             = WinUser.WM_USER + 4;
  FORGE_VBT            = WinUser.WM_USER + 5;
  ICONIZE_VBT          = WinUser.WM_USER + 6;
  OVERLAP_VBT          = WinUser.WM_USER + 7;
  RETITLE_VBT          = WinUser.WM_USER + 8;
  PAINTBATCH_VBT       = WinUser.WM_USER + 9;

END WinMsg.
