(* Copyright (C) 1996, Critical Mass, Inc.   All rights reserved.            *)

INTERFACE WinKey;

IMPORT VBT;

PROCEDURE Translate (vk: [0 .. 255]): VBT.KeySym;
(* Translate a Win32 virtual key code to its Trestle representation *)

END WinKey.
