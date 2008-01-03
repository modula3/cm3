(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 16:50:21 PST 1994 by isard      *)

INTERFACE M3LoaderUnsafeWin32;

IMPORT WinDef;

PROCEDURE get_dll_addr_from_ordinal (handle: WinDef.HINSTANCE;
                                     ordinal: INTEGER): INTEGER;
PROCEDURE get_dll_addr_from_name (handle: WinDef.HINSTANCE;
                                  name: TEXT): INTEGER;
PROCEDURE load_dll (name: TEXT): WinDef.HINSTANCE;

END M3LoaderUnsafeWin32.
