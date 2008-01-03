(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE FontCache;

IMPORT Font;

PROCEDURE Get (READONLY name: TEXT): Font.T;
(* Returns "Font.FromName (ARRAY OF TEXT {name})", 
   but frequently calls for the same name are expected to be more efficient.
*)

END FontCache.
