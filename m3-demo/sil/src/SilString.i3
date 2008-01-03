(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 12:58:54 PST 1994 by kalsow    *)

INTERFACE SilString;

IMPORT Point, SilObject, SilFont, SilWindow;

TYPE
  T <: Tx;
  Tx = SilObject.T OBJECT METHODS
    init (READONLY p: Point.T;  body: TEXT;  f: SilFont.T;  w: SilWindow.T): T;
  END;

PROCEDURE SetBBox (t: T;  w: SilWindow.T);

END SilString.


