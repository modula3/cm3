(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* File: Uutmp.i3                                             *)
(* Last modified on Mon Jan  5 11:47:09 GMT 1998 by rrw       *)
(*      modified on Mon May  2 08:50:25 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 16 15:59:54 1990 by jerome        *)

(* $Id: Uutmp.i3,v 1.4 2008-02-11 12:26:34 jkrell Exp $ *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char_star;

(*** <utmp.h> ***)

(*** getlogin(3) ***)

<*EXTERNAL*> PROCEDURE getlogin (): char_star;


END Uutmp.
