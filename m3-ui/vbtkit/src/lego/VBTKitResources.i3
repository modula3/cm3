(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri May 17 08:40:41 PDT 1996 by mhb *)

(* This interface used by widgets in VBTKit to retrieve their
   resources.  Resources are look for using the path consisting
   of the value of the environment variable VBTKITPATH followed
   by the bundle VBTKitBundle. *)

INTERFACE VBTKitResources;

IMPORT Pixmap;

PROCEDURE Get (name: TEXT): TEXT;
(* Return the contents of the resource "name".  It is a runtime
   exception if "name" cannot be read successfully. *)

PROCEDURE GetPixmap (name: TEXT): Pixmap.T;
(* Return the pixmap "name" as a scaled pixmap.  It is a runtime exception
   if "name" is not found or if it is not a valid "ppm(5)" format. *)

END VBTKitResources.

