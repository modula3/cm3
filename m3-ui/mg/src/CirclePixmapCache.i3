(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison *)
(* *)
(* Last modified on Tue Jul 21 20:28:23 PDT 1992 by harrison *)

INTERFACE CirclePixmapCache;

IMPORT Pixmap;

TYPE
  T <: T_Public;
  T_Public = OBJECT
             METHODS
               create (width, height: CARDINAL;
                       border       : CARDINAL   := 0;
                       fill                      := TRUE): Pixmap.T;
               purge ();
             END;

CONST DEFAULT_REMEMBER = 100;

PROCEDURE New (remember := DEFAULT_REMEMBER): T;
(* Return a cache, remembering only the last "remember" referenced circles
   (ellipses). *)

END CirclePixmapCache.

