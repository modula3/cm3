(* Copyright 1997, Critical Mass, Inc. All Rights Reserved. *)

INTERFACE TabVBT;

IMPORT VBT, Filter, Font, Shadow;

(* A "TabVBT" displays a row of ``tabs'' each one
   connected to a child. When a tab is clicked, the
   child associated with the tab is displayed. *)

TYPE 
  T <: Public;
  Public = Filter.T OBJECT METHODS
    init(READONLY tabs     : ARRAY OF TEXT;
         READONLY children : ARRAY OF VBT.T;
                  fnt      : Font.T   := Font.BuiltIn;
                  shadow   : Shadow.T := NIL         ): T;
  END;
(* The call "init" initializes a "TabVBT" with a
   set of labels and a set of children. It is a 
   checked runtime error if the "NUMBER(children)"
   is not the same as "NUMBER(labels)". *)


PROCEDURE New (READONLY tabs     : ARRAY OF TEXT;
               READONLY contents : ARRAY OF VBT.T;
                        fnt      :  Font.T := Font.BuiltIn;
                        shadow   : Shadow.T := NIL         ): T;
(* == NEW (T).init (tabs, contents, fnt, shadow) *)

END TabVBT.

