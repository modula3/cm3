(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Mick Jordan                                         *)
(* Last modified on Tue Jun  9 19:36:02 1992 by steveg   *)

(*| TypeSelector.ig and TypeSelector.mg generate a Selector proc
    (see MGV.i3) which iterates over all elements in the current
    MG VBT and finds the element that is a subtype of Type that
    is closest to the selection point.

    Likely Usage:
    
    file Piddly.i3:

    INTERFACE Piddly;
    IMPORT <Some interface with an interesting type>;
    TYPE Type = <interface>.<MG.T subtype>;
    END Piddly.

    file PiddlySelector.i3:

    INTERFACE PiddlySelector = TypeSelector()
    END PiddlySelector.

    file PiddlySelector.m3:

    MODULE PiddlySelector = TypeSelector(Piddly)
    END PiddlySelector.

    zeus view implementation:

    ...
    PROCEDURE Install(view: T) =
    VAR v := 
      NEW(<MGV.V subtype>, selector := PiddlySelector.closest, ...).init(...)
    BEGIN
      View.T.install(view);
      ...
    END Install;
    ...

    m3makefile:

    [Ii]nterface(Piddly)
    [Mm]odule(PiddlySelector)

*)

GENERIC INTERFACE TypeSelector();

IMPORT MGV;

VAR
  closest: MGV.Selector;

END TypeSelector.
