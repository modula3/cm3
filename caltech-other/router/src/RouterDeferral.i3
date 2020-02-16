(* $Id$ *)

INTERFACE RouterDeferral;
FROM EndPointStatus IMPORT Dir;
IMPORT MagLabelList AS LabelList;

TYPE
  T <: Public;

  Public = OBJECT

    exitDirs : SET OF Dir; 
(* directions in which routes should be able to exit from node;
   if exitDir is the empty set, that means that ANY direction is OK; 
   otherwise the router should try very hard to provide exits in all listed
   directions; if it cannot manage any direction, that will be considered
   a failure *)

    labels : LabelList.T;
(* labels (used as unique ID by router modules) *)

  END;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "RouterDeferral";

PROCEDURE FormatExitDirs(d : SET OF Dir) : TEXT;

END RouterDeferral.
