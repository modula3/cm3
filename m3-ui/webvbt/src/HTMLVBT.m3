(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 28 16:07:24 PDT 1995 by mhb                      *)

MODULE HTMLVBT;

IMPORT Filter, HTML, Point, VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init     := Init;

        hotlink  := HotLink;
        ismap    := Ismap;
        isindex  := Isindex;
        form     := Form;
      END;


PROCEDURE Init (v: T; <* UNUSED *> html: HTML.T): T =
  BEGIN
    RETURN Filter.T.init(v, NIL)
  END Init;

PROCEDURE HotLink (<* UNUSED *>          self: T;
                   <* UNUSED *>          url : TEXT;
                   <* UNUSED *> READONLY cd  : VBT.MouseRec) =
  BEGIN
  END HotLink;

PROCEDURE Ismap (<* UNUSED *>          self: T;
                 <* UNUSED *>          url : TEXT;
                 <* UNUSED *> READONLY pt  : Point.T;
                 <* UNUSED *> READONLY cd  : VBT.MouseRec) =
  BEGIN
  END Ismap;

PROCEDURE Isindex (<* UNUSED *> self: T; <* UNUSED *> typein: TEXT) =
  BEGIN
  END Isindex;

PROCEDURE Form (<* UNUSED *> self: T) =
  BEGIN
  END Form;

BEGIN 
END HTMLVBT.
