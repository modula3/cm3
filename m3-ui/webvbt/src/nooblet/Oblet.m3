(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 18 11:22:20 PST 1996 by najork                   *)
(*      modified on Thu Jan 18 11:00:51 PST 1996 by mhb                      *)

MODULE Oblet;

IMPORT VBT, Split, Filter, Text, TextVBT, HTMLVBTG, HTMLVBTGRep, HTML;

TYPE
  ObletInfo = HTMLVBTG.ObletInfo;

  T = HTMLVBTG.T;

  State = HTMLVBTG.State;

  RigidTextVBT = HTMLVBTG.RigidTextVBT;

  RigidPixmapVBT = HTMLVBTG.RigidPixmapVBT;

REVEAL 
  ObletInfo = HTMLVBTG.PublicObletInfo BRANDED OBJECT 
    filter: Filter.T;
  END;

PROCEDURE DisplayOblet (v: T; vsplit: VBT.T; s: State; oblet: HTML.Oblet) =
  BEGIN
    HTMLVBTG.EnterHMode(v, vsplit);

    VAR alt: TEXT;
    BEGIN
      IF oblet.source = NIL THEN
        alt := "[oblet]"
      ELSE
        alt := "[oblet:<" & oblet.source & ">]"
      END;
      WITH vbt = NEW(RigidTextVBT).init(
                   txt := alt, hmargin := 0.0, halign := 0.5,
                   vmargin := 0.0, valign := 0.0, fnt := s.font,
                   bgFg := s.bgFg) DO
        Split.AddChild(v.hsplit, vbt)
      END
    END
  END DisplayOblet;

BEGIN
END Oblet.
