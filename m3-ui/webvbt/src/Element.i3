(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  9 16:35:44 PDT 1996 by mhb                      *)

INTERFACE Element;

CONST
  Brand = "Element";

TYPE
  T = {None, HTML, Head, Title, IsIndex, Base,
       Body, H1, H2, H3, H4, H5, H6, A, P, BR, HR,
       BlockQuote, DL, DT, DD, UL, OL, Menu, Dir, LI,
       Address, Pre, TT, B, I, U,
       Em, Strong, Code, Samp, Kbd, Var, Dfn, Cite,
       Img, Oblet, Table, TR};
CONST
  Strings = ARRAY T OF TEXT {
       "None", "HTML", "Head", "Title", "IsIndex", "Base",
       "Body", "H1", "H2", "H3", "H4", "H5", "H6", "A", "P", "BR", "HR",
       "BlockQuote", "DL", "DT", "DD", "UL", "OL", "Menu", "Dir", "LI",
       "Address", "Pre", "TT", "B", "I", "U",
       "Em", "Strong", "Code", "Samp", "Kbd", "Var", "Dfn", "Cite",
       "Img", "Oblet", "Table", "TR"};

END Element.
