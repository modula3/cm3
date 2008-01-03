(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Wed Dec 14 15:26:23 PST 1994 by mcjones        *)


INTERFACE HTML;

TYPE T <: REFANY;

CONST Brand = "HTML";

PROCEDURE ToText(t: T): TEXT;

VAR (*CONST*)
  Empty,
  HorizontalRule: T;

PROCEDURE Document(
    title: T := NIL;
    body: T := NIL)
  : T;

PROCEDURE Section(
    level: [1..6] := 1;
    heading: T := NIL;
    body: T := NIL)
  : T;
(* *** heading and body should be arrays *)

PROCEDURE Paragraph(t: T): T;

PROCEDURE Break(t: T): T;

PROCEDURE Text(t: TEXT): T;
PROCEDURE Preformatted(t: TEXT; width := -1): T;
(* *** Should both these escape <>&" ? *)

PROCEDURE Anchor(
    caption: T;
    url: TEXT := NIL;
    name: TEXT := NIL;
    ): T;

TYPE ListStyle = {Unnumbered, Numbered, Menu, Dir};

PROCEDURE List(
    READONLY elts: ARRAY OF T;
    style := ListStyle.Unnumbered)
  : T;

PROCEDURE DefinitionList(
    READONLY terms, defs: ARRAY OF T;
    compact := FALSE)
  : T;

PROCEDURE BlockQuote(t: T): T;

PROCEDURE Address(t: T): T;

PROCEDURE Defined(t: T): T;
PROCEDURE Emphasis(t: T): T;
PROCEDURE Cite(t: T): T;
PROCEDURE Code(t: T): T;
PROCEDURE Kbd(t: T): T;
PROCEDURE Samp(t: T): T;
PROCEDURE Strong(t: T): T;
PROCEDURE Var(t: T): T;

PROCEDURE Bold(t: T): T;
PROCEDURE Italic(t: T): T;
PROCEDURE TT(t: T): T;

PROCEDURE Cat(t1, t2, t3: T := NIL): T;
PROCEDURE CatArray(READONLY a: ARRAY OF T): T;

END HTML.
