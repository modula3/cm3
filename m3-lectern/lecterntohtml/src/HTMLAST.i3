(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Thu May 26 10:22:32 PDT 1994 by mcjones        *)


INTERFACE HTML;

TYPE T <: REFANY;

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

PROCEDURE Paragraph(t: T): T;

PROCEDURE Break(t: T): T;

PROCEDURE Text(t: TEXT): T;
PROCEDURE Preformatted(t: TEXT): T;
(* *** Should both these escape <>&" ? *)

PROCEDURE Anchor(
    url: TEXT := NIL;
    name: TEXT := NIL;
    caption: T;
    ): T;

TYPE ListStyle = (Unnumbered, Numbered, Descrptions);

PROCEDURE List(
    READONLY elts: ARRAY OF T
    numbered := FALSE)
  : T;

PROCEDURE DefinitionList(
    READONLY elts: ARRAY OF RECORD term, def: T END): T;

PROCEDURE BlockQuote(t: T): T;

PROCEDURE Address(t: TEXT): T;

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


END HTML.
