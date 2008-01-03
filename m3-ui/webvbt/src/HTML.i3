(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  9 16:35:49 PDT 1996 by mhb                      *)

INTERFACE HTML;

IMPORT Rd, TextList, Thread, Wr;

TYPE
  T = OBJECT (* READONLY *)
    title: TEXT := "<untitled>";
    isIndex: BOOLEAN := FALSE;
    base: TEXT := NIL;
    body: Sequence := NIL;
  END;

  Sequence = BRANDED OBJECT next: Sequence; END;

  Word = Sequence BRANDED OBJECT word: TEXT; END;
  Heading = Sequence BRANDED OBJECT
    level: [1..6];
    content: Sequence;
    END;
  Anchor = Sequence BRANDED OBJECT
    href: TEXT;
    content: Sequence;
    END;
  Paragraph = Sequence BRANDED OBJECT END;
  LineBreak = Sequence BRANDED OBJECT END;
  HorizontalRule = Sequence BRANDED OBJECT END;
  Address = Sequence BRANDED OBJECT content: Sequence; END;
  BlockQuote = Sequence BRANDED OBJECT content: Sequence; END;
  Glossary = Sequence BRANDED OBJECT
    compact: BOOLEAN;
    preContent: Sequence;
    content: GlossarySequence;
    END;
  GlossarySequence = BRANDED OBJECT
    term: Sequence;
    definition: Sequence;
    next: GlossarySequence;
    END;
  ListKind = {Unordered, Ordered, Menu, Dir};
  List = Sequence BRANDED OBJECT
    kind: ListKind;
    preContent: Sequence;
    content: ListItem;
    END;
  ListItem = BRANDED OBJECT
    content: Sequence;
    next: ListItem;
    END;
  Preformatted = Sequence BRANDED OBJECT content: Sequence; END;
  Typewriter = Sequence BRANDED OBJECT content: Sequence; END;
  Boldface = Sequence BRANDED OBJECT content: Sequence; END;
  Italic = Sequence BRANDED OBJECT content: Sequence; END;
  Underline = Sequence BRANDED OBJECT content: Sequence; END;
  Emphasis = Sequence BRANDED OBJECT content: Sequence; END;
  Strong = Sequence BRANDED OBJECT content: Sequence; END;
  Code = Sequence BRANDED OBJECT content: Sequence; END;
  Sample = Sequence BRANDED OBJECT content: Sequence; END;
  Keyboard = Sequence BRANDED OBJECT content: Sequence; END;
  Definition = Sequence BRANDED OBJECT content: Sequence; END;
  Variable = Sequence BRANDED OBJECT content: Sequence; END;
  Citation = Sequence BRANDED OBJECT content: Sequence; END;
  Alignment = {Top, Middle, Bottom};
  Image = Sequence BRANDED OBJECT
    source: TEXT;
    align: Alignment;
    alternate: TEXT;
    ismap: BOOLEAN;
    END;
  Oblet = Sequence BRANDED OBJECT
    source: TEXT;
    END;

  Table = Sequence BRANDED OBJECT content: Sequence; END;
  TableRow = Sequence BRANDED OBJECT content: Sequence; END;


PROCEDURE FromRd (rd: Rd.T): T RAISES {Thread.Alerted};
(* Parses the HTML stored in "rd". Does the best it can
   at error recovery, but... *)

PROCEDURE Dump(html: T; wr: Wr.T);
(* Outputs a linearized version of "html" into the "wr" writer. *)

PROCEDURE GetLinks (html:T): TextList.T;
(* Returns a list of the URLs found in "html".  These are either the HREF
   attribute of an anchor or the SRC attribute of an image. *)

END HTML.

