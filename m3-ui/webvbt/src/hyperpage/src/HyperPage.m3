(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Jan 12 15:51:53 PST 1995 by kalsow  *)

MODULE HyperPage;


IMPORT Rd, Wr, Text, Thread, VBT, Point, Rect, PaintOp, TextIntTbl;
IMPORT TextWr;

REVEAL
  T = Public BRANDED "HyperPage.T" OBJECT
    title     : TEXT         := NIL;
    lineBreak : CARDINAL     := 0;
    curWidth  : CARDINAL     := 0;
    looks     : Looks        := NIL;
    nChars    : INTEGER      := 0;
    body      : Chars        := NIL;
    nCmds     : INTEGER      := 0;
    cmds      : CmdList      := NIL;
    nLines    : INTEGER      := 0;
    lines     : LineList     := NIL;
    nImages   : INTEGER      := 0;
    images    : ImageList    := NIL;
    nLinks    : INTEGER      := 0;
    links     : IntList      := NIL; (* indicies into cmds *)
    anchors   : TextIntTbl.T := NIL; (* name -> cmd index *)
    hiliteA   : Point.T      := Point.Origin;
    hiliteB   : Point.T      := Point.Origin;
  OVERRIDES
    init              := Init;
    resetWidth        := ResetWidth;
    resetLooks        := ResetLooks;
    size              := Size;
    paint             := Paint;
    hiliteBackground  := HiliteBackground;
    getText           := GetText;
    translate         := Translate;
    getTitle          := GetTitle;
    locateAnchor      := LocateAnchor;
    getLink           := GetLink;
    setLinkLooks      := SetLinkLooks;
    useImage          := UseImage;
    fetchImage        := FetchImage;
    noteError         := NoteError;
  END;

TYPE
  Chars = BRANDED "HyperPage.Chars" REF ARRAY OF CHAR;

TYPE
  CmdList = REF ARRAY OF Cmd;
  Cmd = RECORD
    op    : Op;
    start : INTEGER;
    len   : INTEGER;
    aux   : INTEGER;
  END;

TYPE
  Op = {              (* aux *)
    BreakableText,    (*  text style *)
    UnbreakableText,  (*  text style *)
    Paragraph,        (*  unused *)
    GIFImage,         (*  index into image table *)
    BitmapImage,      (*  index into image table *)
    Link,             (*  ORD (highlight) *)
    Anchor            (*  unused *)
  };

TYPE
  Loc = RECORD
    cmd_index : INTEGER;
    offset    : INTEGER;
  END;

TYPE
  LineList = REF ARRAY OF Line;
  Line = RECORD
    loc   : Loc;
    north : INTEGER;
  END;

TYPE
  IntList = BRANDED "HyperPage.IntList" REF ARRAY OF INTEGER;

TYPE
  ImageList = REF ARRAY OF Image;
  Image = RECORD
    name: TEXT;
    data: REFANY;
  END;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init (t: T;  src: Rd.T;  width: CARDINAL;
                looks: Looks;  delayedImages: BOOLEAN): T =
  VAR 
  BEGIN
    IF (looks = NIL) THEN looks := DefaultLooks (); END;
    t.looks := looks;
    t.lineBreak := MAX (1, width);
    InhaleBody (t, src);
    ParseHTML (t);
    BreakLines (t);
    IF (NOT delayedImages) THEN ResolveImages (t); END;
    RETURN t;
  END Init;

PROCEDURE ResetWidth (t: T;  wid: INTEGER;  READONLY x: Point.T): Point.T =
  VAR loc: Loc;  y: Point.T;
  BEGIN
    IF (wid = t.lineBreak) THEN RETURN x; END;
    PtoL (x, loc);
    t.lineBreak := MAX (wid, 1);
    BreakLines (t);
    LtoP (loc, y);
    RETURN y;
  END ResetWidth;

PROCEDURE ResetLooks (t: T;  looks: Looks;  READONLY x:  Point.T): Point.T =
  VAR loc: Loc;  y: Point.T;
  BEGIN
    PtoL (x, loc);
    t.looks := looks;
    BreakLines (t);
    LtoP (loc, y);
    RETURN y;
  END ResetLooks;

VAR (*const*) defaultLooks: Looks := NIL;

PROCEDURE DefaultLooks (): Looks =
  BEGIN
    IF (defaultLooks = NIL) THEN
      defaultLooks := NEW (Looks,
        fontFamily := "Times",
        fontSize   := 10,
        background := PaintOp.Pair (PaintOp.FromRGB (1.0, 1.0, 1.0),
                                    PaintOp.FromRGB (0.0, 1.0, 0.0)),
        textColor  := PaintOp.Pair (PaintOp.FromRGB (1.0, 0.0, 0.0),
                                    PaintOp.FromRGB (0.0, 0.0, 0.0)),
        linkColor  := PaintOp.Pair (PaintOp.FromRGB (0.7, 0.7, 0.0),
                                    PaintOp.FromRGB (0.0, 0.0, 1.0))
      );
    END;
    RETURN defaultLooks;
  END DefaultLooks;

PROCEDURE InhaleBody (t: T;  src: Rd.T) =
  (* This is a quick-and-dirty way to suck the bits out of a reader.
     Note that it allocates the bytes 3 times: inside TextWr, as a text,
     and finally as t.body. *)
  VAR
    wr  : TextWr.T;
    txt : TEXT;
    len : INTEGER;
    buf : ARRAY [0..1023] OF CHAR;
  BEGIN
    IF (src = NIL) THEN
      t.noteError ("NIL reader");
      t.body := NEW (Chars, 0);
      RETURN;
    END;
    wr := TextWr.New ();
    TRY
      LOOP
        len := Rd.GetSub (src, buf);
        IF (len = 0) THEN EXIT; END;
        Wr.PutString (wr, SUBARRAY (buf, 0, len));
      END;
    EXCEPT
    | Wr.Failure     => <*ASSERT FALSE*> (* TextWr's never raise exceptions *)
    | Rd.Failure     => t.noteError ("Rd.Failure");
    | Thread.Alerted => t.noteError ("Interrupted while reading");
    END;
    txt := TextWr.ToText (wr);
    t.body := NEW (Chars, Text.Length (txt));
    wr := NIL;
    txt := NIL;
  END InhaleBody;

(*---------------------------------------------------------------- images ---*)

PROCEDURE UseImage (t: T;  nm: TEXT;  image: Rd.T) =
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    FOR i := 0 TO t.nImages - 1 DO
      WITH z = t.images [i] DO
        IF (z.data = NIL) AND Text.Equal (nm, z.name) THEN
          z.data := InhaleImage (t, image);
          BreakLines (t);
          RETURN;
        END;
      END;
    END;
  END UseImage;

PROCEDURE FetchImage (<*UNUSED*> t: T;  <*UNUSED*> nm: TEXT): Rd.T =
  BEGIN
    RETURN NIL;
  END FetchImage;

PROCEDURE ResolveImages (t: T) =
  VAR dirty := FALSE;
  BEGIN
    FOR i := 0 TO t.nCmds-1 DO
      WITH z = t.cmds[i] DO
        IF (z.op = Op.GIFImage) OR (z.op = Op.BitmapImage) THEN
          IF ResolveImage (t, z.aux) THEN dirty := TRUE; END;
        END;
      END;
    END;
    IF dirty THEN BreakLines (t); END;
  END ResolveImages;

PROCEDURE ResolveImage (t: T;  imageIndex: INTEGER): BOOLEAN =
  VAR rd: Rd.T;
  BEGIN
    WITH z = t.images [imageIndex] DO
      IF (z.data = NIL) THEN
        rd := t.fetchImage (z.name);
        IF (rd # NIL) THEN
          z.data := InhaleImage (t, rd);
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END ResolveImage;

PROCEDURE InhaleImage (t: T;  src: Rd.T): REFANY =
  BEGIN
    t.noteError ("InhaleImage not implemented");
    RETURN NIL;
  END InhaleImage;

(*--------------------------------------------------------------- parsing ---*)

TYPE
  ParseState = RECORD
    t         : T;
    cur       : INTEGER;
    ch        : CHAR;
    textOp    : Op;
    textStyle : INTEGER;
  END;

PROCEDURE ParseHTML (t: T) =
  VAR s: ParseState;
  BEGIN
    s.t         := t;
    s.cur       := 0;
    s.textOp    := Op.BreakableText;
    s.textStyle := 0;
    WHILE (s.cur < t.nChars) DO
      s.ch := t.body [s.cur];
      ParseChunk (s);
    END;
  END ParseHTML;

PROCEDURE ParseChunk (VAR s: ParseState) =
  VAR start := s.cur;
  BEGIN
    IF (s.ch # '<') THEN
      (* scan a text segment *)
      WHILE (s.cur < s.t.nChars) AND (s.t.body[s.cur] # '<') DO INC (s.cur) END;
      IF (s.cur > start) THEN
        AddCmd (s.t, s.textOp, start, s.cur-start, s.textStyle);
      END;
    ELSE
      (* parse an SGML marker *)
    END;
  END ParseChunk;

PROCEDURE AddCmd (t: T;  op: Op;  start, len, aux: INTEGER) =
  BEGIN
    IF (t.cmds = NIL) OR (t.nCmds >= NUMBER (t.cmds^)) THEN ExpandCmds(t) END;
    WITH z = t.cmds [t.nCmds] DO
      z.op    := op;
      z.start := start;
      z.len   := len;
      z.aux   := aux;
    END;
  END AddCmd;

PROCEDURE ExpandCmds (t: T) =
  VAR new: CmdList;
  BEGIN
    IF (t.cmds = NIL) THEN
      t.cmds := NEW (CmdList, 128);
    ELSE
      new := NEW (CmdList, 2 * t.nCmds);
      SUBARRAY (new^, 0, t.nCmds) := t.cmds^;
      t.cmds := new;
    END;
  END ExpandCmds;

(*--------------------------------------------------------- line breaking ---*)

PROCEDURE BreakLines (t: T) =
  BEGIN
  END BreakLines;

(*----------------------------------------------------------- VBT support ---*)

PROCEDURE Size (t: T): Point.T =
  BEGIN
    IF (t.nLines = 0)
      THEN RETURN Point.Origin;
      ELSE RETURN Point.T { t.curWidth, t.lines [t.nLines].north };
    END;
  END Size;

PROCEDURE Paint (t      : T;
                 dest   : VBT.T;
        READONLY offset : Point.T;
        READONLY clip   : Rect.T) =
  BEGIN
  END Paint;

PROCEDURE HiliteBackground (t: T;  READONLY from, to: Point.T) =
  BEGIN
    t.hiliteA := from;
    t.hiliteB := to;
  END HiliteBackground;

PROCEDURE GetText (t: T;  READONLY from, to: Point.T): TEXT =
  BEGIN
  END GetText;

PROCEDURE Translate (t: T;  READONLY p: Point.T): Location =
  BEGIN
  END Translate;

(*---------------------------------------------------------- HTML support ---*)

PROCEDURE GetTitle (t: T): TEXT =
  BEGIN
    RETURN t.title;
  END GetTitle;

PROCEDURE LocateAnchor (t: T;  nm: TEXT): Point.T =
  CONST Nowhere = Point.T { -1, -1 };
  VAR loc: Loc;  p: Point.T;  i: INTEGER;
  BEGIN
    IF (t.anchors = NIL) OR NOT t.anchors.get (nm, i) THEN RETURN Nowhere; END;
    loc.cmd_index := i;
    loc.offset    := 0;
    LtoP (loc, p);
    RETURN p;
  END LocateAnchor;

PROCEDURE GetLink (t: T;  n: CARDINAL): TEXT =
  BEGIN
    IF (n >= t.nLinks) THEN RETURN NIL; END;
    WITH z = t.cmds [t.links [n]] DO
      RETURN Text.FromChars (SUBARRAY (t.body^, z.start, z.len));
    END;
  END GetLink;

PROCEDURE SetLinkLooks (t: T;  n: CARDINAL;  hilite: BOOLEAN) =
  BEGIN
    IF (n >= t.nLinks) THEN RETURN; END;
    WITH z = t.cmds [t.links [n]] DO
      z.aux := ORD (hilite);
    END;
  END SetLinkLooks;

PROCEDURE NoteError (t: T;  <*UNUSED*> msg: TEXT) =
  BEGIN
    (* ignore the error *)
  END NoteError;

(*-------------------------------------------------------- misc internals ---*)

PROCEDURE PtoL (READONLY p: Point.T;  VAR(*OUT*)l: Loc) =
  BEGIN
  END PtoL;

PROCEDURE LtoP (READONLY l: Loc;  VAR(*OUT*)p: Point.T) =
  BEGIN
  END LtoP;

BEGIN
END HyperPage.
