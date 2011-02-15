(*                                                                           *)
(*  TextReader.m3                                                            *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*  pushback portion: Karl Papadantonakis <kp@caltech.edu>                   *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
MODULE TextReader;
IMPORT Text, TextList;
(*IMPORT Debug,Fmt;*)
IMPORT Rd, Wr, Thread, RdCopy, TextWr;
IMPORT FloatMode, Lex, Scan;

EXCEPTION IncompatibleDelimiters;

REVEAL 
  T = Public BRANDED Brand OBJECT

    (* the remaining text is represented as (pushback & Sub(line, start)) *)

    (* for efficiency, we can advance in the text without reallocating *)
    (* the line contains valid data from character 0 to Text.Length(line) *)
    pushback, line : TEXT;
    start : CARDINAL := 0;
  OVERRIDES
    next := Next;
    nextS := NextS;
    nextE := NextE;
    get := Get;
    getLR := GetLR;
    getLongReal := GetLR;
    getInt := GetInt;
    getCard := GetCard;
    getBool := GetBool;
    nextSE := NextSE;
    init := Init;
    initFromRd := InitFromRd;
    isEmpty := IsEmpty;
    empty := IsEmpty;
    shatter := Shatter;
    pushBack := PushBack;

    save := Save;
    continue := Unwind;
  END;

(**********************************************************************)

REVEAL
  Continuation = BRANDED Brand & " Continuation" OBJECT
    t : T;
    pushback, line : TEXT;
    start : CARDINAL;
  END;

PROCEDURE Save(t : T) : Continuation =
  BEGIN 
    WITH c = NEW(Continuation) DO
      c.t := t;
      c.pushback := t.pushback;
      c.line := t.line;
      c.start := t.start;
      RETURN c
    END
  END Save;

PROCEDURE Unwind(t : T; to : Continuation) =
  BEGIN
    <* ASSERT t = to.t *>
    t.pushback := to.pushback;
    t.line := to.line;
    t.start := to.start
  END Unwind;

(**********************************************************************)

PROCEDURE Get(self : T) : TEXT RAISES { NoMore } = 
  CONST
    Delims = SET OF CHAR { ' ', '\t', '\n', '\r' };
  BEGIN
    RETURN self.nextSE(Delims,TRUE)
  END Get;

PROCEDURE NextE(self : T; 
                delims : TEXT; skipNulls : BOOLEAN) : TEXT RAISES { NoMore } = 
  VAR res : TEXT; BEGIN 
    IF self.next(delims,res,skipNulls) THEN RETURN res
    ELSE RAISE NoMore
    END
  END NextE;

PROCEDURE NextSE(self : T; 
                READONLY delims : SET OF CHAR; 
                skipNulls : BOOLEAN) : TEXT RAISES { NoMore } = 
  VAR res : TEXT; BEGIN 
    IF self.nextS(delims,res,skipNulls) THEN RETURN res
    ELSE RAISE NoMore
    END
  END NextSE;

PROCEDURE IsEmpty(self : T) : BOOLEAN = 
  BEGIN RETURN Text.Length(self.line) <= self.start END IsEmpty;

PROCEDURE NextS(self : T; 
                READONLY delims: SET OF CHAR;
                VAR res : TEXT; skipNulls : BOOLEAN) : BOOLEAN =
  VAR
    min, lineLen: INTEGER;
  BEGIN
    IF Text.Length(self.pushback) > 0 THEN
      VAR
        saveLine := self.line;
        (* savePB := self.pushback; *)
        saveStart := self.start;
        found: BOOLEAN;
      BEGIN
        self.start := 0;
        self.line := self.pushback;
        self.pushback := "";
        found := NextS(self, delims, res, skipNulls);
        self.pushback := Text.Sub(self.line, self.start);
        self.line := saveLine;
        self.start := saveStart;
        IF found THEN
          (* actually could merge with next. oops. *)
          RETURN TRUE;
        ELSE
          <* ASSERT skipNulls *>
        END;
      END;
    END;
    min := self.start;
    lineLen := Text.Length(self.line);
    WHILE min < lineLen AND NOT Text.GetChar(self.line, min) IN delims DO
      INC(min); (* this loop a little tighter than it used to be *)
    END;
    res := Text.Sub(self.line,self.start,min - (*Mika forgot*)self.start);

(*
    (* this can be implemented simply by increasing self.start *)
    self.line := Text.Sub(self.line,min+1,LAST(CARDINAL)); (* save rest *)
*)

    self.start := min+1;

    (* clear out any stray delimiters after the end, if skipNulls is true *)
    IF skipNulls THEN
      VAR
        len := Text.Length(self.line);
      BEGIN
        WHILE len > self.start AND Text.GetChar(self.line, self.start) IN delims DO
          INC(self.start)
        END
      END
    END;

    IF Text.Length(self.line) <= self.start THEN
      self.line := "";
      self.start := 0;
      IF Text.Length(res) = 0 THEN 
        RETURN FALSE;
      END;
    END;
    
    IF Text.Length(res) = 0 AND skipNulls THEN
      RETURN NextS(self,delims,res,skipNulls)
    END;

    RETURN TRUE
  END NextS; 

PROCEDURE Next(self : T; 
               delims : TEXT; VAR res : TEXT; skipNulls : BOOLEAN) : BOOLEAN =
  VAR
    dset := SET OF CHAR{};
  BEGIN
    FOR i := 0 TO Text.Length(delims)-1 DO
      dset := dset + SET OF CHAR{Text.GetChar(delims, i)};
    END;
    RETURN NextS(self, dset, res, skipNulls);
  END Next;

PROCEDURE Init(self: T; line : TEXT) : T =
  BEGIN
    self.line := line;
    self.start := 0;
    self.pushback := "";
    RETURN self;
  END Init;

PROCEDURE InitFromRd(self : T; rd : Rd.T) : T RAISES { Rd.Failure, Thread.Alerted } =
  <* FATAL Wr.Failure *> (* cant happen *)
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    EVAL RdCopy.ToWriter(rd, wr);
    RETURN self.init(TextWr.ToText(wr))
  END InitFromRd;

PROCEDURE Shatter(self : T; listDelims : TEXT; 
            endDelims : TEXT; skipNulls := FALSE) : TextList.T =

  <* FATAL IncompatibleDelimiters *> (* force a program abort *)

  PROCEDURE CheckOverlap() =
    BEGIN
      FOR i := 0 TO Text.Length(listDelims) - 1 DO
        IF Text.FindChar(endDelims, Text.GetChar(listDelims, i)) # -1 THEN
          RAISE IncompatibleDelimiters
        END
      END
    END CheckOverlap;

  VAR
    res : TextList.T := NIL;
  BEGIN
    CheckOverlap();
    TRY
      VAR 
        reader := NEW(T).init(self.nextE(endDelims)); 
        word : TEXT; 
      BEGIN
        WHILE Next(reader, listDelims, word, skipNulls) DO
          res := TextList.Cons(word, res)
        END;
        res := TextList.ReverseD(res)
      END
    EXCEPT NoMore => <* ASSERT res = NIL *> (* skip *)
    END;
    RETURN res
  END Shatter;

(*
PROCEDURE Simplify(self: T) =
  BEGIN
    IF self.start # 0 THEN
      self.line := Text.Sub(self.line, self.start);
      self.start := 0;
    END;
    IF Text.Length(self.pushback) > 0 THEN
      self.line := self.pushback & self.line;
      self.pushback := "";
    END;
  END Simplify;
*)

PROCEDURE PushBack(self: T; t: TEXT) =
  BEGIN
    (* this is buggy if we change delimiters 
       self.pushback := t & self.pushback;
    *)
    (* less efficient, but: *)
    self.line := Text.Sub(self.line,0,self.start) & t & Text.Sub(self.line,self.start)
  END PushBack; 

(**********************************************************************)

PROCEDURE GetLR(t : T) : LONGREAL RAISES { NoMore, Lex.Error, FloatMode.Trap }=
  BEGIN RETURN Scan.LongReal(t.get()) END GetLR;

PROCEDURE GetInt(t : T) : INTEGER
  RAISES { NoMore, Lex.Error, FloatMode.Trap }=
  BEGIN RETURN Scan.Int(t.get()) END GetInt;

PROCEDURE GetBool(t : T) : BOOLEAN
  RAISES { NoMore, Lex.Error } =
  BEGIN RETURN Scan.Bool(t.get()) END GetBool;

PROCEDURE GetCard(t : T) : CARDINAL 
  RAISES { NoMore, Lex.Error, FloatMode.Trap }=
  BEGIN 
    WITH res = Scan.Int(t.get()) DO
      IF res < FIRST(CARDINAL) OR res > LAST(CARDINAL) THEN
        RAISE Lex.Error
      END;
      RETURN res
    END
  END GetCard;

PROCEDURE New(txt : TEXT) : T = BEGIN RETURN NEW(T).init(txt) END New;

BEGIN END TextReader.







