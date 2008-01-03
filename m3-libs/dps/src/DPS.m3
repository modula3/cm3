(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:24:28 PDT 1994 by kalsow                   *)
(*      modified on Wed Feb 12 11:49:06 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

UNSAFE MODULE DPS;

IMPORT Unix, Fifo, SchedulerPosix, Text, Err, Fmt;
IMPORT TextWr, Thread, Wr, cDPS, wraps, DPS;

<*FATAL Wr.Failure, Thread.Alerted *>

CONST PointsPerPixel = 0.92182; 
(* Get this from X someday. Experimental pMax value. *)

CONST SendStackBufferSize = 8000;

CONST ClipRepainting = TRUE;

TYPE TalkToXThreadClosure = Thread.Closure OBJECT END; 

TYPE TList = RECORD next: REFTList; t: T; END;
TYPE REFTList = REF TList;

VAR cPostScriptMutex: MUTEX;
VAR transformChangeMutex: MUTEX;

VAR tsMonitor: MUTEX; (* Serializes access to X and DPS engine. *)

VAR globalTs: REFTList;
VAR globalMaxXfd: INTEGER;
VAR globalXfds: Unix.FDSet;

CONST millisecond = 0.001d0;

VAR preferredFontName := "Times-Roman";

PROCEDURE GSaveAndClip (box: Box): TEXT = 
  BEGIN
  RETURN " gsave " & NewPathBox (box) & " clip newpath ";
  END GSaveAndClip;

PROCEDURE NewPathBox (box: Box): TEXT = 
 VAR data: TEXT;
  BEGIN
  data := " newpath " 
    & Fmt.Real(box.low.x) & " dup " & Fmt.Real(box.low.y)
    & " moveto "
    & Fmt.Real(box.high.y)
    & " lineto "
    & Fmt.Real(box.high.x) & " dup " & Fmt.Real(box.high.y)
    & " lineto "
    & Fmt.Real(box.low.y)
    & " lineto closepath ";
  RETURN data;
  END NewPathBox;

PROCEDURE GSaveAndClipIf (box, containee: Box): TEXT = 
  BEGIN
  IF ContainerContainee (box, containee) THEN RETURN " gsave "; 
   ELSE RETURN GSaveAndClip (box);
    END;
  END GSaveAndClipIf;

PROCEDURE GRestore (): TEXT =
  BEGIN
  RETURN " grestore ";
  END GRestore;

PROCEDURE BoxCoordsAsText (box: Box): TEXT = 
  BEGIN
  RETURN " " & Fmt.Real(box.low.x) & " " & Fmt.Real(box.low.y)
     & " " & Fmt.Real(box.high.x) & " " & Fmt.Real(box.high.y)
     & " ";
  END BoxCoordsAsText;

PROCEDURE BoxAlter (b: Box; fp: FixedPoint; w, h: REAL := -1.0): Box = 
 VAR d: REAL;
  BEGIN
  IF w >= 0.0 THEN
    CASE fp OF
    | FixedPoint.nw, FixedPoint.w, FixedPoint.sw =>
       b.high.x := b.low.x + w;
    | FixedPoint.n, FixedPoint.c, FixedPoint.s => 
      d := w - (b.high.x - b.low.x); 
      b.low.x := b.low.x - d / 2.0; b.high.x := b.high.x + d / 2.0;
    | FixedPoint.ne, FixedPoint.e, FixedPoint.se => 
      b.low.x := b.high.x - w;
      END;
    END;
  IF h >= 0.0 THEN
    CASE fp OF
    | FixedPoint.nw, FixedPoint.n, FixedPoint.ne =>
       b.low.y := b.high.y - h;
    | FixedPoint.w, FixedPoint.c, FixedPoint.e => 
      d := h - (b.high.y - b.low.y); 
      b.low.y := b.low.y - d / 2.0; b.high.y := b.high.y + d / 2.0;
    | FixedPoint.sw, FixedPoint.s, FixedPoint.se =>
       b.high.y := b.low.y + h;
      END;
    END;
  RETURN b;
  END BoxAlter;

PROCEDURE BoxesIntersect (b1, b2: Box): BOOLEAN = 
  BEGIN
  RETURN (b1.high.x > b2.low.x) AND (b1.high.y > b2.low.y)
   AND (b1.low.x < b2.high.x) AND (b1.low.y < b2.high.y);
  END BoxesIntersect;

PROCEDURE ContainerContainee (b1, b2: Box): BOOLEAN = 
  BEGIN
  RETURN (b1.high.x >= b2.high.x) AND (b1.high.y >= b2.high.y)
   AND (b1.low.x <= b2.low.x) AND (b1.low.y <= b2.low.y);
  END ContainerContainee;

PROCEDURE PlaceIsInBox (p: Place; b: Box): BOOLEAN = 
  BEGIN
  RETURN (p.x > b.low.x) AND (p.y > b.low.y)
   AND (p.x < b.high.x) AND (p.y < b.high.y);
  END PlaceIsInBox;

PROCEDURE BoxUnion (b1, b2: Box): Box = 
  BEGIN
  RETURN Box { Place { MIN(b1.low.x,b2.low.x), MIN(b1.low.y,b2.low.y) },
    Place { MAX(b1.high.x,b2.high.x), MAX(b1.high.y,b2.high.y) } };
  END BoxUnion;

CONST grayStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)
CONST colorStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)

PROCEDURE EdgedBoxClipAndPaint (box: Box; hue: REAL := -1.0): TEXT = (* Hue < 0.0 => Grays. *) 
 VAR path: TEXT := "";
  BEGIN
  IF box = ZeroBox THEN RETURN NIL; END;
  path := " newpath "
     & Fmt.Real(box.low.x) & " " & Fmt.Real(box.low.y) & " moveto " 
     & Fmt.Real(box.low.x) & " " & Fmt.Real(box.high.y) & " lineto "
     & Fmt.Real(box.high.x) & " " & Fmt.Real(box.high.y) & " lineto "
     & Fmt.Real(box.high.x) & " " & Fmt.Real(box.low.y) & " lineto "
     & " closepath ";
  IF hue >= 0.0 THEN
    RETURN " " & path & " clip " 
     & Fmt.Real(hue) & " 0.5 0.9 sethsbcolor gsave fill grestore "
     & Fmt.Real(hue) & " 1.0 0.3 sethsbcolor "
     & colorStrokeWidthText & " setlinewidth stroke ";
   ELSE
    RETURN " " & path & " clip 0.9 setgray gsave fill grestore "
     & "0.1 setgray " & grayStrokeWidthText & " setlinewidth stroke ";
    END;
  END EdgedBoxClipAndPaint;

PROCEDURE EscapeText (text: TEXT): TEXT = (* Fixes parens etc. for show. *)
 VAR k: INTEGER;
 VAR count: INTEGER;
 VAR a, b: REF ARRAY OF CHAR;
  BEGIN 
  k := Text.FindChar (text, ')');
  IF k < 0 THEN k := Text.FindChar (text, '(') END;
  IF k < 0 THEN k := Text.FindChar (text, '\\') END;
  IF k < 0 THEN RETURN text; END;
  (* Do the above first, hoping it's fast. *)
  a := NEW (REF ARRAY OF CHAR, Text.Length(text));
  Text.SetChars (a^, text);
  count := 0;
  FOR m := 0 TO Text.Length(text)-1 DO
    IF (a^[m]='(') OR (a^[m]=')') OR (a^[m]='\\') THEN count := count + 1; END;
    END;
  b := NEW (REF ARRAY OF CHAR, Text.Length(text) + count);
  k := 0;
  FOR m := 0 TO Text.Length(text)-1 DO
    IF (a^[m]='(') OR (a^[m]=')') OR (a^[m]='\\') THEN b^[k] := '\\'; k := k + 1; END;
    b^[k] := a^[m]; 
    k := k + 1;
    END;
  <* ASSERT k = Text.Length(text) + count *>
  RETURN Text.FromChars (SUBARRAY (b^, 0, Text.Length(text) + count));
  END EscapeText;

VAR unshifted: ARRAY [0..255] OF CHAR;
VAR shifted: ARRAY [0..255] OF CHAR;

PROCEDURE CharFromKey (key: INTEGER; modifiers: Modifiers): CHAR =
 VAR ret: CHAR;
  BEGIN 
  IF Modifier.Option IN modifiers THEN RETURN '\000'; END;
  IF Modifier.Shift IN modifiers THEN ret := shifted[key]; 
   ELSIF Modifier.Lock IN modifiers THEN ret := shifted[key]; 
   ELSE ret := unshifted[key];
    END;
  IF Modifier.Ctrl IN modifiers THEN ret := VAL(ORD(ret)+128, CHAR); END;
  RETURN ret;
  END CharFromKey;

PROCEDURE InitializeCharArrays() = 
 PROCEDURE Letter(i: INTEGER; c: CHAR) = 
   BEGIN 
   shifted[i] := c; 
   unshifted[i] := VAL ( ORD(c) + 32, CHAR ); 
   END Letter;
 PROCEDURE Other(i: INTEGER; s, c: CHAR) = 
   BEGIN 
   shifted[i] := s; 
   unshifted[i] := c; 
   END Other;
  BEGIN
  FOR j := 0 TO 255 DO shifted[j] := '\000'; END;
  FOR j := 0 TO 255 DO unshifted[j] := '\000'; END;
  Letter(194,'A'); Letter(217,'B'); Letter(206,'C'); Letter(205,'D');
  Letter(204,'E'); Letter(210,'F'); Letter(216,'G'); Letter(221,'H');
  Letter(230,'I'); Letter(226,'J'); Letter(231,'K'); Letter(236,'L');
  Letter(227,'M'); Letter(222,'N'); Letter(235,'O'); Letter(240,'P');
  Letter(193,'Q'); Letter(209,'R'); Letter(199,'S'); Letter(215,'T');
  Letter(225,'U'); Letter(211,'V'); Letter(198,'W'); Letter(200,'X');
  Letter(220,'Y'); Letter(195,'Z'); 
  Other(192,'!','1'); Other(197,'@','2'); Other(203,'#','3'); Other(208,'$','4'); 
  Other(214,'%','5'); Other(219,'^','6'); Other(224,'&','7'); Other(229,'*','8');   
  Other(234,'(','9'); Other(239,')','0'); Other(249,'_','-'); Other(245,'+','='); 
  Other(250,'{','['); Other(246,'}',']'); Other(242,':',';'); Other(251,'\"','\''); 
  Other(247,'|','\\'); Other(232,',',','); Other(237,'.','.'); Other(243,'?','/'); 
  Other(212,' ',' '); Other(232,',',','); Other(237,'.','.'); Other(243,'?','/'); 
  Other(189,'\n','\n'); Other(188,'\010','\010'); Other(190,'\t','\t'); 
  END InitializeCharArrays;

VAR globalRepainting, globalMouse, globalKey: BOOLEAN;

VAR mouseOrCleanMutex: MUTEX;

PROCEDURE CleanThreadForkee (ctc: CleanThreadClosure): REFANY RAISES {} =
 <*FATAL DPS.BadPostScript*>
 VAR r: T;
 VAR db: DirtyBox;
  BEGIN
  r := ctc.root;
  LOOP
    db := r.dirtyFifo.RemoveOrWait();
    LOCK mouseOrCleanMutex DO
      globalRepainting := TRUE;
      IF ClipRepainting THEN
       r.Send (
          " " & Fmt.Real(db.box.low.x)
        & " " & Fmt.Real(db.box.low.y)
        & " " & Fmt.Real(db.box.high.x-db.box.low.x)
        & " " & Fmt.Real(db.box.high.y-db.box.low.y)
        & " rectviewclip " );
       END;
      r.Paint (db.box, db.only);
      IF ClipRepainting THEN r.Send (" initviewclip "); END;
      globalRepainting := FALSE;
      END;
    END; (* of infinite LOOP *)
  END CleanThreadForkee;

PROCEDURE Create (t: T; width, height: INTEGER := 600; color: BOOLEAN := TRUE; over: T := NIL) =
 VAR tl: REFTList;
  BEGIN
  IF over = NIL THEN IF color THEN t.planes := 256; ELSE t.planes := 255; END;
   ELSIF over = t THEN t.planes := 1;
   ELSE 
    t.dpy := over.dpy; t.win := over.win;
    IF over.planes = 1 THEN t.planes := 2;
     ELSIF over.planes = 2 THEN t.planes := 4;
     ELSE t.planes := 1; 
      Err.Msg ("Bad usage of -over- in DPS.Create call.");
      END;
    END;
  LOCK cPostScriptMutex DO cDPS.docreatesimplewindow (t, width, height); END;
  tl := NEW (REFTList);
  tl^.t := t;
  t.dirtyFifo := Fifo.New (DirtyBoxMatchProc);
  LOCK tsMonitor DO 
    tl^.next := globalTs; globalTs := tl;
    globalMaxXfd := MAX (globalMaxXfd, t.fd);
    globalXfds := globalXfds + Unix.FDSet{t.fd};
    END;
  SendInternal ( t, " /dps-m3-original-matrix matrix currentmatrix def " 
   & " /ufill { gsave newpath uappend fill grestore } def ", TRUE );
  (* LOCK transformChangeMutex DO cDPS.stufftransforms (t); END; *)
  (* Should not need this ^^ since SendInternal does it too.  But do. *)
  (* I think its because the first one after a create has problems ... *)
  EVAL Thread.Fork (
   NEW (CleanThreadClosure, apply := CleanThreadForkee, root := t) );
  END Create;

PROCEDURE Send ( t: T; text: TEXT;
 regardlessOfCircumstance: BOOLEAN := FALSE;
 alreadyLocked: BOOLEAN := FALSE ) RAISES {BadPostScript} =
 VAR wr: Wr.T;
 VAR ri: INTEGER;
  BEGIN
  <* ASSERT ( regardlessOfCircumstance 
   OR globalRepainting OR globalMouse OR globalKey) *>
  wr := t.specialWriter; IF wr#NIL THEN Wr.PutText (wr, text); END;
  IF t.alwaysNervous THEN 
    ri := SendInternalNervously (t, text, FALSE, alreadyLocked);
   ELSE ri := 1; SendInternal (t, text, FALSE, alreadyLocked);
    END;
  IF ri # 0 THEN RETURN; ELSE RAISE BadPostScript(text); END;
 END Send;
 
 PROCEDURE SendInternalNervously ( t: T; text: TEXT;
  calculateTransforms, alreadyLocked: BOOLEAN ): INTEGER =
   VAR ri: INTEGER;
   PROCEDURE Internal () =
     BEGIN
     SendInternal (t, " { ", FALSE, TRUE);
     SendInternal (t, text, FALSE, TRUE);
     SendInternal ( t, 
      " } stopped { 0 } { 1 } ifelse /success exch def ",
      calculateTransforms, TRUE );
     ri := wraps.FetchInteger (t.ctx, "success", TRUE);
     END Internal;
  BEGIN
  IF alreadyLocked THEN Internal(); 
   ELSE LOCK cPostScriptMutex DO Internal(); END;
    END;
  RETURN ri;
  END SendInternalNervously;

PROCEDURE SendNervously ( t: T; text: TEXT;
 regardlessOfCircumstance: BOOLEAN := FALSE;
 alreadyLocked: BOOLEAN := FALSE ) RAISES {BadPostScript} =
 VAR wr: Wr.T;
 VAR ri: INTEGER;
  BEGIN
  <* ASSERT ( regardlessOfCircumstance 
   OR globalRepainting OR globalMouse OR globalKey) *>
  wr := t.specialWriter; IF wr#NIL THEN Wr.PutText (wr, text); END;
  ri := SendInternalNervously (t, text, FALSE, alreadyLocked); 
  IF ri # 0 THEN RETURN; ELSE RAISE BadPostScript(text); END;
  END SendNervously;

PROCEDURE AcquireDPSMutex () = 
  BEGIN
  Thread.Acquire (cPostScriptMutex);
  END AcquireDPSMutex;

PROCEDURE ReleaseDPSMutex () = 
  BEGIN
  Thread.Release (cPostScriptMutex);
  END ReleaseDPSMutex;

PROCEDURE SendInternal ( t: T; text: TEXT; 
 calculateTransforms: BOOLEAN := FALSE; 
 alreadyLocked: BOOLEAN := FALSE)  =
 VAR chars: ARRAY [0..SendStackBufferSize] OF CHAR;
 VAR point: UNTRACED REF CHAR;
  BEGIN
  IF Text.Length(text) >= SendStackBufferSize THEN 
    IF alreadyLocked THEN SimpleSendBig (t, text);
     ELSE LOCK cPostScriptMutex DO SimpleSendBig (t, text); END;
      END;
   ELSE
    Text.SetChars (chars, Text.Cat(text,"\000")); 
    point := ADR(chars[0]);
    IF alreadyLocked THEN cDPS.dosendps (t, point);
     ELSE LOCK cPostScriptMutex DO cDPS.dosendps (t, point); END;
      END;
    END;
  cDPS.doflush (t); (* Necessary when sending not in TalkToX thread - 7jun91 *)
  IF calculateTransforms THEN 
    LOCK transformChangeMutex DO cDPS.stufftransforms (t); END;
    END;
  END SendInternal;

VAR allocSimple: REF ARRAY OF CHAR;
VAR allocedSimple: INTEGER := 0;
PROCEDURE SimpleSendBig ( t: T; text: TEXT ) = (* Very single threaded. *)
 VAR point: UNTRACED REF CHAR;
  BEGIN
  IF Text.Length(text) > allocedSimple THEN
    Err.Msg ("Allocated buffer in DPS.SendInternal. Size = ",
              Fmt.Int(Text.Length(text)));
    allocSimple := NEW (REF ARRAY OF CHAR, Text.Length(text) + 100 + 1);
    allocedSimple := Text.Length (text) + 100;
    END;
  Text.SetChars (allocSimple^, Text.Cat(text,"\000")); 
  point := ADR(allocSimple^[0]);
  cDPS.dosendps (t, point);
  END SimpleSendBig;

PROCEDURE SendClientTransformation (t: T; text: TEXT) =
  BEGIN
  t.currentTransformation := text;
  SendTransformations (t);
  END SendClientTransformation;

PROCEDURE SendTransformations (t: T) =
  BEGIN
  SendInternal ( t, " dps-m3-original-matrix setmatrix "
   & t.backgroundTransformation & " " & t.currentTransformation, TRUE );
  END SendTransformations;

PROCEDURE SendSpecialFoundation (t: T; text: TEXT) =
 VAR wr: Wr.T;
  BEGIN 
  (* Do not save in foundation. But Send. DPS versus early PS printers. *)
  wr := t.specialWriter; IF wr#NIL THEN Wr.PutText (wr, text); END;
  SendInternal (t, text);
  END SendSpecialFoundation;

PROCEDURE SendFoundation (t: T; text: TEXT) =
 VAR newList: REF ARRAY OF TEXT;
 VAR slot: INTEGER;
  BEGIN (* Needs to be monitored. *)
  slot := -1;
  IF t.foundationList=NIL THEN t.foundationList := NEW (REF ARRAY OF TEXT, 100); END;
  FOR k := NUMBER(t.foundationList^)-1 TO 0 BY -1 DO
    IF t.foundationList^[k] = NIL THEN slot := k; 
     ELSIF Text.Equal (t.foundationList^[k], text) THEN RETURN; 
      END;
    END;
  IF slot < 0 THEN (* Full. *)
    newList := NEW (REF ARRAY OF TEXT, NUMBER(t.foundationList^) + 100); 
    FOR k := 0 TO NUMBER(t.foundationList^)-1 DO
      newList^[k] := t.foundationList^[k];
      END;
    slot := NUMBER(t.foundationList^);
    t.foundationList := newList;
    END;
  t.foundationList^[slot] := text;
  SendInternal (t, text);
  END SendFoundation;

PROCEDURE UnsendFoundation (t: T; text: TEXT) =
  BEGIN (* Needs to be monitored. *)
  IF t.foundationList=NIL THEN RETURN; END;
  FOR k := NUMBER(t.foundationList^)-1 TO 0 BY -1 DO
    IF Text.Equal (t.foundationList^[k], text) THEN 
      t.foundationList^[k] := NIL; 
      RETURN;
      END;
    END;
  END UnsendFoundation;

PROCEDURE PostscriptToWriter (t: T; wr: Wr.T) =
 VAR r1, r2: REAL;
  BEGIN (* Not monitored. Should be. *)
  (* Client responsible for any wrapping, including 'showpage' *)
  IF t.foundationList # NIL THEN
    FOR k := 0 TO NUMBER(t.foundationList^)-1 DO
      IF t.foundationList^[k] # NIL THEN  
        Wr.PutText (wr, t.foundationList^[k]);
        Wr.PutText (wr, "\n");
        END;
      END;
    END;
  Wr.PutText (wr, " /window { ");
  Wr.PutText (wr, t.backgroundTransformation);
  Wr.PutText (wr, " } def \n");
  (* Wr.PutText (wr, " window \n"); *)
  (* Up to client to invoke 'windiw' if he really wants *)
  (* the screen size as opposed to the 'desired' size. *)
  TransformToDPS (t, t.xWidth, t.xHeight, r1, r2);
  Wr.PutText ( wr,
   " 0.0 0.0 moveto " & Fmt.Real(r1) & " " & Fmt.Real(r2) & " stroke \n" );
  Wr.PutText (wr, t.currentTransformation);
  Wr.PutText (wr, "\n");
  t.specialWriter := wr;
  globalRepainting := TRUE;
  t.Paint (EverywhereBox, NIL);
  globalRepainting := FALSE; (* RISKY *)
  t.specialWriter := NIL;
  END PostscriptToWriter;

PROCEDURE PostscriptToText (t: T): TEXT =
 VAR wr: Wr.T;
  BEGIN
  wr := TextWr.New ();
  PostscriptToWriter (t, wr);
  RETURN TextWr.ToText (wr);
  END PostscriptToText;

PROCEDURE Flush (t: T) =
  BEGIN
  LOCK cPostScriptMutex DO cDPS.doflush (t); END;
  END Flush;

PROCEDURE KillInputFocus (<*UNUSED*> t: T) =
  BEGIN
  (* Work done by subclasser. *)
  END KillInputFocus;

PROCEDURE ModifiersFromX (xModifiers: INTEGER): Modifiers =
 VAR mods: Modifiers;
  BEGIN
  mods := Modifiers{};
  IF (xModifiers) MOD 16 >= 8 THEN mods := mods + Modifiers{Modifier.Option}; END;
  IF (xModifiers) MOD 8 >= 4 THEN mods := mods + Modifiers{Modifier.Ctrl}; END;
  IF (xModifiers) MOD 4 >= 2 THEN mods := mods + Modifiers{Modifier.Lock}; END;
  IF (xModifiers) MOD 2 >= 1 THEN mods := mods + Modifiers{Modifier.Shift}; END;
  RETURN mods;
  END ModifiersFromX;

PROCEDURE PreferredFontName (): TEXT =
  BEGIN
  RETURN preferredFontName;
  END PreferredFontName;

PROCEDURE SetPreferredFontName (name: TEXT) =
  BEGIN
  preferredFontName := name;
  END SetPreferredFontName;

PROCEDURE ShowItAccentedPostScript (text: TEXT): TEXT =
 VAR data: TEXT := "";
 VAR c, baseChar: CHAR;
 VAR isAccent: BOOLEAN;
  BEGIN
  IF ContainsAccent(text) THEN
    FOR j := 0 TO Text.Length(text)-1 DO
      c := Text.GetChar (text,j);
      isAccent := (j > 0) AND IsAccent (c);
      (* Treat initial accent as real character! *)
      IF isAccent THEN 
        data := data & " currentpoint currentpoint exch "
         & "(" & Text.FromChar(c) & ") stringwidth pop "
         (* Knowing that accents do not have to be escaped .. *)
         & "(" & EscapeText(Text.FromChar(baseChar)) & ") stringwidth pop "
         & " add 2.0 div 1.0 add sub exch moveto ";
         (* The "1.0 add" is a heuristic for centering over vowels. *)
        ELSE baseChar := c; (* For later. *)
        END;
      data := data 
       & " (" & EscapeText(Text.FromChar(c)) & ") " & " show ";
      IF isAccent THEN data := data & " moveto "; END;
      END;
   ELSE data := " (" & EscapeText(text) & ") " & " show ";
    END;
  RETURN data;
  END ShowItAccentedPostScript;

PROCEDURE ContainsAccent (text: TEXT): BOOLEAN =
 VAR it: INTEGER;
  BEGIN
  FOR j := 0 TO Text.Length(text)-1 DO
    it := ORD(Text.GetChar(text,j));
    IF (it > 192) AND (it < 208) THEN RETURN TRUE; END;
    END;
  RETURN FALSE;
  END ContainsAccent;

PROCEDURE IsAccent (char: CHAR): BOOLEAN =
 VAR it: INTEGER;
  BEGIN
  it := ORD(char);
  IF (it > 192) AND (it < 208) THEN RETURN TRUE; END;
  RETURN FALSE;
  END IsAccent;

 VAR recentMeasureFontName: TEXT := "";
 VAR recentMeasurement: REF ARRAY [0..255] OF REAL;
 VAR recentMeasureMutex: MUTEX;

PROCEDURE MeasureText ( 
 text: TEXT; window: T; fontName: TEXT; 
 accentsHaveWidth: BOOLEAN := FALSE ): 
 REF ARRAY OF REAL =
 VAR ret: REF ARRAY OF REAL;
 VAR thisMeasurement: REF ARRAY [0..255] OF REAL;
 VAR voidHeight: REAL;
  BEGIN
  ret := NEW ( REF ARRAY OF REAL, Text.Length(text) );
  LOCK recentMeasureMutex DO
    IF Text.Equal (recentMeasureFontName, fontName) THEN
      thisMeasurement := recentMeasurement;
     ELSE thisMeasurement := NIL;
      END;
    END;

  IF thisMeasurement = NIL THEN
     thisMeasurement := NEW (REF ARRAY [0..255] OF REAL);
     FOR j := 0 TO 255 DO
     IF j < 32 THEN thisMeasurement^[j] := 0.0;
      ELSIF (j > 127) AND (j < 161) THEN thisMeasurement^[j] := 0.0;
      ELSIF j = 255 THEN thisMeasurement^[j] := 0.0;
      ELSE
        LOCK tsMonitor DO
          wraps.Stringwidth ( window.ctx, fontName, Text.FromChar(VAL(j,CHAR)), 
          voidHeight, thisMeasurement^[j] );
          END;
        END;
      END;
    END;

  LOCK recentMeasureMutex DO
    IF NOT Text.Equal (recentMeasureFontName, fontName) THEN
      recentMeasureFontName := fontName;
      recentMeasurement := thisMeasurement;
      END;
    END;

  FOR j := 0 TO Text.Length(text)-1 DO
    IF (NOT accentsHaveWidth) AND IsAccent(Text.GetChar(text,j)) THEN
      ret^[j] := 0.0;
     ELSE ret^[j] := thisMeasurement^[ORD(Text.GetChar(text,j))];
      END;
    END;
  RETURN ret;
  END MeasureText;

PROCEDURE TextWidth ( 
 text: TEXT; window: T; fontName: TEXT; 
 accentsHaveWidth: BOOLEAN := FALSE ): REAL =
 VAR thisMeasurement: REF ARRAY [0..255] OF REAL;
 VAR voidHeight, width: REAL;
  BEGIN
  LOCK recentMeasureMutex DO
    IF Text.Equal (recentMeasureFontName, fontName) THEN
      thisMeasurement := recentMeasurement;
     ELSE thisMeasurement := NIL;
      END;
    END;

  IF (thisMeasurement = NIL) AND accentsHaveWidth THEN
    LOCK tsMonitor DO
      wraps.Stringwidth ( window.ctx, fontName, text, 
      voidHeight, width );
      END;
    RETURN width;
    END;

  IF thisMeasurement = NIL THEN
     thisMeasurement := NEW (REF ARRAY [0..255] OF REAL);
     FOR j := 0 TO 255 DO
     IF j < 32 THEN thisMeasurement^[j] := 0.0;
      ELSIF (j > 127) AND (j < 161) THEN thisMeasurement^[j] := 0.0;
      ELSIF j = 255 THEN thisMeasurement^[j] := 0.0;
      ELSE
        LOCK tsMonitor DO
          wraps.Stringwidth ( window.ctx, fontName, Text.FromChar(VAL(j,CHAR)), 
          voidHeight, thisMeasurement^[j] );
          END;
        END;
      END;
    END;

  LOCK recentMeasureMutex DO
    IF NOT Text.Equal (recentMeasureFontName, fontName) THEN
      recentMeasureFontName := fontName;
      recentMeasurement := thisMeasurement;
      END;
    END;

  width := 0.0;
  FOR j := 0 TO Text.Length(text)-1 DO
    IF accentsHaveWidth OR (NOT IsAccent(Text.GetChar(text,j))) THEN
      width := width + thisMeasurement^[ORD(Text.GetChar(text,j))];
      END;
    END;
  RETURN width;
  END TextWidth;

PROCEDURE MeasureChar ( char: CHAR; window: T; fontName: TEXT; 
 accentsHaveWidth: BOOLEAN := FALSE ): REAL =
 VAR array: REF ARRAY OF REAL;
  BEGIN
  array := MeasureText ( Text.FromChar(char), 
   window, fontName, accentsHaveWidth );
  RETURN array^[0];
  END MeasureChar;

PROCEDURE ButtonFromX (xButton: INTEGER): Button =
  BEGIN
  CASE xButton OF
  | 1 => RETURN Button.Left;
  | 2 => RETURN Button.Middle;
  | 3 => RETURN Button.Right;
    ELSE <*ASSERT FALSE*>
    END;
  END ButtonFromX;

TYPE DirtyBox = Fifo.E OBJECT box: Box; only: REFANY; END;

TYPE CleanThreadClosure = Thread.Closure OBJECT root: T; END;

PROCEDURE Dirty (t: T; box: Box; only: REFANY) =
 (* VAR c: INTEGER;*)
  BEGIN
  t.dirtyFifo.Insert ( NEW(DirtyBox, box := box, only := only) );
  (*
  c := t.dirtyFifo.Count();
  IF c > 9 THEN 
    Err.Msg ("Awaiting DPS: ", Fmt.Integer(c), " Requests." ); 
    END;
  *)
  END Dirty;

PROCEDURE DirtyBoxMatchProc (new, old: Fifo.E): Fifo.E =
 VAR o, n: DirtyBox;
  BEGIN
  TYPECASE old OF DirtyBox => o := NARROW(old, DirtyBox) ELSE RETURN NIL END;
  TYPECASE new OF DirtyBox => n := NARROW(new, DirtyBox) ELSE RETURN NIL END;
  IF n.box = o.box THEN (* Only equality for now. *)
    IF n.only = o.only THEN RETURN old; END;
    IF o.only = NIL THEN RETURN old; END;
    IF n.only = NIL THEN RETURN new; END;
    END;
  RETURN NIL;
  END DirtyBoxMatchProc;

PROCEDURE CallMouseProc ( t: T;
 b: Button; p: Place; m: Modifiers; c: ClickType ) =
  BEGIN
  LOCK mouseOrCleanMutex DO
    globalMouse := TRUE;
    EVAL t.Mouse ( MouseEvent { b, p, m, c } );
    globalMouse := FALSE;
    END;
  END CallMouseProc;

PROCEDURE CallKeyProc ( t: T; k: INTEGER; m: Modifiers; c: ClickType ) =
  BEGIN
  LOCK mouseOrCleanMutex DO
    globalKey := TRUE;
    t.Key ( KeyEvent { k, m, c } );
    globalKey := FALSE;
    END;
  END CallKeyProc;

PROCEDURE TalkToX (<*UNUSED*> tc: TalkToXThreadClosure): REFANY RAISES {} =
 VAR displaySource: T;
 VAR ts: ARRAY [0..7] OF T;
 VAR tscount: INTEGER;
 VAR box: Box;
 VAR in, out: INTEGER;
 VAR win: INTEGER;
 VAR event: INTEGER;
 VAR xButton, xModifiers: INTEGER;
 VAR xX, xY, xW, xH: INTEGER;
 VAR r1, r2, r3, r4, r5, r6, r7, r8: REAL;
 VAR scale1, scale2: REAL;
 VAR num, den: REAL;
 VAR recentDown: Button;
 VAR cursorLocation: T;
 VAR it: INTEGER;
  PROCEDURE EvaluateCursorLocation() =
   BEGIN
    FOR k := 0 TO tscount-1 DO
      IF ts[k].cursor # 0 THEN cursorLocation := ts[k]; RETURN; END;
      END;
    cursorLocation := ts[0];
    END EvaluateCursorLocation;
  BEGIN
  in := 0; out := 0;
  displaySource := NIL;
  WHILE displaySource = NIL DO (* Assuming all windows on same display. *)
    Thread.Pause (10.0d0 * millisecond); 
    LOCK tsMonitor DO 
      IF globalTs#NIL THEN displaySource := globalTs^.t;
        <* ASSERT displaySource#NIL *> 
        END; 
      END;
    END;
  LOOP
    win := 0;
    in := in + 1;
    Thread.Pause(0.1d0 * millisecond); (* Thread.Yield() *)
    LOCK cPostScriptMutex DO 
      cDPS.doprocessinputs ( displaySource.dpy, 
      ADR(win), ADR(event), ADR(xButton), ADR(xModifiers), 
      ADR(xX), ADR(xY), ADR(xW), ADR(xH) ); 
      END;
    out := out + 1;
    IF win # 0 THEN
      tscount := WinToTs (win, ts);
      IF tscount > 0 THEN
        CASE event OF
        | 21 => (* Button Down. *) 
             EvaluateCursorLocation();
             TransformToDPS (cursorLocation, xX, xY, r1, r2);
             recentDown := ButtonFromX(xButton);
             CallMouseProc ( cursorLocation, 
              recentDown, Place{r1,r2}, 
              ModifiersFromX (xModifiers), ClickType.FirstDown );
        | 22 => (* Button Up. *)
             EvaluateCursorLocation();
             TransformToDPS (cursorLocation, xX, xY, r1, r2);
             CallMouseProc ( cursorLocation, 
              ButtonFromX(xButton), Place{r1,r2}, 
              ModifiersFromX (xModifiers), ClickType.LastUp );
        | 23 => (* Button Dragging: xButton is -state- not -button-! *)
             EvaluateCursorLocation();
             TransformToDPS (cursorLocation, xX, xY, r1, r2);
             CallMouseProc ( cursorLocation, 
              recentDown, Place{r1,r2}, 
              ModifiersFromX (xModifiers), ClickType.Dragging );
        | 31 => (* Key Down is mostly ignored. *) 
             IF (xButton=167) AND (tscount>1) THEN 
               (* Cycle the cursor window. *)
               (* 167 thru 170 are arrow keys. *)
               (* 177 is -compose- 174 is top-left function. *)
               it := -1;
               FOR k := 0 TO tscount-1 DO 
                 IF ts[k].cursor # 0 THEN it := k; END; 
                 END;
               IF it >= 0 THEN
                 ts[(it+1) MOD tscount].cursor := ts[it].cursor;
                 ts[it].cursor := 0;
                 cDPS.noticeCursor (ts[(it+1) MOD tscount]); 
                 END;
               END;
        | 32 => (* Key Up. *)
             CallKeyProc ( ts[0], xButton, 
              ModifiersFromX (xModifiers), ClickType.LastUp );
        | 41 => (* ConfigureNotify: new values in x, y, width, height. *)
             IF (xW # 0) AND (xH # 0) THEN (* Not bogus / null reformat. *)
               FOR k := 0 TO tscount-1 DO
                 IF ((xW # ts[k].xWidth) OR (xH # ts[k].xHeight)) THEN 
                   IF (ts[k].desiredWidth # 0.0) 
                    AND (ts[k].desiredHeight # 0.0) THEN
                     scale1 := FLOAT (xW) 
                      * PointsPerPixel / ts[k].desiredWidth; 
                     scale2 := FLOAT (xH) 
                      * PointsPerPixel / ts[k].desiredHeight; 
                     IF ts[k].backgroundTransformationMaintainsSimilarity THEN
                       scale1 := MIN (scale1, scale2);
                       IF ts[k].backgroundTransformationScaler > 0.0 THEN
                         scale2 := 1.0; (* Used as a temp. *)
                         WHILE scale2 >= scale1 * 2.0 DO
                           scale2 := scale2 / 2.0; 
                           END;
                         WHILE scale2 < scale1 DO scale2 := scale2 * 2.0; END;
                         (* Scale2 is closest 2^n above or equal to scale1. *)
                         num := ts[k].backgroundTransformationScaler * scale2;
                         den := num;
                         WHILE ( scale2 * num / den > scale1 ) DO 
                           num := num - 1.0; 
                           END;
                         scale1 := scale2 * num / den;
                         END;
                       scale2 := scale1;
                       END;
                     (* DPS with gravity # NorthWest is buggy. *)
                     ts[k].yTranslationNeeded := FLOAT (FullHeight - xH) 
                      * PointsPerPixel; 
                     ts[k].backgroundTransformation := 
                       " 0.0 " & Fmt.Real(ts[k].yTranslationNeeded) 
                       & " translate " & Fmt.Real(scale1) & " "
                       & Fmt.Real(scale2) & " scale ";
                     SendTransformations (ts[k]);
                     ts[k].Dirty (EverywhereBox, NIL); 
                    ELSE (* Client doesn't want re-scale. *)
                     (* If height changes, we relocate. *)
                     (* DPS with gravity # NorthWest buggy. *)
                     IF ts[k].xHeight = 0 THEN 
                       ts[k].xHeight := FullHeight; 
                       END;
                     IF xH # ts[k].xHeight THEN 
                       ts[k].yTranslationNeeded := ts[k].yTranslationNeeded
                         + FLOAT (ts[k].xHeight - xH) * PointsPerPixel; 
                       ts[k].backgroundTransformation := 
                         " 0.0 " & Fmt.Real(ts[k].yTranslationNeeded) 
                         & " translate ";
                       SendTransformations (ts[k]);
                       ts[k].Dirty (EverywhereBox, NIL);
                       END;
                     END;
                   END;
                 ts[k].xWidth := xW;
                 ts[k].xHeight := xH;
                 END; (* of FOR *)
               END; (* of IF not bogus w & h *)
        | 99 =>
             FOR k := 0 TO tscount-1 DO
               TransformToDPS (ts[k], xX, xY, r1, r2);
               TransformToDPS (ts[k], xX+xW, xY, r3, r4);
               TransformToDPS (ts[k], xX, xY+xH, r5, r6);
               TransformToDPS (ts[k], xX+xW, xY+xH, r7, r8);
               box.low := Place { MIN(MIN(r1,r3),MIN(r5,r7)),
                MIN(MIN(r2,r4),MIN(r6,r8)) };
               box.high := Place { MAX(MAX(r1,r3),MAX(r5,r7)),
                MAX(MAX(r2,r4),MAX(r6,r8)) };
               ts[k].Dirty (box, NIL);
               END;
          ELSE
           Err.Msg ("Got unknown event ", Fmt.Int(event), " from X.");
          END;
       ELSE Err.Msg ("Got unknown window from X.");
       END; (* of IF tscount > 0 *)
     ELSE (* win = 0 *)
       (* WaitForX (); *)
       EVAL SchedulerPosix.IOWait (displaySource.fd, read := TRUE);
     END; (* of win # 0 *)
    END; (* of LOOP *)
  END TalkToX;

(***********
PROCEDURE WaitForX () =
 VAR maxfd: INTEGER;
 VAR fds: Unix.FDSet;
  BEGIN
  LOCK tsMonitor DO (* Copy to avoid concurrent use. *)
    maxfd := globalMaxXfd;
    fds := globalXfds;
    END; 
  EVAL SchedulerPosix.IOSelect (maxfd + 1, ADR(fds), NIL, ADR(fds)); 
  END WaitForX;
*************)

PROCEDURE TransformToDPS (t: T; x, y: INTEGER; VAR rx, ry: REAL) =
  BEGIN
  LOCK transformChangeMutex DO
    cDPS.transformtodps (t, x, y, ADR(rx), ADR(ry));
    END; 
  END TransformToDPS;

PROCEDURE WinToTs (win: INTEGER; VAR ts: ARRAY [0..7] OF T): INTEGER =
 VAR tl: REFTList;
 VAR k: INTEGER;
  BEGIN
  k := 0;
  LOCK tsMonitor DO 
    tl := globalTs;
    WHILE tl#NIL DO
      IF tl^.t.win=win THEN ts[k] := tl^.t; k := k + 1; END;
      tl := tl^.next;
      END;
    END;
  RETURN k;
  END WinToTs;

PROCEDURE PlaceToStderr (pre: TEXT; place: Place) =
  BEGIN
    Err.Msg (pre, " ", Fmt.Real (place.x), " " & Fmt.Real (place.y));
  END PlaceToStderr;

PROCEDURE BoxToStderr (pre: TEXT; box: Box) =
  BEGIN
    Err.Msg (pre, " ",
     Fmt.Real(box.low.x) & " "       
   & Fmt.Real(box.low.y) & " "       
   & Fmt.Real(box.high.x) & " "       
   & Fmt.Real(box.high.y) );
  END BoxToStderr;

BEGIN
  cPostScriptMutex := NEW (MUTEX);
  transformChangeMutex := NEW (MUTEX);
  mouseOrCleanMutex := NEW (MUTEX);
  recentMeasureMutex := NEW (MUTEX);

  tsMonitor := NEW (MUTEX);
  globalMaxXfd := 0;
  globalXfds := Unix.FDSet{};

  InitializeCharArrays();

  Thread.MinDefaultStackSize (65536);
  EVAL Thread.Fork (NEW (TalkToXThreadClosure, apply := TalkToX));
  Thread.MinDefaultStackSize (0);
  END DPS.



