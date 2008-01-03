(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Searching and querying *)

(* Last modified on Thu Mar 16 14:39:25 PST 1995 by birrell   *)

MODULE Find EXPORTS Find;

IMPORT ASCII, Fmt, FormsVBT, LecternDoc, LecternOCR, Text, TextF, Thread, VBT;

REVEAL T = Public BRANDED OBJECT
    dir: LecternDoc.Dir;
    ocr: LecternOCR.T;
    fv: FormsVBT.T;
  OVERRIDES
    init := Init;
    search := Search;
    query := Query;
  END;

PROCEDURE Init(t: T; READONLY dir: LecternDoc.Dir; ocr: LecternOCR.T;
               fv: FormsVBT.T): T =
  (* LL = VBT.mu *)
  BEGIN
    t.dir := dir;
    t.ocr := ocr;
    t.fv := fv;
    RETURN t;
  END Init;


(* *)
(* Search engine *)
(* *)

TYPE Target = RECORD
    pat: TEXT;                  (* what to search for *)
    ignoreCase: BOOLEAN;        (* case sensitivity *)
    len: INTEGER;               (* length of "pat" *)
    tbl: ARRAY CHAR OF INTEGER; (* how far to skip *)
  END;

PROCEDURE SetupTarget(VAR t: Target; pat: TEXT) =
    (* Set up table for Boyer-Moore (or is it KNP?) string search *)
  BEGIN
    t.pat := pat;
    t.ignoreCase := TRUE;
    t.len := Text.Length(pat);
    t.tbl := ARRAY CHAR OF INTEGER{t.len, ..};
(* We could say ginore case unless the target has some upper case ...
   But that doesn't seem to work out well for the documents of interest,
   and certainly not for the usage of double-clicking a word to start a
   search.
    FOR i := 0 TO t.len-1 DO
      IF pat[i] IN ASCII.Uppers THEN t.ignoreCase := FALSE; EXIT END;
    END;
*)
    FOR i := 0 TO t.len-1 DO
      IF t.ignoreCase THEN
        t.tbl[ASCII.Lower[pat[i]]] := t.len-i-1;
        t.tbl[ASCII.Upper[pat[i]]] := t.len-i-1;
      ELSE
        t.tbl[pat[i]] := t.len-i-1;
      END;
    END;
  END SetupTarget;

PROCEDURE FindTarget(READONLY t: Target; candidate: TEXT): INTEGER
                    RAISES { Error } =
    (* Returns index in t.pat at which candidate starts, or -1. Efficiently. *)
  VAR
    txtlen: INTEGER;
    p := t.len-1;
  BEGIN
    IF candidate = NIL THEN RAISE Error("OCR data missing") END;
    txtlen := NUMBER(candidate^)-1;
    WHILE p < txtlen DO
      VAR m := t.tbl[candidate[p]];
      BEGIN
        IF m = 0 THEN
          (* test for match ending at candidate[p] *)
          VAR
            j := 0;
            startOfMatch := p-(t.len-1);
          BEGIN
            IF t.ignoreCase THEN
              WHILE j # t.len AND ASCII.Lower[t.pat[j]] =
                                      ASCII.Lower[candidate[startOfMatch+j]] DO
                INC(j);
              END;
            ELSE
              WHILE j # t.len AND t.pat[j] = candidate[startOfMatch+j] DO
                INC(j);
              END;
            END;
            IF j = t.len THEN RETURN startOfMatch ELSE INC(p) END;
          END;
        ELSE
          INC(p, m);
        END;
      END;
    END;
    RETURN -1
  END FindTarget;

<*UNUSED*>
PROCEDURE FindAsWord(READONLY t: Target; candidate: TEXT): INTEGER
                    RAISES { Error } =
  VAR found := FindTarget(t, candidate);
  BEGIN
    IF found < 0 THEN RETURN found END;
    IF found > 0 AND t.pat[0] IN ASCII.AlphaNumerics AND
                     candidate[found-1] IN ASCII.AlphaNumerics THEN
      (* pattern starts with alpha, and char preceding match is alpha *)
      RETURN -1;
    END;
    IF found + t.len < Text.Length(candidate) AND
             t.pat[t.len-1] IN ASCII.AlphaNumerics AND
             candidate[found+t.len] IN ASCII.AlphaNumerics THEN
      (* pattern ends with alpha, and char following match is alpha *)
      RETURN -1;
    END;
    RETURN found;
  END FindAsWord;

PROCEDURE Search(t: T; forward, extreme: BOOLEAN;
                 VAR selStart, selEnd: LecternOCR.SelPos)
                 RAISES { Error, Thread.Alerted } =
  (* LL = lect *)
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR
    words: LecternOCR.Words := NIL;
    done := FALSE;
    target: Target;
  BEGIN
    LOCK VBT.mu DO
      WITH
        pat = FormsVBT.GetText(t.fv, "FindTxt"),
        patLen = Text.Length(pat) DO
        IF patLen = 0 THEN RAISE Error("Search string is empty") END;
        FOR i := 0 TO patLen-1 DO
          IF pat[i] = ' ' THEN
            RAISE Error("Search string contains spaces: not implemented");
          END;
        END;
        SetupTarget(target, pat);
      END;
      t.ocr.getSelection(selStart, selEnd);
      FormsVBT.PutText(t.fv, "Status", "Searching ...");
    END;
    TRY
      IF selStart = LecternOCR.NoSelPos THEN extreme := TRUE END;
      IF extreme THEN
        IF forward THEN
          selStart.page := 0 - t.dir.origin;
        ELSE
          selStart.page := LAST(t.dir.pages^) - t.dir.origin;
        END;
      END;
      Thread.Pause(0.001D0); (* to allow "stop" *)
      IF extreme THEN
        IF forward THEN
          selStart.word := -1;
        ELSE
          selStart.word := 1000000;
        END;
      END;
      LOOP
        IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
        IF FindTarget(target, t.ocr.getWordSeq(selStart.page)) >= 0 THEN
          (* It seems to be one this page: check in detail *)
          LOCK VBT.mu DO words := t.ocr.getWords(selStart.page) END;
          LOOP
            IF forward THEN
              INC(selStart.word);
              IF selStart.word > LAST(words^) THEN EXIT END;
            ELSE
              IF selStart.word > NUMBER(words^) THEN
                selStart.word := NUMBER(words^);
              END;
              DEC(selStart.word);
              IF selStart.word < 0 THEN EXIT END;
            END;
            IF FindTarget(target, words[selStart.word]) >= 0 THEN
              selEnd := selStart;
              INC(selEnd.word);
              LOCK VBT.mu DO
                IF selStart.page > 0 THEN
                  FormsVBT.PutText(t.fv, "Status",
                                   "Found on page " & Fmt.Int(selStart.page));
                ELSE
                  FormsVBT.PutText(t.fv, "Status",
                                   "Found on image " &
                                   Fmt.Int(selStart.page+t.dir.origin+1));
                END;
              END;
              done := TRUE;
              RETURN
            END;
          END;(*inner loop*)
        END;
        IF forward THEN
          INC(selStart.page);
          IF selStart.page+t.dir.origin > LAST(t.dir.pages^) THEN EXIT END;
        ELSE
          DEC(selStart.page);
          IF selStart.page+t.dir.origin < 0 THEN EXIT END;
        END;
        IF forward THEN
          selStart.word := -1;
        ELSE
          selStart.word := 1000000;
        END;
      END;
      selStart := LecternOCR.NoSelPos;
      selEnd := LecternOCR.NoSelPos;
      LOCK VBT.mu DO FormsVBT.PutText(t.fv, "Status", "Not found") END;
      done := TRUE;
    FINALLY
      IF NOT done THEN
        LOCK VBT.mu DO FormsVBT.PutText(t.fv, "Status", "Stopped") END;
      END;
    END;
  END Search;

PROCEDURE Query(<*UNUSED*>t: T): Request RAISES { Error } =
  BEGIN
    RAISE Error("Not implemented yet");
  END Query;

BEGIN
END Find.
