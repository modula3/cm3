(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 18:09:57 PST 1994 by isard      *)

MODULE M3LoaderObj EXPORTS M3LoaderObj, M3LoaderObjRep;

IMPORT IntIntTbl, SortedIntIntTbl, SortedIntRefTbl, IO, IntArraySort, Text;
IMPORT Fmt, Wr, Stdio;
IMPORT M3ID, M3LoaderAccess, M3LoaderProcess, M3Loader;

FROM M3LoaderAccess IMPORT SegType;
FROM M3Loader IMPORT ObjModule;

REVEAL
  M3Loader.ObjModule = ObjRep BRANDED OBJECT
      symreloctable : REF ARRAY OF RelocIndex := NIL;
      relocindex    : REF ARRAY OF INTEGER;
      importtbl     : IntIntTbl.Default;
      copied_init    := FALSE;
    OVERRIDES
      relocate := relocate;
      discard := discard;
      fixup := fixup;
      show_function := show_function;
    END;

TYPE
  RelocIndex = RECORD
    start := 0;
    end   := -1;
  END;

PROCEDURE error (msg: TEXT) =
  <* FATAL ANY *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
  END error;

PROCEDURE show_function (obj: ObjModule; addr: INTEGER): BOOLEAN =
  VAR
    findproc     : SortedIntRefTbl.Iterator;
    findline     : SortedIntIntTbl.Iterator;
    line,
    doubleunder,
    label        : INTEGER;
    dummy        : REFANY;
    proc         : ProcLabel;
    func,
    nametext     : TEXT;
  BEGIN
    IF addr <= obj.segment[SegType.Text].address OR
       addr > (obj.segment[SegType.Text].address + obj.segsize[SegType.Text])
       THEN
      RETURN FALSE;
    END;

    findproc := obj.labels.iterateOrdered(up := FALSE);
    findproc.seek(addr-1);
    IF NOT findproc.next(label, dummy) THEN
      RETURN TRUE;
    END;

    proc := dummy;

    IO.Put(Fmt.Pad(Fmt.Unsigned(addr), 8, '0'));
    IO.Put(" ");
    nametext := M3ID.ToText(proc.name);
    doubleunder := Text.FindChar(nametext, '_', 1);
    IF doubleunder # -1 AND doubleunder < (Text.Length(nametext)-1) AND
       Text.GetChar(nametext, doubleunder+1) = '_' THEN
      func := Text.Sub(nametext, 1, doubleunder-1) & "." &
                Text.Sub(nametext, doubleunder+2);
    ELSIF Text.Equal(Text.Sub(nametext, 0, 6), "__INIT") THEN
      func := "main body of " & Text.Sub(nametext, 8);
      IF Text.GetChar(nametext, 6) = 'M'
        THEN func := func & ".m3";
        ELSE func := func & ".i3";
      END;
    ELSE
      func := Text.Sub(nametext, 1);
    END;

    IO.Put(Fmt.Pad(func, 40, ' ', Fmt.Align.Left));

    IF proc.lines # NIL THEN
      findline := proc.lines.iterateOrdered(up := FALSE);
      findline.seek(addr-1);
      IF findline.next(label, line) THEN
        IO.Put("line ");
        IO.PutInt(line+proc.baseline);
      END
    END;
    IO.Put("\n");

    RETURN TRUE;
  END show_function;

PROCEDURE fixup (obj: ObjModule) RAISES {M3Loader.LoadError} =
  BEGIN
    obj.importtbl := NEW(IntIntTbl.Default);
    obj.importtbl := obj.importtbl.init(NUMBER(obj.imports^));
    obj.symreloctable := NEW(REF ARRAY OF RelocIndex, NUMBER(obj.imports^));

    FOR i := FIRST(obj.imports^) TO LAST(obj.imports^) DO
      IF obj.importtbl.put(obj.imports[i], i) THEN
        error("Bad object " & M3ID.ToText(obj.name) & " imports " &
                M3ID.ToText(obj.imports[i]) & " more than once");
        RAISE M3Loader.LoadError;
      END
    END;

    sort_relocs(obj);
  END fixup;

PROCEDURE sort_relocs (obj: ObjModule) RAISES {M3Loader.LoadError} =
  PROCEDURE cmprelocs(a, b: INTEGER): [-1 .. 1] =
    BEGIN
      WITH aname = obj.relocs[a].symbol,
           bname = obj.relocs[b].symbol DO
        IF aname > bname THEN RETURN 1; END;
        IF aname < bname THEN RETURN -1; END;
        RETURN 0;
      END
    END cmprelocs;

  VAR
    cursymplace : INTEGER;
    first       := 0;
    currentsym  : M3ID.T;
  BEGIN
    IF obj.nrelocs = 0 THEN RETURN END;

    obj.relocindex := NEW(REF ARRAY OF INTEGER, obj.nrelocs);
    FOR i := 0 TO obj.nrelocs-1 DO
      obj.relocindex[i] := i;
    END;

    IntArraySort.Sort(obj.relocindex^, cmprelocs);

    currentsym := obj.relocs[obj.relocindex[0]].symbol;

    FOR i := 1 TO obj.nrelocs DO
      IF i = obj.nrelocs OR obj.relocs[obj.relocindex[i]].symbol # currentsym
         THEN
        IF NOT obj.importtbl.get(currentsym, cursymplace) THEN
          error("Bad object " & M3ID.ToText(obj.name) &
                  " tried to relocate " & M3ID.ToText(currentsym) &
                  " but didn't import it");
          RAISE M3Loader.LoadError;
        END;

        obj.symreloctable[cursymplace].start := first;
        obj.symreloctable[cursymplace].end := i-1;

        IF i < obj.nrelocs THEN
          first := i;
          currentsym := obj.relocs[obj.relocindex[i]].symbol;
        END
      END
    END
  END sort_relocs;

PROCEDURE discard (obj: ObjModule; process: M3LoaderProcess.T) =
  BEGIN
    IF NOT obj.loaded THEN
      error("Discard error: " & M3ID.ToText(obj.name) & " not loaded");
      <* ASSERT FALSE *>
    END;

    obj.loaded := FALSE;

    FOR i := FIRST(SegType) TO LAST(SegType) DO
      TRY
        process.free(obj.segment[i]);
      EXCEPT
        M3LoaderProcess.AllocateError =>
          error("Internal error: discard free failed segment " &
                  Fmt.Int(ORD(i)));
          <* ASSERT FALSE *>
      END
    END
  END discard;

PROCEDURE relocate (obj: ObjModule; addr: INTEGER; sympos: INTEGER;
                    load: BOOLEAN) =
  BEGIN
    FOR i := obj.symreloctable[sympos].start TO
             obj.symreloctable[sympos].end DO
      WITH reloc = obj.relocs[obj.relocindex[i]] DO
        M3LoaderAccess.do_reloc(obj.segment[reloc.seg],
                                reloc.offs,
                                addr,
                                reloc.type,
                                load);
      END
    END
  END relocate;

BEGIN
END M3LoaderObj.
