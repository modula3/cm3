(* Copyright © 1993, Digital Equipment Corporation                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 17 09:32:24 PST 1996 by mhb                      *)
(*      modified on Tue Jan 31 11:11:58 PST 1995 by kalsow                   *)
(*      modified on Tue Sep  6 10:17:55 PDT 1994 by bharat                   *)
(*      modified on Sat Jun 26 16:41:30 PDT 1993 by steveg                   *)
(*      modified on Tue Jun 15 16:40:02 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 22:56:07 PDT 1992 by muller                   *)

MODULE FVRuntime EXPORTS FVRuntime, FVTypes, FormsVBT;

(* This module contains the runtime code for FormsVBTs. *)

IMPORT AnchorSplit, AnchorHelpSplit, AnyEvent, Atom, AudioVBT, Axis,
       BooleanVBT, BorderedVBT, ButtonVBT, ChoiceVBT, Color, ColorName,
       Cursor, FileBrowserVBT, FileRd, Filter, FlexVBT,
       FormsVBTPixmapsBundle, Fmt, Font, GuardedBtnVBT, HighlightVBT,
       HVSplit, IO, IP, Jva, JVSink, ListVBT, Macro, MenuSwitchVBT,
       MultiClass, MultiSplit, NumericVBT, OSError, PaintOp, Pathname,
       Pixmap, PixmapVBT, Pts, Rd, RdUtils, ReactivityVBT, RefList, Rsrc,
       RTTypeSRC, ScaleFilter, ScrollerVBT, Shadow, ShadowedVBT,
       ShadowedFeedbackVBT, SortedTextRefTbl, SourceVBT, Split,
       SplitterVBT, SwitchVBT, Sx, Text, TextEditVBT, TextPort,
       TextPortClass, TextRd, TextureVBT, TextVBT, TextWr, Thread,
       TrillSwitchVBT, TSplit, TextIntTbl, TypeinVBT, TypescriptVBT, VBT,
       VBTClass, Web, Wr, ZChassisVBT, ZChildVBT, ZSplit, ZSplitUtils;
IMPORT StubImageRd AS ImageRd;
IMPORT StubImageVBT AS ImageVBT;

FROM RefListUtils IMPORT Push, Pop;

<* PRAGMA LL *>

REVEAL
  T = Private BRANDED OBJECT
        mu: MUTEX;
        <* LL = mu *>
        getVBT     : SortedTextRefTbl.T;
        eventCount : CARDINAL              := 0;
        keyRec     : REF VBT.KeyRec;
        mouseRec   : REF VBT.MouseRec;
        positionRec: REF VBT.PositionRec;
        miscRec    : REF VBT.MiscRec;
        eventCode  : CARDINAL              := 0; (* typecode of event *)
        timeStamp  : VBT.TimeStamp;
        gensym                             := 0;
        raw                                := FALSE;
      OVERRIDES
        init         := InitFromText;
        initFromFile := InitFromFile;
        initFromSx   := InitFromSx;
        initFromRd   := InitFromRd;
        initFromRsrc := InitFromRsrc;
        initFromURL  := InitFromURL;
        snapshot     := Snapshot;
        restore      := Restore;
      END;

VAR cleanState: State;            (* CONST *)


(*************************** Creation *******************************)
  
PROCEDURE NewFromFile (filename: TEXT; raw := FALSE; path: Rsrc.Path := NIL): T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN NEW (T).initFromFile (filename, raw, path)
  END NewFromFile;

PROCEDURE InitFromFile (fv      : T;
                        filename: TEXT;
                        raw                 := FALSE;
                        path    : Rsrc.Path := NIL    ): T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open (filename);
      TRY RETURN InitFromRd (fv, rd, raw, path) FINALLY Rd.Close (rd) END
    EXCEPT
    | OSError.E (code) => RAISE Error (Atom.ToText (code.head))
    END
  END InitFromFile;


PROCEDURE InitFromText (fv         : T;
                        description: TEXT;
                        raw                    := FALSE;
                        path       : Rsrc.Path := NIL    ): T RAISES {Error} =
  <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    RETURN InitFromRd (fv, TextRd.New (description), raw, path)
  END InitFromText;


PROCEDURE InitFromRd (fv: T; rd: Rd.T; raw := FALSE; path: Rsrc.Path := NIL):
  T RAISES {Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN FromRd(fv, rd, raw, path)
  END InitFromRd;

PROCEDURE FromRd (fv: T; rd: Rd.T; raw := FALSE; path: Rsrc.Path := NIL; baseURL: TEXT :=NIL): T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE Thread.Join(Thread.Fork(NEW(ReaderClosure, rd := rd,
                                         stackSize := 10000))) OF
    | ReaderClosure (rc) =>
        CASE rc.errType OF
        | ErrType.ReadError => RAISE Error("Sx.ReadError: " & rc.errArg)
        | ErrType.EndOfFile => RAISE Error("End of input")
        | ErrType.Failure => RAISE Rd.Failure(rc.errArg)
        | ErrType.Alerted => RAISE Thread.Alerted
        END
    | REFANY (desc) => RETURN FromSx(fv, desc, raw, path, baseURL)
    END
  END FromRd;

TYPE
  ReaderClosure = Thread.SizedClosure OBJECT
                    rd     : Rd.T;
                    errType: ErrType;
                    errArg : REFANY
                  OVERRIDES
                    apply := Read
                  END;
  ErrType = {ReadError, EndOfFile, Failure, Alerted};
 
PROCEDURE Read (rc: ReaderClosure): REFANY =
  VAR
    exp  : REFANY;
    gotIt         := FALSE;
  BEGIN
    TRY
      exp := Sx.Read (rc.rd, syntax := FVSyntax);
      gotIt := TRUE;
      IF Rd.EOF (rc.rd) THEN RETURN exp END;
      (* Check for extra garbage: *)
      EVAL Sx.Read (rc.rd, syntax := FVSyntax);
      RAISE Sx.ReadError ("extra characters on input")
    EXCEPT
    | Sx.ReadError (Text) => rc.errArg := Text; rc.errType := ErrType.ReadError
    | Rd.EndOfFile =>
        IF gotIt THEN RETURN exp END;
        rc.errType := ErrType.EndOfFile
    | Rd.Failure (ref) => rc.errArg := ref; rc.errType := ErrType.Failure
    | Thread.Alerted => rc.errType := ErrType.Alerted
    END;
    (* If there's an error, we return the ReaderClosure itself. *)
    RETURN rc
  END Read;
               
PROCEDURE InitFromRsrc (fv: T; name: TEXT; path: Rsrc.Path; raw := FALSE): T
  RAISES {Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted} =
  VAR rd: Rd.T;
  BEGIN
    rd := Rsrc.Open (name, path);
    TRY RETURN InitFromRd (fv, rd, raw, path) FINALLY Rd.Close (rd) END
  END InitFromRsrc;


PROCEDURE InitFromURL (fv: T; baseURL: TEXT; raw := FALSE): T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR rd := OpenURL(baseURL);
  BEGIN
    TRY
      RETURN FromRd(fv, rd, raw := raw, path := NIL, baseURL := baseURL)
    FINALLY
      Rd.Close(rd)
    END
  END InitFromURL;

PROCEDURE InitFromSx (fv         : T;
                      description: Sx.T;
                      raw                    := FALSE;
                      path       : Rsrc.Path := NIL): T
  RAISES {Error} =
  BEGIN
    RETURN FromSx(fv, description, raw, path)
  END InitFromSx;

PROCEDURE FromSx (fv         : T;
                  description: Sx.T;
                  raw                    := FALSE;
                  path       : Rsrc.Path := NIL;
                  baseURL    : TEXT      := NIL    ): T RAISES {Error} =
  VAR
    state        := cleanState;
    ch   : VBT.T;
  BEGIN
    fv.getVBT := NEW(SortedTextRefTbl.Default).init();
    fv.mu := NEW(MUTEX);
    fv.keyRec := NEW(REF VBT.KeyRec);
    fv.mouseRec := NEW(REF VBT.MouseRec);
    fv.positionRec := NEW(REF VBT.PositionRec);
    fv.miscRec := NEW(REF VBT.MiscRec);
    fv.path :=
      RefList.Append(path, RefList.List1(FormsVBTPixmapsBundle.Get()));
    fv.baseURL := baseURL;
    fv.raw := raw;
    MultiClass.Be(fv, NEW(MC));
    state.menubar := NEW(VBT.T);
    IF raw THEN
      (* fv = (Filter parsedVBT) *)
      ch := Parse(fv, description, state);
      EVAL Filter.T.init(fv, ch);
    ELSE
      (* fv = (Filter (ZSplit (Highlight (Reactivity parsedVBT)))) *)
      (* The trick here is that state.zsplit must already be set BEFORE we
         parse the description. *)
      WITH react     = NEW(FVFilter),
           highlight = NEW(HighlightVBT.T).init(react),
           zsplit    = NEW(ZSplit.T).init(highlight)    DO
        EVAL Filter.T.init(fv, zsplit);
        state.zsplit := zsplit;
        ch := Parse(fv, description, state);
        EVAL react.init(ch)
      END
    END;
    MultiClass.BeChild(fv, ch);
    RETURN fv
  END FromSx;


TYPE
  MC = MultiClass.Filter OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace
       END;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  <* FATAL Split.NotAChild *>
  VAR fv: T := m.vbt;
  BEGIN
    IF fv.raw THEN
     EVAL  Filter.Replace (fv, new)
    ELSE
      WITH zsplit    = Filter.Child (fv),
           highlight = Split.Succ (zsplit, NIL),
           react     = Filter.Child (highlight) DO
        EVAL Filter.Replace (react, new)
      END
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  <* FATAL Split.NotAChild *>
  VAR fv: T := m.vbt;
  BEGIN
    IF ch # NIL THEN
      RETURN NIL
    ELSIF fv.raw THEN
      RETURN Filter.Child (fv)
    ELSE
      WITH zsplit    = Filter.Child (fv),
           highlight = Split.Succ (zsplit, NIL),
           react     = Filter.Child (highlight) DO
        RETURN Filter.Child (react)
      END
    END
  END Succ;


PROCEDURE GetZSplit (fv: T): ZSplit.T RAISES {Error} =
  BEGIN
    IF fv.raw THEN RAISE Error ("Uncooked FormsVBT (GetZSplit)") END;
    RETURN Filter.Child (fv)
  END GetZSplit;

PROCEDURE Insert (fv         : T;
                  parent     : TEXT;
                  description: TEXT;
                  at         : CARDINAL := LAST (CARDINAL)): VBT.T
  RAISES {Error} =
  VAR
    stateRef: REF State := VBT.GetProp (
                             GetVBT (fv, parent), TYPECODE (REF State));
    res: VBT.T;
    rd         := TextRd.New (description);
  BEGIN
    TRY
      res := Parse (fv, Sx.Read (rd, syntax := FVSyntax), stateRef^);
      Rd.Close (rd);
      InsertVBT (fv, parent, res, at);
      RETURN res
    EXCEPT
    | Sx.ReadError (text) => RAISE Error ("Sx.ReadError: " & text)
    | Rd.EndOfFile => RAISE Error ("End of input")
    | Rd.Failure =>              <* ASSERT FALSE *>
    | Thread.Alerted => RAISE Error ("Thread.Alerted")
    END
  END Insert;
  
PROCEDURE InsertFromFile (fv      : T;
                          parent  : TEXT;
                          filename: Pathname.T;
                          at      : CARDINAL     := LAST (CARDINAL)): VBT.T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open (filename);
      TRY
        RETURN Insert (fv, parent, Rd.GetText (rd, LAST (CARDINAL)), at)
      FINALLY
        Rd.Close (rd)
      END
    EXCEPT
    | OSError.E (code) => RAISE Error (Atom.ToText (code.head))
    END
  END InsertFromFile;

PROCEDURE InsertFromRsrc (fv    : T;
                          parent: TEXT;
                          name  : TEXT;
                          path  : Rsrc.Path;
                          n     : CARDINAL    := LAST (CARDINAL)): VBT.T
  RAISES {Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted} =
  BEGIN
    RETURN Insert (fv, parent, Rsrc.Get (name, path), n)
  END InsertFromRsrc;


(*************************** URLs *******************************)

PROCEDURE Open (name: TEXT; path: Rsrc.Path; baseURL: TEXT): Rd.T
  RAISES {Error} =
  BEGIN
    IF baseURL # NIL THEN
      RETURN OpenURL(name, baseURL)
    ELSE
      TRY
        RETURN Rsrc.Open(name, path)
      EXCEPT
        Rsrc.NotFound => RAISE Error("No such resource: " & name)
      END
    END
  END Open;


PROCEDURE OpenURL (url: TEXT; base: TEXT := NIL): Rd.T RAISES {Error} =
  VAR
    rd    : Rd.T;
    header: Web.Header;
  BEGIN
    IF base # NIL THEN url := Web.AbsoluteURL(url, base) END;
    TRY
      rd := Web.Get(url, header)
    EXCEPT
      Thread.Alerted, IP.Error, Web.Error =>
        RAISE Error("Cannot access url:" & url)
    END;
    IF header.statusCode = 200 THEN
      RETURN rd
    ELSE
      RAISE Error("Bad http status code accessing url: " & url & " ("
                    & Fmt.Int(header.statusCode) & ")")
    END
  END OpenURL;



(*************************** snapshots *******************************)

PROCEDURE GetVal (fv: T; name: TEXT): REFANY =
  (* Returns value of name as REFANY, if a value can be
     retrieved *)
  BEGIN
    TRY
      WITH ri = NEW(REF INTEGER) DO
        ri^ := GetInteger(fv, name);
        RETURN ri
      END
    EXCEPT
      Error, Unimplemented =>
    END;
    TRY
      WITH rb = NEW(REF BOOLEAN) DO
        rb^ := GetBoolean(fv, name);
        IF rb^ THEN RETURN Sx.True ELSE RETURN Sx.False END;
      END
    EXCEPT
      Error, Unimplemented =>
    END;
    TRY
      RETURN GetText(fv, name);
    EXCEPT
      Error, Unimplemented =>
    END;
    RETURN NIL
  END GetVal;

PROCEDURE Snapshot (fv: T; wr: Wr.T) RAISES {Error} =
  VAR
    key        : TEXT;
    val, ignore: REFANY;
    iter                   := fv.getVBT.iterateOrdered ();
    alist: RefList.T := NIL;
  BEGIN
    TRY
      WHILE iter.next (key, ignore) DO
        val := GetVal (fv, key);
        IF val # NIL THEN
          Push (alist, RefList.List2 (Atom.FromText (key), val))
        END
      END;
      Sx.Print (wr, alist);
      Wr.PutChar (wr, '\n')
    EXCEPT
    | Sx.PrintError, Thread.Alerted, Wr.Failure =>
        RAISE Error ("Problem writing snapshot");
    END
  END Snapshot;

PROCEDURE Restore (fv: T; rd: Rd.T) RAISES {Mismatch, Error} =
  VAR
    mismatch          := FALSE;
    ignoreRef: REFANY;
    name     : TEXT;
  BEGIN
    TRY
      TYPECASE Sx.Read(rd) OF
      | NULL =>
      | RefList.T (sx) =>
          WHILE sx # NIL DO
            TYPECASE sx.head OF
            | RefList.T (l) =>
                IF RefList.Length(l) # 2 THEN
                  RAISE Error("Illegal expression in snapshot")
                END;
                TYPECASE l.head OF
                | Atom.T (sym) =>
                    name := Atom.ToText(sym);
                    IF NOT fv.getVBT.get(name, ignoreRef) THEN
                      mismatch := TRUE
                    ELSE
                      TYPECASE l.tail.head OF
                      | TEXT (text) => PutText(fv, name, text)
                      | REF BOOLEAN (refBool) =>
                          PutBoolean(fv, name, refBool^)
                      | REF INTEGER (refInt) =>
                          PutInteger(fv, name, refInt^)
                      | Atom.T (atm) =>
                          IF atm = Sx.True THEN
                            PutBoolean(fv, name, TRUE)
                          ELSIF atm = Sx.False THEN
                            PutBoolean(fv, name, FALSE)
                          ELSE
                            RAISE Error("Value of component "
                                          & Atom.ToText(sym)
                                          & " has illegal value: "
                                          & Atom.ToText(atm));
                          END;
                      ELSE
                        RAISE
                          Error("Value of component " & Atom.ToText(sym)
                                  & " has illegal type");
                      END        (* TYPECASE *)
                    END          (* IF *)
                ELSE
                  RAISE Error("Illegal component name in snapshot");
                END              (* TYPECASE *)
            ELSE
              RAISE Error("Snapshot is not a valid s-expression");
            END;                 (* TYPECASE *)
            sx := sx.tail
          END;                   (* WHILE *)
          IF mismatch THEN RAISE Mismatch END;
      ELSE
        RAISE Error("Snapshot is not a valid s-expression")
      END                        (* TYPECASE *)
    EXCEPT
    | Sx.ReadError, Rd.EndOfFile, Thread.Alerted, Unimplemented =>
        RAISE Error("Problem with reading snapshot")
    END
  END Restore;


(* ========================= Attachment ========================= *)

TYPE ClosureRef = BRANDED REF RECORD fv: T; name: TEXT; cl: Closure END;

PROCEDURE Attach (fv: T; name: TEXT; cl: Closure) RAISES {Error} =
  VAR vbt := GetVBT (fv, name);
  BEGIN
    TYPECASE vbt OF
    | FVBoolean, FVBrowser, FVButton, FVChoice, FVCloseButton, FVFileBrowser,
        FVGuard, FVLinkButton, FVLinkMButton, FVMButton, FVMenu,
        FVMultiBrowser, FVNumeric, FVPageButton, FVPageMButton, FVPopButton,
        FVPopMButton, FVRadio, FVScroller, FVSource,
        FVTextEdit, FVTrillButton, FVTypeIn,
        FVZChassis, FVZChild, ReservedVBT =>
        IF cl # NIL THEN
          VBT.PutProp (
            vbt, NEW (ClosureRef, fv := fv, name := name, cl := cl))
        ELSE
          VBT.RemProp (vbt, TYPECODE (ClosureRef))
        END
    ELSE
      RAISE Error ("The component named \"" & name
                     & "\" does not generate events.")
    END;
  END Attach;

PROCEDURE MouseProc (self: VBT.T; READONLY cd: VBT.MouseRec) =
  (* This is the callback, directly or indirectly, for all the components that
     generate events (see Attach) except TypeIn, TextEdit, and Numeric, 
     which handle attachment directly and using KeyProc. *)
  VAR
    cr: ClosureRef := VBT.GetProp (self, TYPECODE (ClosureRef));
    fv: T;
  BEGIN
    IF cr # NIL THEN
      fv := cr.fv;
      LOCK fv.mu DO
        INC (fv.eventCount);
        IF fv.eventCount = 1 THEN
          fv.mouseRec^ := cd;
          fv.eventCode := TYPECODE (REF VBT.MouseRec)
        END
      END;
      TRY
        cr.cl.apply (fv, cr.name, cd.time)
      FINALLY
        LOCK fv.mu DO DEC (fv.eventCount) END
      END;
    END
  END MouseProc;

PROCEDURE KeyProc (self: VBT.T; READONLY cd: VBT.KeyRec) =
  VAR
    cr: ClosureRef := VBT.GetProp (self, TYPECODE (ClosureRef));
    fv: T;
  BEGIN
    IF cr # NIL THEN
      fv := cr.fv;
      LOCK fv.mu DO
        INC (fv.eventCount);
        IF fv.eventCount = 1 THEN
          fv.keyRec^ := cd;
          fv.eventCode := TYPECODE (REF VBT.KeyRec)
        END
      END;
      TRY
        cr.cl.apply (fv, cr.name, cd.time)
      FINALLY
        LOCK fv.mu DO DEC (fv.eventCount) END
      END
    END
  END KeyProc;

TYPE
  OldClosure = Closure OBJECT
                 ref : REFANY;
                 proc: Proc
               OVERRIDES
                 apply := OldApply
               END;
                    
PROCEDURE AttachProc (fv: T; name: TEXT; proc: Proc; cl: REFANY := NIL)
  RAISES {Error} =
  BEGIN
    IF proc # NIL THEN
      Attach (fv, name, NEW (OldClosure, ref := cl, proc := proc))
    ELSE
      Attach (fv, name, NIL)
    END
  END AttachProc;
  
PROCEDURE OldApply (oc: OldClosure; fv: T; name: TEXT; time: VBT.TimeStamp) =
  BEGIN
    oc.proc (fv, name, oc.ref, time)
  END OldApply;

(* ========================= Edit Ops ========================= *)

TYPE
  C = Closure OBJECT
        port: TextPort.T;
        op  : Op
      OVERRIDES
        apply := ApplyEditOp
      END;
  Op = {cut, copy, paste, clear, selectAll, undo, redo, first, next, prev};

PROCEDURE AttachEditOps (fv        : T;
                         editorName: TEXT;
                         cut, copy, paste, clear,
                         selectAll, undo, redo,
                         findFirst, findNext, findPrev: TEXT := NIL)
  RAISES {Error} =
  VAR port: TextPort.T := NIL;
  BEGIN
    TYPECASE GetVBT (fv, editorName) OF
    | NULL =>
    | FVTextEdit (vbt) => port := vbt.tp
    | FVNumeric (vbt) => port := vbt.typein
    | FVTypescript (vbt) => port := vbt.tp
    | FVTypeIn (vbt) => port := vbt
    ELSE
    END;
    IF port = NIL THEN
      RAISE
        Error ("There's no TextPort in the component named \"" &
          editorName & "\"")
    END;
    IF cut # NIL THEN
      Attach (fv, cut, NEW (C, port := port, op := Op.cut))
    END;
    IF copy # NIL THEN
      Attach (fv, copy, NEW (C, port := port, op := Op.copy))
    END;
    IF paste # NIL THEN
      Attach (fv, paste, NEW (C, port := port, op := Op.paste))
    END;
    IF clear # NIL THEN
      Attach (fv, clear, NEW (C, port := port, op := Op.clear))
    END;
    IF selectAll # NIL THEN
      Attach (fv, selectAll, NEW (C, port := port, op := Op.selectAll))
    END;
    IF undo # NIL THEN
      Attach (fv, undo, NEW (C, port := port, op := Op.undo))
    END;
    IF redo # NIL THEN
      Attach (fv, redo, NEW (C, port := port, op := Op.redo))
    END;
    IF findFirst # NIL THEN
      Attach (fv, findFirst, NEW (C, port := port, op := Op.first))
    END;
    IF findNext # NIL THEN
      Attach (fv, findNext, NEW (C, port := port, op := Op.next))
    END;
    IF findPrev # NIL THEN
      Attach (fv, findPrev, NEW (C, port := port, op := Op.prev))
    END;
  END AttachEditOps; 

PROCEDURE ApplyEditOp (             cl  : C;
                       <* UNUSED *> fv  : T;
                       <* UNUSED *> name: TEXT;
                                    time: VBT.TimeStamp) =
  VAR
    port := cl.port;
    m    := port.m;
  BEGIN
    LOCK port.mu DO
      CASE cl.op OF
      | Op.cut => m.cut (time)
      | Op.copy => m.copy (time)
      | Op.paste => m.paste (time)
      | Op.clear => m.clear ()
      | Op.selectAll => 
            m.select (time, 0, LAST (CARDINAL), replaceMode := TRUE)
      | Op.undo => TextPortClass.Undo (port)
      | Op.redo => TextPortClass.Redo (port)
      | Op.first => port.findSource (time, TextPortClass.Loc.First)
      | Op.next => port.findSource (time, TextPortClass.Loc.Next)
      | Op.prev => port.findSource (time, TextPortClass.Loc.Prev)
      END
    END
  END ApplyEditOp;



TYPE ReservedVBT = VBT.Leaf BRANDED OBJECT END;

PROCEDURE AddSymbol (fv: T; name: TEXT) RAISES {Error} =
  VAR ref: REFANY;
  BEGIN
    LOCK fv.mu DO
      IF fv.getVBT.get (name, ref) THEN
        RAISE Error ("The name " & name & " is already in use.")
      ELSE
        EVAL fv.getVBT.put (name, NEW (ReservedVBT))
      END
    END
  END AddSymbol;

PROCEDURE AddUniqueSymbol (fv: T): TEXT =
  VAR
    ref : REFANY;
    name: TEXT;
  BEGIN
    LOCK fv.mu DO
      LOOP
        name := "-v-b-t-" & Fmt.Int (fv.gensym);
        IF fv.getVBT.get (name, ref) THEN INC (fv.gensym) ELSE EXIT END
      END;
      EVAL fv.getVBT.put (name, NEW (ReservedVBT));
      RETURN name
    END
  END AddUniqueSymbol;


(* ===================== MakeEvent & GetTheEvent ==================== *)

VAR MakeEventSelection: VBT.Selection; (* CONST *)

PROCEDURE MakeEvent (fv: T; name: TEXT; time: VBT.TimeStamp) RAISES {Error} =
  <* LL = VBT.mu *>
  VAR
    vbt                    := GetVBT (fv, name);
    cr        : ClosureRef := VBT.GetProp (vbt, TYPECODE (ClosureRef));
    popTarget : PopTarget  := VBT.GetProp (vbt, TYPECODE (PopTarget));
    pageTarget: PageTarget := VBT.GetProp (vbt, TYPECODE (PageTarget));
    linkTarget: LinkTarget := VBT.GetProp (vbt, TYPECODE (LinkTarget));
  BEGIN
    IF cr = NIL AND popTarget = NIL AND pageTarget = NIL AND linkTarget = NIL THEN
      RAISE Error ("Nothing attached to " & name)
    END;
    IF popTarget # NIL THEN
      popTarget.apply (time)
    ELSIF pageTarget # NIL THEN
      pageTarget.apply (time)
    ELSIF linkTarget # NIL THEN
      linkTarget.apply (time)
    END;
    IF cr # NIL THEN
      LOCK fv.mu DO
        INC (fv.eventCount);
        IF fv.eventCount = 1 THEN
          fv.miscRec.type := MakeEventMiscCodeType;
          fv.miscRec.time := time;
          fv.miscRec.selection := MakeEventSelection;
          fv.eventCode := TYPECODE (REF VBT.MiscRec)
        END
      END;
      TRY
        cr.cl.apply (cr.fv, cr.name, time)
      FINALLY
        LOCK fv.mu DO DEC (fv.eventCount) END
      END
    END
  END MakeEvent;

PROCEDURE GetTheEvent (fv: T): AnyEvent.T RAISES {Error} =
  VAR tc: CARDINAL;
  BEGIN
    LOCK fv.mu DO
      tc := fv.eventCode;
      IF fv.eventCount = 0 THEN
        RAISE Error ("There is no active event")
      (*
      ELSIF fv.eventCount > 1 THEN
        RAISE Error ("More than 1 event is active")
      *)
      ELSIF tc = TYPECODE (REF VBT.KeyRec) THEN
        RETURN AnyEvent.FromKey (fv.keyRec^)
      ELSIF tc = TYPECODE (REF VBT.MouseRec) THEN
        RETURN AnyEvent.FromMouse (fv.mouseRec^)
      ELSIF tc = TYPECODE (REF VBT.PositionRec) THEN
        RETURN AnyEvent.FromPosition (fv.positionRec^)
      ELSIF tc = TYPECODE (REF VBT.MiscRec) THEN
        RETURN AnyEvent.FromMisc (fv.miscRec^)
      ELSE
        RAISE
          Error ("Internal error: The active event has an unknown type")
      END
    END
  END GetTheEvent;

PROCEDURE GetTheEventTime (fv: T): VBT.TimeStamp RAISES {Error} =
  VAR tc: CARDINAL;
  BEGIN
    LOCK fv.mu DO
      IF fv.eventCount = 0 THEN RETURN 0 END;
      tc := fv.eventCode;
      IF tc = TYPECODE (REF VBT.KeyRec) THEN
        RETURN fv.keyRec.time
      ELSIF tc = TYPECODE (REF VBT.MouseRec) THEN
        RETURN fv.mouseRec.time
      ELSIF tc = TYPECODE (REF VBT.PositionRec) THEN
        RETURN fv.positionRec.time
      ELSIF tc = TYPECODE (REF VBT.MiscRec) THEN
        RETURN fv.miscRec.time
      ELSE
        RAISE Error ("Internal error: The active event has an unknown type")
      END
    END
  END GetTheEventTime;

(************************ Text edit-widget callback ************************)

REVEAL 
  Port = PublicPort BRANDED OBJECT
    textedit: FVTextEdit;
    reportKeys: BOOLEAN;
  OVERRIDES
    init := PortInit;
    filter := PortFilter;
  END;

PROCEDURE PortInit (v: Port;
    textedit: FVTextEdit; 
    reportKeys: BOOLEAN;
    font: Font.T;
    colorScheme: PaintOp.ColorScheme;
    wrap, readOnly: BOOLEAN; 
    turnMargin: REAL): Port = 
  BEGIN
    v.textedit := textedit;
    v.reportKeys := reportKeys;
    RETURN TextPort.T.init(v, font := font, colorScheme := colorScheme,
                readOnly := readOnly, wrap := wrap,
                turnMargin := turnMargin)
  END PortInit;

PROCEDURE PortFilter (v: Port; cd: VBT.KeyRec) = 
  BEGIN
    IF NOT v.reportKeys THEN
      TextPort.T.filter (v, cd)
    ELSE
      WITH len = TextPort.Length (v), text = TextPort.GetText(v) DO
        TextPort.T.filter (v, cd);
        IF len = TextPort.Length (v) THEN
           IF Text.Equal (text, TextPort.GetText (v)) THEN RETURN END
        END
      END;
      KeyProc (v.textedit, cd)
    END
  END PortFilter;


(************************ Typein-widget callback ************************)

REVEAL
  FVTypeIn =
    TypeinVBT.T BRANDED OBJECT OVERRIDES returnAction := DeliverText END;

PROCEDURE DeliverText (typein: TypeinVBT.T; READONLY cd: VBT.KeyRec) =
  (* Callback for our TypeIns. *)
  BEGIN
    IF VBT.GetProp (typein, TYPECODE (ClosureRef)) = NIL THEN
      TypeinVBT.T.returnAction (typein, cd)
    ELSE
      KeyProc (typein, cd)
    END
  END DeliverText;
  

(* ====================== Pixmap ===================== *)

REVEAL 
  FVImage = PrivateImage BRANDED OBJECT
    OVERRIDES
      shape := ImageShape;
    END;

PROCEDURE ImageShape(v: PrivateImage; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
    (* LL = VBT.mu.v *)
  VAR sr := ImageVBT.T.shape(v, ax, n);
  BEGIN
    sr.hi := 99999;
    RETURN sr
  END ImageShape;


(* ====================== FileBrowser ===================== *)

REVEAL
  FVFileBrowser = FileBrowserVBT.T BRANDED OBJECT
                  OVERRIDES
                    activateFile := ActivateFileB
                  END;

PROCEDURE ActivateFileB (             self    : FVFileBrowser;
                         <* UNUSED *> filename: TEXT;
                                      event   : AnyEvent.T) =
  (* callback for our FileBrowserVBTs. *)
  VAR mr: VBT.MouseRec;
  BEGIN
    TYPECASE event OF
    | AnyEvent.Key (key) =>
        mr.time := key.key.time;
        MouseProc (self, mr);
    | AnyEvent.Mouse (mouse) => MouseProc (self, mouse.mouse)
    ELSE
    END;
  END ActivateFileB;


(* ====================== Browser ===================== *)

REVEAL
  UniSelector = PrivateUniSelector BRANDED OBJECT
                OVERRIDES
                  insideClick := InsideClick;
                  outsideClick := OutsideClick
                END;

PROCEDURE InsideClick (         v   : UniSelector;
                       READONLY cd  : VBT.MouseRec;
                                this: ListVBT.Cell  ) =
  BEGIN
    ListVBT.UniSelector.insideClick (v, cd, this);
    IF cd.clickType = VBT.ClickType.LastUp
         AND (v.quick OR cd.clickCount = 3) THEN
      MouseProc (v.browser, cd)
    END
  END InsideClick;

PROCEDURE OutsideClick (         v   : UniSelector;
                       READONLY cd  : VBT.MouseRec) =
  BEGIN
    ListVBT.UniSelector.outsideClick (v, cd);
    IF cd.clickType = VBT.ClickType.LastUp
         AND (v.quick OR cd.clickCount = 3) THEN
      MouseProc (v.browser, cd)
    END
  END OutsideClick;

REVEAL
  MultiSelector = PrivateMultiSelector BRANDED OBJECT
                  OVERRIDES
                    insideClick := MultiInsideClick;
                    outsideClick := MultiOutsideClick
                  END;

PROCEDURE MultiInsideClick (         v   : MultiSelector;
                            READONLY cd  : VBT.MouseRec;
                                     this: ListVBT.Cell   ) =
  BEGIN
    ListVBT.MultiSelector.insideClick (v, cd, this);
    IF cd.clickType = VBT.ClickType.LastUp
         AND (v.quick OR cd.clickCount = 3) THEN
      MouseProc (v.browser, cd)
    END
  END MultiInsideClick;

PROCEDURE MultiOutsideClick (         v   : MultiSelector;
                            READONLY cd  : VBT.MouseRec) =
  BEGIN
    ListVBT.MultiSelector.outsideClick (v, cd);
    IF cd.clickType = VBT.ClickType.LastUp
         AND (v.quick OR cd.clickCount = 3) THEN
      MouseProc (v.browser, cd)
    END
  END MultiOutsideClick;


(* ====================== Buttons ===================== *)

REVEAL
  FVBoolean = BooleanVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVButton = SwitchVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVGuard =
    GuardedBtnVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVMButton =
    MenuSwitchVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVScroller =
    ScrollerVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVSource = SourceVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVTrillButton =
    TrillSwitchVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;
  FVZChassis =
    ZChassisVBT.T BRANDED OBJECT OVERRIDES callback := MouseProc END;

(* ====================== Radio & Choice ===================== *)

REVEAL
  FVChoice =
    PrivateChoice BRANDED OBJECT OVERRIDES callback := ChoiceCallback END;

PROCEDURE ChoiceCallback (self: FVChoice; READONLY cd: VBT.MouseRec) =
  BEGIN
    MouseProc (self, cd);
    MouseProc (self.radio, cd)
  END ChoiceCallback;


(* ============================ FirstFocus =========================== *)

(* The following widgets play the first-focus game: Helper,
   Numeric (.typein), TextEdit (.tp), TypeIn, and TypeScript
   (.tp).  When a component of this type is encountered during
   the parsing of an s-expression, and if the FirstFocus property
   was set TRUE, SetFirstFocus is called to mark the VBT as
   having a TRUE FirstFocus property.  Later, when a TSplit or
   sub-window is made visible, FirstFocus is called to find a
   visible descendant that was marked as having a TRUE FirstFocus
   property. *)

TYPE FirstFocusProp = BRANDED REF INTEGER;

PROCEDURE SetFirstFocus (widget: VBT.T) =
  VAR prop := NEW(FirstFocusProp);
  BEGIN
    VBT.PutProp(widget, prop)
  END SetFirstFocus;

PROCEDURE FirstFocus (v: VBT.T; time: VBT.TimeStamp) =
  VAR
    widget: VBT.T;
    port  : TextPort.T;
  BEGIN
    widget := FindFocus(v);
    IF widget = NIL THEN RETURN END;
    TYPECASE widget OF
    | FVHelper (h) => port := h;
    | FVNumeric (n) => port := n.typein;
    | FVTextEdit (t) => port := t.tp;
    | FVTypeIn (t) => port := t;
    | FVTypescript (t) => port := t.tp;
    ELSE <* ASSERT FALSE *>
    END;
    IF NOT TextPort.TryFocus(port, time) THEN RETURN END;
    IF ISTYPE(port, TypeinVBT.T) THEN
      TextPort.Select(port, time, replaceMode := TRUE)
    END
  END FirstFocus;

PROCEDURE FindFocus (v: VBT.T): VBT.T =
  <* FATAL MultiSplit.NotAChild *>
  VAR ch, focus: VBT.T;
  BEGIN
    IF VBT.GetProp(v, TYPECODE(FirstFocusProp)) # NIL THEN
      RETURN v
    END;
    IF LeafVBT (v) THEN RETURN NIL END;
    IF ISTYPE(v, FVTSplit) THEN
      ch := TSplit.GetCurrent(v);
      IF ch # NIL THEN RETURN FindFocus(ch) END
    ELSE
      ch := MultiSplit.Succ(v, NIL);
      WHILE ch # NIL DO
        focus := FindFocus(ch);
        IF focus # NIL THEN RETURN focus END;
        ch := MultiSplit.Succ(v, ch);
      END
    END;
    RETURN NIL;
  END FindFocus;


(* ====================== PopButton & PopMButton ===================== *)
(* ========================== PopUp & PopDown ======================== *)

REVEAL
  FVPopButton =
    SwitchVBT.T BRANDED OBJECT OVERRIDES callback := PopButtonProc END;
  FVPopMButton =
    MenuSwitchVBT.T BRANDED OBJECT OVERRIDES callback := PopButtonProc END;
    
TYPE
  Callback = OBJECT METHODS apply (time: VBT.TimeStamp) END;
  PopTarget =
    Callback OBJECT target: ZChildVBT.T OVERRIDES apply := ApplyPopTarget END;

PROCEDURE SetPopTarget (source: ButtonVBT.T; target: ZChildVBT.T) =
  BEGIN
    VBT.PutProp (source, NEW (PopTarget, target := target))
  END SetPopTarget;

PROCEDURE PopButtonProc (self: VBT.T; READONLY cd: VBT.MouseRec) =
  (* Callback procedure for Pop[M]Button *)
  VAR popTarget: PopTarget := VBT.GetProp (self, TYPECODE (PopTarget));
  BEGIN
    IF popTarget # NIL THEN popTarget.apply (cd.time) END;
    MouseProc (self, cd)
  END PopButtonProc;

PROCEDURE ApplyPopTarget (p: PopTarget; time: VBT.TimeStamp) =
  BEGIN
    DoPopUp (p.target, forcePlace := FALSE, time := time);
  END ApplyPopTarget;

PROCEDURE PopUp (fv        : T;
                 name      : TEXT;
                 forcePlace: BOOLEAN       := FALSE;
                 time      : VBT.TimeStamp := 0      ) RAISES {Error} =
  VAR vbt := GetVBT (fv, name);
  BEGIN
    IF time = 0 THEN time := GetTheEventTime (fv) END;
    DoPopUp (vbt, forcePlace, time);
  END PopUp;

PROCEDURE DoPopUp (vbt: VBT.T; forcePlace: BOOLEAN; time: VBT.TimeStamp) =
  VAR zchild := ZSplitUtils.FindZChild (vbt);
  BEGIN
    IF zchild # NIL THEN
      ZChildVBT.Pop (zchild, forcePlace);
      FirstFocus (zchild, time)
    END
  END DoPopUp;

PROCEDURE PopDown (fv: T; name: TEXT) RAISES {Error} =
  VAR zchild := ZSplitUtils.FindZChild (GetVBT (fv, name));
  BEGIN
    IF zchild # NIL THEN ZSplit.Unmap (zchild) END
  END PopDown;

(* ===================== PageButton, PageMButton ============================ *)

TYPE
  PageTarget = Callback OBJECT
                 target   : FVTSplit;
                 backwards: BOOLEAN
               OVERRIDES
                 apply := ApplyPageTarget
               END;

REVEAL
  FVPageButton = PublicPageButton BRANDED OBJECT
                   backwards := FALSE
                 OVERRIDES
                   callback := PageButtonProc;
                   init     := InitPageButton
                 END;
  FVPageMButton = PublicPageMButton BRANDED OBJECT
                    backwards := FALSE
                  OVERRIDES
                    callback := PageButtonProc;
                    init     := InitPageMButton
                  END;

PROCEDURE InitPageButton (b        : FVPageButton;
                          ch       : VBT.T;
                          shadow   : Shadow.T;
                          backwards: BOOLEAN;
                          tsplit   : FVTSplit      ): FVPageButton =
  BEGIN
    EVAL SwitchVBT.T.init (b, NEW (ShadowedFeedbackVBT.T).init (ch, shadow));
    VBT.PutProp (
      b, NEW (PageTarget, backwards := backwards, target := tsplit));
    RETURN b
  END InitPageButton;

PROCEDURE InitPageMButton (b        : FVPageMButton;
                           ch       : VBT.T;
                           shadow   : Shadow.T;
                           backwards: BOOLEAN;
                           tsplit   : FVTSplit       ): FVPageMButton =
  BEGIN
    EVAL MenuSwitchVBT.T.init (b, ShadowedFeedbackVBT.NewMenu (ch, shadow));
    VBT.PutProp (
      b, NEW (PageTarget, backwards := backwards, target := tsplit));
    RETURN b
  END InitPageMButton;

PROCEDURE SetPageTarget (source: ButtonVBT.T; target: FVTSplit) =
  VAR p: PageTarget := VBT.GetProp (source, TYPECODE (PageTarget));
  BEGIN
    p.target := target
  END SetPageTarget;

PROCEDURE PageButtonProc (self: VBT.T; READONLY cd: VBT.MouseRec) =
  VAR p: PageTarget := VBT.GetProp (self, TYPECODE (PageTarget));
  BEGIN
    IF p # NIL THEN p.apply (cd.time) END;
    MouseProc (self, cd)
  END PageButtonProc;

PROCEDURE ApplyPageTarget (p: PageTarget; time: VBT.TimeStamp) =
  VAR
    tsplit         := p.target;
    current        := TSplit.GetCurrent (tsplit);
    next   : VBT.T;
  <* FATAL Split.NotAChild *>
  BEGIN
    IF p.backwards THEN
      next := Split.Pred (tsplit, current);
      IF next = NIL AND tsplit.circular THEN
        next := Split.Pred (tsplit, NIL)
      END
    ELSE
      next := Split.Succ (tsplit, current);
      IF next = NIL AND tsplit.circular THEN
        next := Split.Succ (tsplit, NIL)
      END
    END;
    IF next # NIL THEN
      TSplit.SetCurrent (tsplit, next);
      FirstFocus (next, time)
    END;
  END ApplyPageTarget;


(* ===================== LinkButton, LinkMButton ============================ *)

TYPE
  LinkTarget = Callback OBJECT
                 Tparent: FVTSplit;
                 Tchild : VBT.T
               OVERRIDES
                 apply := ApplyLinkTarget
               END;
REVEAL
  FVLinkButton = SwitchVBT.T BRANDED OBJECT
                 OVERRIDES
                   callback := LinkButtonProc
                 END;
  FVLinkMButton = MenuSwitchVBT.T BRANDED OBJECT
                  OVERRIDES
                    callback := LinkButtonProc
                  END;

PROCEDURE SetLinkTarget (source: ButtonVBT.T; target: VBT.T) =
  BEGIN
    VBT.PutProp (source, NEW (LinkTarget, Tchild := target,
                              Tparent := VBT.Parent (target)))
  END SetLinkTarget; 

PROCEDURE LinkButtonProc (self: VBT.T; READONLY cd: VBT.MouseRec) =
  VAR lt: LinkTarget := VBT.GetProp (self, TYPECODE (LinkTarget));
  BEGIN
    IF lt # NIL THEN lt.apply (cd.time) END;
    MouseProc (self, cd)
  END LinkButtonProc;

PROCEDURE ApplyLinkTarget (lt: LinkTarget; time: VBT.TimeStamp) =
  BEGIN
    TRY
      TSplit.SetCurrent (lt.Tparent, lt.Tchild);
      FirstFocus (lt.Tchild, time)
    EXCEPT
    | Split.NotAChild =>         (* ignore *)
    END
  END ApplyLinkTarget;

(* =========================== CloseButton ============================ *)

REVEAL
  FVCloseButton = PrivateCloseButton BRANDED OBJECT
                  OVERRIDES
                    callback := CloseButtonProc;
                    init     := InitCloseButton
                  END;

PROCEDURE InitCloseButton (b: FVCloseButton; ch: VBT.T; shadow: Shadow.T):
  FVCloseButton =
  BEGIN
    EVAL
      SwitchVBT.T.init (b, NEW (ShadowedFeedbackVBT.T).init (ch, shadow));
    RETURN b
  END InitCloseButton;

PROCEDURE CloseButtonProc (         self: FVCloseButton;
                           READONLY cd  : VBT.MouseRec   ) =
  VAR zch := ZSplitUtils.FindZChild (self.target);
  BEGIN
    IF zch # NIL THEN
      ZSplit.Unmap (zch);
      MouseProc (self, cd);
      MouseProc (zch, cd)
    END
  END CloseButtonProc;


(* ============================= HBox, VBox ============================== *)

REVEAL
  FVHBox = HVSplit.T BRANDED OBJECT OVERRIDES shape := HVSplitShape END;
  FVVBox = HVSplit.T BRANDED OBJECT OVERRIDES shape := HVSplitShape END;

CONST EmptyShape = VBT.SizeRange {lo := 0, pref := 0, hi := 1};

PROCEDURE HVSplitShape (v: HVSplit.T; ax: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    IF v.succ (NIL) = NIL THEN
      RETURN EmptyShape
    ELSE
      RETURN HVSplit.T.shape (v, ax, n)
    END
  END HVSplitShape;
    
(* ============================= HTile, VTile ============================== *)

REVEAL
  FVHTile = SplitterVBT.T BRANDED OBJECT OVERRIDES shape := HVTileShape END;
  FVVTile = SplitterVBT.T BRANDED OBJECT OVERRIDES shape := HVTileShape END;

PROCEDURE HVTileShape (v: SplitterVBT.T; ax: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    IF v.succ (NIL) = NIL THEN
      RETURN EmptyShape
    ELSE
      RETURN SplitterVBT.T.shape (v, ax, n)
    END
  END HVTileShape;
        
(* ============================= Numeric ============================== *)

REVEAL
  FVNumeric =
    NumericVBT.T BRANDED OBJECT OVERRIDES callback := NumericProc END;
    
PROCEDURE NumericProc (self: FVNumeric; event: AnyEvent.T) =
  BEGIN
    TYPECASE event OF
    | AnyEvent.Mouse (mouse) => MouseProc (self, mouse.mouse)
    | AnyEvent.Key (key) =>
          IF VBT.GetProp (self, TYPECODE (ClosureRef)) = NIL THEN
            NumericVBT.T.callback (self, event)
          ELSE
            KeyProc (self, key.key)
          END
    ELSE <* ASSERT FALSE *>
    END
  END NumericProc;


(* ============================= Menu ============================== *)

REVEAL FVMenu = AnchorSplit.T BRANDED OBJECT OVERRIDES pre := PreMenu END;

PROCEDURE PreMenu (v: AnchorSplit.T) =
  VAR mouse: VBT.MouseRec;
  BEGIN
    mouse.time := 0;
    MouseProc (v, mouse);
    AnchorSplit.T.pre (v);
  END PreMenu;


(* ============================= Help ============================== *)

REVEAL FVHelp = AnchorHelpSplit.T BRANDED OBJECT END;
  

(* ============================= IntApply ============================ *)

REVEAL
  FVIntApply =
    IntApplyPublic BRANDED OBJECT
      name    : TEXT;            (* name of destination VBT *)
      property: TEXT   := NIL;   (* name of property to set, if any *)
    OVERRIDES
      init    := IntApplyInit;
      discard := IntApplyDiscard;
      misc    := IntApplyMisc;
    END;

PROCEDURE IntApplyInit (v       : FVIntApply;
                        fv      : VBT.T;
                        ch      : VBT.T;
                        name    : TEXT;
                        property: TEXT       := NIL): FVIntApply
  RAISES {Error} =
  BEGIN
    v.name := name;
    v.property := property;
    TYPECASE fv OF
    | T =>
        TYPECASE ch OF
        | FVNumeric, FVScroller =>
            IF VBT.GetProp(ch, TYPECODE(ClosureRef)) # NIL THEN
              RAISE
                Error("IntApply: child already has event handler attached");
            END;
            VBT.PutProp(ch, NEW(ClosureRef, fv := fv,
                                cl := NEW(IAClosure, ia := v)));
        ELSE
          RAISE Error("IntApply: child not a Numeric or Scroller");
        END;
    ELSE
      RAISE Error("IntApply: not attached to a FormsVBT");
    END;
    RETURN Filter.T.init(v, ch);
  END IntApplyInit;

PROCEDURE IntApplyMisc (v: FVIntApply; READONLY cd: VBT.MiscRec) =
  VAR ch := Filter.Child(v);
  BEGIN
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected AND ch # NIL THEN
      (* remove the callback if its ours *)
      WITH cr = VBT.GetProp(ch, TYPECODE(ClosureRef)) DO
        IF cr # NIL AND ISTYPE(NARROW(cr, ClosureRef).cl, IAClosure) THEN
          VBT.RemProp(ch, TYPECODE(ClosureRef))
        END;
      END;
    END;
    Filter.T.misc(v, cd);
  END IntApplyMisc;

PROCEDURE IntApplyDiscard (v: FVIntApply) =
  VAR ch := Filter.Child(v);
  BEGIN
    IF ch # NIL THEN
      WITH cr = VBT.GetProp(ch, TYPECODE(ClosureRef)) DO
        IF cr # NIL AND ISTYPE(NARROW(cr, ClosureRef).cl, IAClosure) THEN
          VBT.RemProp(ch, TYPECODE(ClosureRef))
        END;
      END;
    END;
    Filter.T.discard(v);
  END IntApplyDiscard;

TYPE
  IAClosure =
    Closure OBJECT ia: FVIntApply;  OVERRIDES apply := IAApply; END;

PROCEDURE IAApply (             cl  : IAClosure;
                                fv  : T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED*>  time: VBT.TimeStamp) =
  VAR int: INTEGER;
  BEGIN
    TRY
      TYPECASE Filter.Child(cl.ia) OF
      | FVScroller (t) => int := ScrollerVBT.Get(t)
      | FVNumeric (t) => int := NumericVBT.Get(t)
      ELSE
        RAISE Unimplemented;
      END;

      IF cl.ia.property = NIL THEN
        PutInteger(fv, cl.ia.name, int);
      ELSE
        PutIntegerProperty(fv, cl.ia.name, cl.ia.property, int);
      END;
    EXCEPT
    | Error, Unimplemented =>
      <* ASSERT FALSE *>
    END;
  END IAApply;

(* ====================== Runtime support routines ==================== *)

PROCEDURE GetText (fv: T; name: TEXT): TEXT RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVBrowser (v) =>
        VAR this: ListVBT.Cell;
        BEGIN
          IF v.getFirstSelected (this) THEN RETURN v.getValue (this) END;
          RETURN ""
        END
    | FVFileBrowser (v) =>
        TRY
          RETURN FileBrowserVBT.GetFile (v)
        EXCEPT
        | FileBrowserVBT.Error (e) =>
            RAISE Error (Fmt.F ("Error for %s: %s", e.path, e.text))
        END
    | FVText (t) => RETURN TextVBT.Get (t)
    | FVTypescript (v) =>
        TRY
          RETURN Rd.GetText (TypescriptVBT.GetRd (v), LAST (CARDINAL))
        EXCEPT
        | Rd.Failure (ref) => RAISE Error (RdUtils.FailureText (ref))
        | Thread.Alerted => RAISE Error ("Thread.Alerted")
        END
    | TextEditVBT.T (v) => RETURN TextPort.GetText (v.tp)
    | TextPort.T (v) => RETURN TextPort.GetText (v)
    | FVNumeric (v) =>
        IF NumericVBT.IsEmpty (v) THEN
          RETURN ""
        ELSE
          RETURN Fmt.Int (NumericVBT.Get (v))
        END
    ELSE
      RAISE Unimplemented
    END
  END GetText;

PROCEDURE PutText (fv: T; name: TEXT; text: TEXT; append := FALSE)
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVBrowser (v) =>
        VAR this: ListVBT.Cell;
        BEGIN
          IF v.getFirstSelected (this) THEN v.setValue (this, text) END
        END
    | FVFileBrowser (v) =>
        TRY
          FileBrowserVBT.Set (v, text)
        EXCEPT
          FileBrowserVBT.Error (e) =>
            RAISE Error (Fmt.F ("Error for %s: %s", e.path, e.text))
        END
    | FVPixmap (t) => PixmapVBT.Put (t, GetPixmap (text, fv.path, NIL))
    | FVImage (t) => 
        VAR pm: ImageRd.T; len: INTEGER; BEGIN
        TRY Rd.Close (t.rd); t.rd := NIL EXCEPT
        | Rd.Failure (ref) => RAISE Error (RdUtils.FailureText (ref))
        | Thread.Alerted => (* ignore *)
        END;
        TRY t.rd := Rsrc.Open (text, fv.path) EXCEPT
        | Rsrc.NotFound => RAISE Error("No such resource: " & text)
        END;
        TRY len := Rd.Length(t.rd) EXCEPT
        | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
        | Thread.Alerted => (* ignore *)
        END;
        pm := t.get();
        EVAL pm.init (t.rd, 0, len, t.op, NIL, t.gamma);
        t.put (pm, t.bg);
        END;
    | FVText (t) =>
        IF append THEN
          TextVBT.Put (t, TextVBT.Get (t) & text)
        ELSE
          TextVBT.Put (t, text)
        END
    | FVTypescript (v) =>
        TRY
          Wr.PutText (TypescriptVBT.GetWr (v), text)
        EXCEPT
        | Wr.Failure (ref) => RAISE Error (RdUtils.FailureText (ref))
        | Thread.Alerted =>      (* ignore *)
        END
    | TextEditVBT.T (v) =>
        IF append THEN
          TextPort.PutText (v.tp, text)
        ELSE
          TextPort.SetText (v.tp, text)
        END
    | TextPort.T (v) =>
        IF append THEN
          TextPort.PutText (v, text)
        ELSE
          TextPort.SetText (v, text)
        END
    ELSE
      RAISE Unimplemented
    END
  END PutText;

PROCEDURE GetInteger (fv: T; name: TEXT): INTEGER
  RAISES {Error, Unimplemented} =
  <* FATAL Split.NotAChild *>
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVScroller (t) => RETURN ScrollerVBT.Get (t)
    | FVNumeric (t) => RETURN NumericVBT.Get (t)
    | FVTSplit (t) => RETURN Split.Index (t, TSplit.GetCurrent (t))
    | FVBrowser (t) =>
        VAR this: ListVBT.Cell;
        BEGIN
          IF t.getFirstSelected (this) THEN RETURN this END;
          RAISE Error ("Nothing has been selected.")
        END
    ELSE
      RAISE Unimplemented
    END
  END GetInteger;

PROCEDURE PutInteger (fv: T; name: TEXT; int: INTEGER)
  RAISES {Error, Unimplemented} =
  VAR vbt: VBT.T;
  <* FATAL Split.NotAChild *>
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVScroller (t) => ScrollerVBT.Put (t, int)
    | FVNumeric (t) => NumericVBT.Put (t, int)
    | FVTSplit (t) =>
        IF 0 <= int AND int < Split.NumChildren (t) THEN
          vbt := Split.Nth (t, int);
          TSplit.SetCurrent (t, vbt);
          FirstFocus (vbt, GetTheEventTime (fv))
        ELSE
          RAISE Error (Fmt.F ("%s is an illegal TSplit-index for %s.",
                              Fmt.Int (int), name))
        END
    | FVBrowser (t) =>
        IF 0 <= int AND int < t.count () THEN
          t.selectOnly (int)
        ELSE
          RAISE Error (Fmt.F ("%s is an illegal selection for %s.",
                              Fmt.Int (int), name))
        END
    ELSE
      RAISE Unimplemented
    END
  END PutInteger;

PROCEDURE GetIntegerProperty (fv: T; name, propertyName: TEXT): INTEGER
  RAISES {Error, Unimplemented} =
  VAR
    fvbt := GetVBT (fv, name);
  BEGIN
    IF Text.Equal(propertyName, "NorthEdge") THEN 
      RETURN ROUND(FLOAT(VBT.Domain(fvbt).north)/ Pts.ToPixels(fvbt, 1.0, Axis.T.Ver));
    ELSIF Text.Equal(propertyName, "SouthEdge") THEN 
      RETURN ROUND(FLOAT(VBT.Domain(fvbt).south)/ Pts.ToPixels(fvbt, 1.0, Axis.T.Ver));
   ELSIF Text.Equal(propertyName, "EastEdge") THEN 
     RETURN ROUND(FLOAT(VBT.Domain(fvbt).east)/ Pts.ToPixels(fvbt, 1.0, Axis.T.Hor));
   ELSIF Text.Equal(propertyName, "WestEdge") THEN 
     RETURN ROUND(FLOAT(VBT.Domain(fvbt).west)/ Pts.ToPixels(fvbt, 1.0, Axis.T.Hor));
   ELSE
    TYPECASE fvbt  OF
    | TextEditVBT.T (v) =>
      IF Text.Equal(propertyName, "Position") THEN
        RETURN TextPort.Index(v.tp)
      ELSIF Text.Equal(propertyName, "Length") THEN
        RETURN TextPort.Length(v.tp) 
      END    
    | FVNumeric (v) =>
        IF Text.Equal (propertyName, "Min") THEN
          RETURN NumericVBT.GetMin (v)
        ELSIF Text.Equal (propertyName, "Max") THEN
          RETURN NumericVBT.GetMax (v)
        END
    | FVScroller (v) =>
        IF Text.Equal (propertyName, "Min") THEN
          RETURN ScrollerVBT.GetMin (v)
        ELSIF Text.Equal (propertyName, "Max") THEN
          RETURN ScrollerVBT.GetMax (v)
        ELSIF Text.Equal (propertyName, "Step") THEN
          RETURN ScrollerVBT.GetStep (v)
        ELSIF Text.Equal (propertyName, "Thumb") THEN
          RETURN ScrollerVBT.GetThumb (v)
        END
    ELSE
    END;
    RAISE Unimplemented
  END;
  END GetIntegerProperty;

PROCEDURE PutIntegerProperty (fv     : T;
                              name, p: TEXT;
                              value  : INTEGER)
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT(fv, name) OF
    | TextEditVBT.T (v) =>
        IF Text.Equal(p, "Position") THEN
          TextPort.Seek(v.tp, Cardinal(value, p));
          RETURN
        ELSIF Text.Equal(p, "Normalize") THEN
          TextPort.Normalize(v.tp, Cardinal(value, p));
          RETURN
        END
    | FVNumeric (v) =>
        IF Text.Equal(p, "Min") THEN
          NumericVBT.PutBounds(v, value, NumericVBT.GetMax(v));
          RETURN
        ELSIF Text.Equal(p, "Max") THEN
          NumericVBT.PutBounds(v, NumericVBT.GetMin(v), value);
          RETURN
        END
    | FVScroller (v) =>
        IF Text.Equal(p, "Step") THEN
          ScrollerVBT.PutStep(v, Cardinal(value, p));
          RETURN
        ELSE
          VAR
            min   := ScrollerVBT.GetMin(v);
            max   := ScrollerVBT.GetMax(v);
            thumb := ScrollerVBT.GetThumb(v);
          BEGIN
            IF Text.Equal(p, "Min") THEN
              min := value
            ELSIF Text.Equal(p, "Max") THEN
              max := value
            ELSIF Text.Equal(p, "Thumb") THEN
              thumb := Cardinal(value, p)
            END;
            ScrollerVBT.PutBounds(v, min, max, thumb);
            RETURN
          END
        END
    | FVVideo (v) =>
        EVAL Cardinal(value, p);
        IF Text.Equal(p, "Quality") THEN
          IF value < FIRST(JVSink.Quality)
               OR LAST(JVSink.Quality) < value THEN
            RAISE
              Error("Video: quality must be between 0 and 15");
          END;
          v.setQuality(value);
        ELSIF Text.Equal(p, "Width") THEN
          SetVideoSize(v, value, Axis.T.Hor);
        ELSIF Text.Equal(p, "Height") THEN
          SetVideoSize(v, value, Axis.T.Ver);
        ELSIF Text.Equal(p, "MSecs") THEN
          v.setMinFrameMSecs(value);
        END;
        RETURN
    | FVAudio (t) =>
        IF Text.Equal(p, "Volume") THEN
          IF value < FIRST(Jva.Volume)
               OR LAST(Jva.Volume) < value THEN
            RAISE
              Error(
                Fmt.F("Audio: volume must be in range [%s..%s]",
                      Fmt.Int(FIRST(Jva.Volume)),
                      Fmt.Int(LAST(Jva.Volume))));
          END;
          TRY
            AudioVBT.SetVolume(t, value);
          EXCEPT
          | Thread.Alerted =>
            RAISE Error("PutInteger: Audio, Thread Alerted");
          END (* TRY *);
          RETURN
          END (* IF *)
    ELSE
    END;
    RAISE Unimplemented
  END PutIntegerProperty;

PROCEDURE GetRealProperty (fv: T; name, propertyName: TEXT): REAL
  RAISES {Error, Unimplemented} =
  VAR hscale, vscale: REAL;
  BEGIN
    TYPECASE GetVBT(fv, name) OF
    | ScaleFilter.T (v) =>
        ScaleFilter.Get(v, hscale, vscale);
        IF Text.Equal(propertyName, "HScale") THEN
          RETURN hscale
        ELSIF Text.Equal(propertyName, "VScale") THEN
          RETURN vscale
        END
    ELSE
    END;
    RAISE Unimplemented
  END GetRealProperty;

PROCEDURE PutRealProperty (fv: T; name, p: TEXT; value: REAL)
  RAISES {Error, Unimplemented} =
  VAR hscale, vscale: REAL;
  BEGIN
    TYPECASE GetVBT(fv, name) OF
    | ScaleFilter.T (v) =>
        ScaleFilter.Get(v, hscale, vscale);
        IF Text.Equal(p, "HScale") THEN
          ScaleFilter.Scale(v, value, vscale);
          RETURN
        ELSIF Text.Equal(p, "VScale") THEN
          ScaleFilter.Scale(v, hscale, value);
          RETURN
        END
    ELSE
    END;
    RAISE Unimplemented
  END PutRealProperty;

PROCEDURE GetBooleanProperty (fv: T; name, propertyName: TEXT):
  BOOLEAN RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT(fv, name) OF
    | TextEditVBT.T (v) =>
        IF Text.Equal(propertyName, "ReadOnly") THEN
          RETURN v.tp.getReadOnly()
        ELSE
        END;
    | ShadowedVBT.T (v) =>
        IF Text.Equal(propertyName, "Raised") THEN
          RETURN ShadowedVBT.GetStyle (v) = Shadow.Style.Raised
        ELSIF Text.Equal(propertyName, "Flat") THEN
          RETURN ShadowedVBT.GetStyle (v) = Shadow.Style.Flat
        ELSIF Text.Equal(propertyName, "Lowered") THEN
          RETURN ShadowedVBT.GetStyle (v) = Shadow.Style.Lowered
        ELSIF Text.Equal(propertyName, "Ridged") THEN
          RETURN ShadowedVBT.GetStyle (v) = Shadow.Style.Ridged
        ELSIF Text.Equal(propertyName, "Chiseled") THEN
          RETURN ShadowedVBT.GetStyle (v) = Shadow.Style.Chiseled
        END
    ELSE
    END;
    RAISE Unimplemented
  END GetBooleanProperty;

PROCEDURE PutBooleanProperty (fv     : T;
                              name, p: TEXT;
                              value  : BOOLEAN)
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT(fv, name) OF
    | TextEditVBT.T (v) =>
        IF Text.Equal(p, "ReadOnly") THEN
          v.tp.setReadOnly(value);
          RETURN
        END
    | ShadowedVBT.T (v) =>
        IF Text.Equal(p, "Raised") THEN
          ShadowedVBT.SetStyle (v, Shadow.Style.Raised);
          RETURN
        ELSIF Text.Equal(p, "Flat") THEN
          ShadowedVBT.SetStyle (v, Shadow.Style.Flat);
          RETURN
        ELSIF Text.Equal(p, "Lowered") THEN
          ShadowedVBT.SetStyle (v, Shadow.Style.Lowered);
          RETURN
        ELSIF Text.Equal(p, "Ridged") THEN
          ShadowedVBT.SetStyle (v, Shadow.Style.Ridged);
          RETURN
        ELSIF Text.Equal(p, "Chiseled") THEN
          ShadowedVBT.SetStyle (v, Shadow.Style.Chiseled);
          RETURN
        END
    | FVVideo (v) =>
        IF Text.Equal(p, "Synchronous") THEN
          v.setSynchronous(value);
          RETURN
        ELSIF Text.Equal(p, "Paused") THEN
          v.setPaused(value);
          RETURN
        ELSIF Text.Equal(p, "FixedSize") THEN
          v.setFixedSize(value);
          VBT.NewShape(v);
          RETURN
        ELSE
          RAISE Error("Video: unknown Boolean property " & p);
        END;
    | FVAudio (a) =>
        TRY
          IF Text.Equal(p, "Mute") THEN
            AudioVBT.SetMute(a, value);
            RETURN
          ELSIF Text.Equal(p, "IgnoreMapping") THEN
            AudioVBT.SetIgnoreMapping(a, value);
            RETURN
          ELSE
            RAISE Error("Audio: unknown Boolean property " & p);
          END;
        EXCEPT
        | Thread.Alerted =>
            RAISE Error("Audio: Put Boolean, Thread Alerted");
        END;
    ELSE
    END;
    RAISE Unimplemented
  END PutBooleanProperty;

PROCEDURE Cardinal (n: INTEGER; name: TEXT): CARDINAL RAISES {Error} =
  BEGIN
    IF n < 0 THEN
      RAISE Error (Fmt.F ("Value for %s, %s, should be a CARDINAL.", name,
                          Fmt.Int (n)))
    ELSE
      RETURN n
    END
  END Cardinal;

PROCEDURE SetVideoSize (v: FVVideo; value: CARDINAL; ax: Axis.T) =
  VAR width, height: CARDINAL;
  BEGIN
    v.getSize(width, height);
    CASE ax OF
    | Axis.T.Hor => width := value;
    | Axis.T.Ver => height := value;
    END;
    v.setSize(width, height);
    VBT.NewShape(v);
  END SetVideoSize;

PROCEDURE PutBoolean (fv: T; name: TEXT; val: BOOLEAN)
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVBoolean (b) => BooleanVBT.Put (b, val)
    | FVChoice (c) =>
        IF val THEN
          ChoiceVBT.Put (c)
        ELSIF ChoiceVBT.Get (c) = c THEN
          ChoiceVBT.Clear (c)
        END
    ELSE
      RAISE Unimplemented
    END
  END PutBoolean;

PROCEDURE PutChoice (fv: T; radioName, choiceName: TEXT)
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, radioName) OF
    | FVRadio (r) =>
        IF choiceName = NIL THEN
          WITH cur = ChoiceVBT.Selection (r.radio) DO
            IF cur # NIL THEN ChoiceVBT.Clear (cur) END
          END
        ELSE
          TYPECASE GetVBT (fv, choiceName) OF
          | FVChoice (c) => ChoiceVBT.Put (c)
          ELSE
            RAISE Error ("No Choice named " & choiceName)
          END
        END
    ELSE
      RAISE Unimplemented
    END
  END PutChoice;

(************************  Direct access  *************************)

PROCEDURE SetVBT (fv: T; name: TEXT; vbt: VBT.T) RAISES {Error} =
  BEGIN
    LOCK fv.mu DO
      IF fv.getVBT.put (name, vbt) THEN
        RAISE Error ("There is already a VBT named " & name)
      END
    END
  END SetVBT;

PROCEDURE GetVBT (fv: T; name: TEXT): VBT.T RAISES {Error} =
  VAR result: REFANY;
  BEGIN
    LOCK fv.mu DO
      IF fv.getVBT.get (name, result) THEN RETURN result END;
      RAISE Error ("There is no VBT named " & name)
    END
  END GetVBT;

PROCEDURE GetName (vbt: VBT.T): TEXT RAISES {Error} =
  VAR stateRef: REF State := VBT.GetProp(vbt, TYPECODE(REF State));
  BEGIN
    IF stateRef # NIL AND stateRef.name # NIL THEN
      RETURN stateRef.name
    ELSE
      RAISE Error("VBT is not named")
    END
  END GetName;

PROCEDURE RemoveName (fv: T; vbt: VBT.T) RAISES {Error} =
  <* FATAL MultiSplit.NotAChild *>
  VAR stateRef: REF State; result: REFANY;
  BEGIN
    stateRef := VBT.GetProp(vbt, TYPECODE(REF State));
    LOCK fv.mu DO
      IF stateRef # NIL AND stateRef.name # NIL THEN
        IF NOT fv.getVBT.delete(stateRef.name, result) THEN
          <* ASSERT FALSE *>
        END
      END
    END;
    (* now recursively remove the names of all descendants as well *)
    IF NOT LeafVBT(vbt) THEN
      VAR ch := MultiSplit.Succ (vbt, NIL); BEGIN
        WHILE ch # NIL DO
          RemoveName (fv, ch);
          ch := MultiSplit.Succ (vbt, ch)
        END
      END
    END
  END RemoveName;

PROCEDURE Delete (fv: T; parent: TEXT; at: CARDINAL; count: CARDINAL := 1)
  RAISES {Error} =
  BEGIN
    TRY
      WITH p  = GetVBT (fv, parent),
           at = MIN (at, MultiSplit.NumChildren (p)) DO
        FOR i := 1 TO MIN (count, MultiSplit.NumChildren (p) - at) DO
          WITH ch = MultiSplit.Nth(p, at) DO
            RemoveName (fv, ch);
            MultiSplit.Delete (p, ch)
          END
        END
      END
    EXCEPT
    | MultiSplit.NotAChild => RAISE Error ("Delete: No Split named " & parent)
    END
  END Delete;

PROCEDURE InsertVBT (fvLocal: T;
                     parent : TEXT;
                     child  : VBT.T;
                     at     : CARDINAL := LAST (CARDINAL)) RAISES {Error} =
  BEGIN
    TRY
      WITH p  = GetVBT (fvLocal, parent),
           at = MIN (at, MultiSplit.NumChildren (p)) DO
        IF at = 0 THEN
          MultiSplit.Insert (p, NIL, child)
        ELSE
          MultiSplit.Insert (p, MultiSplit.Nth (p, at - 1), child)
        END
      END
    EXCEPT
    | MultiSplit.NotAChild =>
        RAISE Error ("InsertVBT: No Split named " & parent)
    END
  END InsertVBT;

PROCEDURE DeleteVBT (fv: T; parent: TEXT; at: CARDINAL; count: CARDINAL := 1)
  RAISES {Error} =
  BEGIN
    TRY
      WITH p  = GetVBT (fv, parent),
           at = MIN (at, MultiSplit.NumChildren (p)) DO
        FOR i := 1 TO MIN (count, MultiSplit.NumChildren (p) - at) DO
          WITH ch = MultiSplit.Nth(p, at) DO
            MultiSplit.Delete (p, ch)
          END
        END
      END
    EXCEPT
    | MultiSplit.NotAChild => RAISE Error ("DeleteVBT: No Split named " & parent)
    END
  END DeleteVBT;



PROCEDURE LeafVBT (v: VBT.T): BOOLEAN =
  BEGIN
    RETURN NOT ISTYPE(v, VBT.Split) AND MultiClass.Resolve(v) = NIL 
  END LeafVBT;


(***********************  Special controls  ***********************)

PROCEDURE TakeFocus (fv       : T;
                     name     : TEXT;
                     eventTime: VBT.TimeStamp;
                     select                     := FALSE) RAISES {Error} =
  VAR vbt := GetVBT (fv, name);
  PROCEDURE focus (port: TextPort.T) =
    BEGIN
      IF TextPort.TryFocus (port, eventTime) AND select THEN
        TextPort.Select (
          port, eventTime, 0, LAST (CARDINAL), replaceMode := TRUE)
      END
    END focus;
  BEGIN
    TYPECASE vbt OF
    | TextPort.T (v) => focus (v)
    | TextEditVBT.T (v) => focus (v.tp)
    | FVNumeric (v) => focus (v.typein)
    ELSE
      RAISE Error (name & " cannot take a keyboard focus")
    END
  END TakeFocus;


(************************ Runtime properties ************************)

PROCEDURE GetTextProperty (fv: T; name, prop: TEXT): TEXT
  RAISES {Error, Unimplemented} =
  VAR
    vbt                 := GetVBT (fv, name);
    stateRef: REF State := VBT.GetProp (vbt, TYPECODE (REF State));
  BEGIN
    IF Text.Equal (prop, "Select") THEN
      TYPECASE vbt OF
      | FVBrowser, FVMultiBrowser =>
          VAR 
            v := NARROW(vbt, ListVBT.T); 
            cells := v.getAllSelected(); 
            sel: TEXT;
          BEGIN
            IF NUMBER(cells^) # 0 THEN
              sel := v.getValue (cells[FIRST(cells^)]) ;
              FOR c := FIRST(cells^)+1 TO LAST(cells^) DO
                sel := sel & "\n" & v.getValue (cells[c])
              END;
              RETURN sel
            ELSE
              RETURN NIL
            END
          END
      ELSE
        RAISE Unimplemented
      END
    ELSIF Text.Equal(prop, "Items") THEN
      TYPECASE vbt OF
      | FVBrowser, FVMultiBrowser =>
          VAR 
            v := NARROW (vbt, ListVBT.T); 
            stringRep := "";
          BEGIN
            IF v.count() # 0 THEN
              stringRep := v.getValue(0);
              FOR this := 1 TO v.count() - 1 DO
                stringRep := stringRep & "\n" & v.getValue(this)
              END;
            END;
            RETURN stringRep
          END 
       ELSE
         RAISE Unimplemented
       END
    ELSIF Text.Equal(prop, "ActiveTarget") THEN
      TYPECASE vbt OF
      | FVSource (v) => 
         WITH target = SourceVBT.GetTarget(v) DO 
            IF target = NIL THEN RAISE Error ("No active target") END;
            RETURN GetName (target);
         END
       ELSE
         RAISE Unimplemented
       END
    ELSIF stateRef = NIL THEN
      RAISE Error (Fmt.F ("The form named \"%s\" has no properties", name))
    ELSIF Text.Equal (prop, "Font") THEN
      RETURN stateRef.fontName
    ELSIF Text.Equal (prop, "LabelFont") THEN
      RETURN stateRef.labelFontName
    ELSE
      RAISE Unimplemented
    END
  END GetTextProperty;
                       
PROCEDURE PutTextProperty (fv: T; name, property: TEXT; t: TEXT)
  RAISES {Error, Unimplemented} =
  VAR
    vbt                 := GetVBT (fv, name);
    stateRef: REF State := VBT.GetProp (vbt, TYPECODE (REF State));
    indx, ct, from : INTEGER;

  PROCEDURE setFont (v: TextPort.T) =
    BEGIN
      stateRef.fontName := t;
      stateRef.font := FindFont (t);
      stateRef.fontMetrics := NIL;
      v.setFont (stateRef.font)
    END setFont;
  BEGIN
    IF Text.Equal (property, "Select") THEN
      TYPECASE vbt OF
      | FVBrowser, FVMultiBrowser =>
          VAR 
            v := NARROW(vbt, ListVBT.T); 
          BEGIN
            FOR this := 0 TO v.count () - 1 DO
              IF Text.Equal (v.getValue (this), t) THEN
                v.selectOnly (this);   (* turn off previous selection if any *)
                RETURN
                END (* IF *)   
            END;
            v.selectNone ();
            RETURN
            END (* BEGIN *)
      ELSE
      END (* TYPECASE *)  
    ELSIF Text.Equal (property, "SelectAlso") AND   ISTYPE(vbt, FVMultiBrowser) THEN
      (* Selects t if present, else noop *)
      VAR 
        v := NARROW(vbt, ListVBT.T); 
      BEGIN
         FOR this := 0 TO v.count () - 1 DO
              IF Text.Equal (v.getValue (this), t) THEN
                v.select (this, TRUE);   (* turn off previous selection if any *)
                RETURN
                END 
            END;
         RETURN;
       END (* BEGIN *)
    ELSIF Text.Equal (property, "ScrollToShow") THEN
      (* Scrolls to first occurrence of specified item. Noop if not present *)
      TYPECASE vbt OF
      | FVBrowser, FVMultiBrowser =>
          VAR 
            v := NARROW(vbt, ListVBT.T); 
          BEGIN
            FOR this := 0 TO v.count () - 1 DO
              IF Text.Equal (v.getValue (this), t) THEN
                v.scrollToShow(this);   
                RETURN
                END
            END;
            RETURN
            END (* BEGIN *) 
      ELSE
      END (* TYPECASE *) 
    ELSIF Text.Equal (property, "Items") THEN
      TYPECASE vbt OF
      | FVBrowser, FVMultiBrowser =>
        (* FVBrowser and  FVMultiBrowser are ListVBTs - interpret t as a sequence of cells
           demarcated by '\n'. Insert appropriately *)
          VAR 
            v := NARROW(vbt, ListVBT.T); 
          BEGIN
            v.removeCells(0, v.count()); (* empty listVBT *)
            indx := Text.FindChar(t, '\n', 0); from := 0; ct := 0;
            WHILE indx # -1 DO
              v.insertCells(ct, 1);
              v.setValue(ct, Text.Sub(t, from, indx-from));
              from := indx+1;
              INC(ct);
              IF from < Text.Length(t) THEN
                indx := Text.FindChar(t, '\n', from);    
              ELSE 
                indx := -1
              END
            END;
            IF from < Text.Length(t) THEN (* last cell *)
              v.insertCells(ct, 1);
              v.setValue(ct, Text.Sub(t, from));
            END;
            v.selectNone ();
            RETURN
            END (* BEGIN *)
      ELSE
      END (* TYPECASE *) 
    ELSIF stateRef = NIL THEN
      RAISE Error (Fmt.F ("The form named \"%s\" has no properties", name))
    ELSIF Text.Equal (property, "Color") OR Text.Equal (property, "BgColor") THEN
      TRY
        PutColorProperty (fv, name, property, ColorName.ToRGB (t));
        RETURN
      EXCEPT
      | ColorName.NotFound => RAISE Error ("No such color: " & t)
      END
    ELSIF Text.Equal (property, "Font") THEN
      TYPECASE vbt OF
      | TextPort.T (v) => setFont (v); RETURN
      | TextEditVBT.T (v) => setFont (v.tp); RETURN
      | NumericVBT.T (v) => setFont (v.typein); RETURN
      ELSE
      END
    ELSIF Text.Equal (property, "LabelFont") THEN
      TYPECASE vbt OF
      | FVText (v) =>
          stateRef.labelFontName := t;
          stateRef.labelFont := FindFont (t);
          stateRef.labelFontMetrics := NIL;
          TextVBT.SetFont (v, stateRef.labelFont, TextVBT.GetQuad (v));
          RETURN
      ELSE
      END
    END;
    RAISE Unimplemented
  END PutTextProperty;

PROCEDURE GetColorProperty (fv: T; name, property: TEXT): Color.T
  RAISES {Error, Unimplemented} =
  VAR
    vbt                 := GetVBT (fv, name);
    stateRef: REF State := VBT.GetProp (vbt, TYPECODE (REF State));
  BEGIN
    IF Text.Equal (property, "Color") THEN
      RETURN stateRef.fgRGB
    ELSIF Text.Equal (property, "BgColor") THEN
      RETURN stateRef.bgRGB
    ELSIF Text.Equal (property, "LightShadow") THEN
      RETURN stateRef.lightRGB
    ELSIF Text.Equal (property, "DarkShadow") THEN
      RETURN stateRef.darkRGB
    ELSE
      RAISE Unimplemented
    END
  END GetColorProperty;

PROCEDURE PutColorProperty (         fv            : T;
                                     name, property: TEXT;
                            READONLY color         : Color.T)
  RAISES {Error, Unimplemented} =
  PROCEDURE setColor (v: TextPort.T) =
    BEGIN
      v.setColorScheme (
        PaintOp.MakeColorScheme (stateRef.bgOp, stateRef.fgOp))
    END setColor;
  VAR
    vbt                 := GetVBT (fv, name);
    stateRef: REF State := VBT.GetProp (vbt, TYPECODE (REF State));
  BEGIN
    IF Text.Equal (property, "Color") THEN
      stateRef.fgRGB := color;
      stateRef.fgOp :=
        PaintOp.FromRGB (color.r, color.g, color.b, PaintOp.Mode.Accurate);
      TYPECASE vbt OF
      | TextureVBT.T (v) => 
           VAR op: PaintOp.T; txt: Pixmap.T; nwAlign: BOOLEAN; BEGIN
             TextureVBT.Get(v, op, txt, nwAlign);
             TextureVBT.Set (v, PaintOp.Pair(stateRef.bgOp, stateRef.fgOp), txt, nwAlign)
           END;
      | FVBar (v) => TextureVBT.Set (Filter.Child (v), stateRef.fgOp)
      | FVBorder (v) => BorderedVBT.SetColor (v, stateRef.fgOp)
      | TextPort.T (v) => setColor (v)
      | TextEditVBT.T (v) => setColor (v.tp)
      | NumericVBT.T (v) => setColor (v.typein)
      | PixmapVBT.T (v) =>
          PixmapVBT.SetColors (v, 
            PaintOp.Pair (stateRef.bgOp, stateRef.fgOp), stateRef.bgOp);
      | FVText (v) =>
          TextVBT.SetFont (
            v, TextVBT.GetFont (v),
            PaintOp.MakeColorQuad (stateRef.bgOp, stateRef.fgOp))
      ELSE
        RAISE Unimplemented
      END
    ELSIF Text.Equal (property, "BgColor") THEN
      stateRef.bgRGB := color;
      stateRef.bgOp :=
        PaintOp.FromRGB (color.r, color.g, color.b, PaintOp.Mode.Accurate);
      TYPECASE vbt OF
      | TextureVBT.T (v) =>
           VAR op: PaintOp.T; txt: Pixmap.T; nwAlign: BOOLEAN; BEGIN
             TextureVBT.Get(v, op, txt, nwAlign);
             TextureVBT.Set (v, PaintOp.Pair(stateRef.bgOp, stateRef.fgOp), txt, nwAlign)
           END;
      | FVGlue (v) => TextureVBT.Set (Filter.Child (v), stateRef.bgOp)
      | FVRim (v) => BorderedVBT.SetColor (v, stateRef.bgOp)
      | TextPort.T (v) => setColor (v)
      | TextEditVBT.T (v) => setColor (v.tp)
      | NumericVBT.T (v) => setColor (v.typein)
      | PixmapVBT.T (v) =>
          PixmapVBT.SetColors (v, 
            PaintOp.Pair (stateRef.bgOp, stateRef.fgOp), stateRef.bgOp);
      | FVText (v) =>
          TextVBT.SetFont (
            v, TextVBT.GetFont (v),
            PaintOp.MakeColorQuad (stateRef.bgOp, stateRef.fgOp))
      ELSE
        RAISE Unimplemented
      END
    ELSE
      RAISE Unimplemented
    END
  END PutColorProperty;

VAR fontCache := NEW (TextIntTbl.Default).init ();
    fontCacheMu := NEW(MUTEX);

PROCEDURE FindFont (fontname: TEXT): Font.T =
  VAR fontnumber: INTEGER;
  BEGIN
    LOCK fontCacheMu DO
      IF fontCache.get(fontname, fontnumber) THEN
        RETURN Font.T{fontnumber}
      ELSE
        WITH f = Font.FromName(ARRAY OF TEXT{fontname}) DO
          EVAL fontCache.put(fontname, f.fnt);
          RETURN f
        END
      END
    END
  END FindFont;

PROCEDURE MakeActive (fv: T; name: TEXT; cursor := "")
  RAISES {Error} =
  BEGIN
    SetReactivity(fv, name, ReactivityVBT.State.Active, cursor);
  END MakeActive;

PROCEDURE MakePassive (fv: T; name: TEXT; cursor := "")
  RAISES {Error} =
  BEGIN
    SetReactivity(fv, name, ReactivityVBT.State.Passive, cursor);
  END MakePassive;
  
PROCEDURE MakeDormant (fv: T; name: TEXT; cursor := "")
  RAISES {Error} =
  BEGIN
    SetReactivity(fv, name, ReactivityVBT.State.Dormant, cursor);
  END MakeDormant;
  
PROCEDURE MakeVanish(fv: T; name: TEXT; cursor:= "") RAISES {Error} =
  BEGIN
     SetReactivity(fv, name, ReactivityVBT.State.Vanish, cursor);
  END MakeVanish;

PROCEDURE SetReactivity (fv    : T;
                         name  : Text.T;
                         state : ReactivityVBT.State;
                         cursor: TEXT                 )
  RAISES {Error} =
  VAR curs: Cursor.T;
  BEGIN
    IF Text.Empty(cursor) THEN
      curs := Cursor.DontCare
    ELSE
      curs := Cursor.FromName(ARRAY OF TEXT{cursor})
    END;
    ReactivityVBT.Set(FindReactivityVBT(fv, name), state, curs);
  END SetReactivity;

PROCEDURE IsActive(fv: T; name: Text.T): BOOLEAN RAISES {Error} =
  BEGIN
    RETURN TestReactivity(fv, name, ReactivityVBT.State.Active);
  END IsActive;

PROCEDURE IsPassive(fv: T; name: Text.T): BOOLEAN RAISES {Error} =
  BEGIN
    RETURN TestReactivity(fv, name, ReactivityVBT.State.Passive);
  END IsPassive;

PROCEDURE IsDormant(fv: T; name: Text.T): BOOLEAN RAISES {Error} =
  BEGIN
    RETURN TestReactivity(fv, name, ReactivityVBT.State.Dormant);
  END IsDormant;

PROCEDURE IsVanished(fv: T; name: Text.T): BOOLEAN RAISES {Error} =
  BEGIN
    RETURN TestReactivity(fv, name, ReactivityVBT.State.Vanish);
  END IsVanished;

PROCEDURE TestReactivity (fv: T; name: Text.T; state: ReactivityVBT.State):
  BOOLEAN RAISES {Error} =
  BEGIN
    RETURN state = ReactivityVBT.Get (FindReactivityVBT (fv, name));
  END TestReactivity;

PROCEDURE FindReactivityVBT (fv: T; name: Text.T): FVFilter
  RAISES {Error} =
  VAR v := GetVBT (fv, name);
  BEGIN
    WHILE v # NIL DO
      TYPECASE v OF FVFilter => RETURN v ELSE END;
      v := VBT.Parent (v);
    END;
    RAISE Error ("Cannot find FVFilter");
  END FindReactivityVBT;

PROCEDURE GetBoolean (fv: T; name: TEXT): BOOLEAN
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, name) OF
    | FVBoolean (b) => RETURN BooleanVBT.Get (b)
    | FVChoice (c) => RETURN ChoiceVBT.Get (c) = c
    ELSE
      RAISE Unimplemented
    END
  END GetBoolean;

PROCEDURE GetChoice (fv: T; radioName: TEXT): TEXT
  RAISES {Error, Unimplemented} =
  BEGIN
    TYPECASE GetVBT (fv, radioName) OF
    | FVRadio (r) =>
        TYPECASE ChoiceVBT.Selection (r.radio) OF
        | NULL => RETURN NIL
        | FVChoice (c) => RETURN c.name
        ELSE
        END
    ELSE
    END;
    RAISE Unimplemented
  END GetChoice;

PROCEDURE MakeSelected (fv: T; choiceName: TEXT) RAISES {Error} =
  BEGIN
    TYPECASE GetVBT (fv, choiceName) OF
    | FVChoice (c) => ChoiceVBT.Put (c)
    ELSE
      RAISE Error ("No Choice named " & choiceName)
    END
  END MakeSelected;

PROCEDURE IsSelected (fv: T; choiceName: TEXT): BOOLEAN RAISES {Error} =
  BEGIN
    TYPECASE GetVBT (fv, choiceName) OF
    | FVChoice (c) => RETURN ChoiceVBT.Get (c) = c
    ELSE
      RAISE Error ("No Choice named " & choiceName)
    END
  END IsSelected;

(************************* Generic interactors **********************)

PROCEDURE PutGeneric (fv: T; genericName: TEXT; vbt: VBT.T)
  RAISES {Error} =
  BEGIN
    TYPECASE GetVBT (fv, genericName) OF
    | FVGeneric (v) =>
        IF vbt = NIL THEN
          EVAL Filter.Replace (v, NIL);
          FlexVBT.Set (v, EMPTYSHAPE)
        ELSE
          EVAL Filter.Replace (v, vbt);
          FlexVBT.Set (v, FlexVBT.Default)
        END;
        RETURN
    ELSE
      RAISE Error ("No Generic named " & genericName)
    END
  END PutGeneric;
    
PROCEDURE GetGeneric (fv: T; genericName: TEXT): VBT.T RAISES {Error} =
  BEGIN
    TYPECASE GetVBT (fv, genericName) OF
    | FVGeneric (v) => RETURN Filter.Child (v)
    ELSE
      RAISE Error ("No Generic named " & genericName)
    END
  END GetGeneric;

(************************ Debugging tools ************************)

PROCEDURE ToText (x        : REFANY;
                  maxDepth : CARDINAL := LAST (CARDINAL);
                  maxLength: CARDINAL := LAST (CARDINAL)  ): TEXT =
  BEGIN
    TYPECASE x OF
    | NULL => RETURN "NIL"
    | Atom.T (sym) => RETURN Atom.ToText (sym)
    | TEXT (t) => RETURN t
    ELSE
      WITH wr = TextWr.New () DO
        TRY
          Sx.Print (wr, x, maxDepth, maxLength);
          RETURN TextWr.ToText (wr)
        EXCEPT
        | Thread.Alerted, Sx.PrintError, Wr.Failure =>
            RETURN "<Unprintable expression>"
        END
      END
    END
  END ToText;

PROCEDURE NamedVBTs (t: T): RefList.T =
  VAR
    name: TEXT;
    vbt : REFANY;
    res : RefList.T := NIL;
    iter            := t.getVBT.iterateOrdered (FALSE);
  BEGIN
    WHILE iter.next (name, vbt) DO Push (res, RefList.List2 (name, vbt)) END;
    RETURN res
  END NamedVBTs;
  
<*UNUSED *> (* except during debugging! *)
PROCEDURE DumpTable (fv: T) =
  VAR
    value      : REFANY;
    key        : TEXT;
    alist, pair: RefList.T;
  BEGIN
    alist := NamedVBTs (fv);
    WHILE alist # NIL DO
      pair := Pop (alist);
      key := Pop (pair);
      value := pair.head;
      IO.Put (key);
      IO.Put (" -> ");
      IO.Put (RTTypeSRC.TypeName (value));
      IO.Put ("\n")
    END
  END DumpTable;

PROCEDURE GetAttachments (fv: T): RefList.T =
  VAR
    key     : TEXT;
    value   : REFANY;
    alist   : RefList.T := NIL;
    iter                := fv.getVBT.iterate ();
    property: REFANY;
  BEGIN
    WHILE iter.next (key, value) DO
      property := VBT.GetProp (value, TYPECODE (ClosureRef));
      IF property # NIL THEN Push (alist, RefList.List2 (key, property)) END
    END;
    RETURN alist
  END GetAttachments;

PROCEDURE SetAttachments (fv: T; alist: RefList.T) RAISES {Error} =
  VAR
    name      : TEXT;
    attachment: ClosureRef;
    pair      : RefList.T;
  BEGIN
    WHILE alist # NIL DO
      pair := Pop (alist);
      name := pair.head;
      attachment := pair.tail.head;
      Attach (fv, name, attachment.cl)
    END
  END SetAttachments;

PROCEDURE InitRuntime () =
  BEGIN
    MakeEventMiscCodeType := VBT.GetMiscCodeType ("FVRuntime.MakeEvent");
    MakeEventSelection := VBT.GetSelection ("FVRuntime.MakeEvent");
    cleanState.fgOp :=
      PaintOp.FromRGB (
        cleanState.fgRGB.r, 
        cleanState.fgRGB.g,
        cleanState.fgRGB.b,
        PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
    cleanState.bgOp :=
      PaintOp.FromRGB (
        cleanState.bgRGB.r,
        cleanState.bgRGB.g, 
        cleanState.bgRGB.b,
        PaintOp.Mode.Accurate, bw := PaintOp.BW.UseBg);
    cleanState.lightOp :=
      PaintOp.FromRGB (
        cleanState.lightRGB.r,
        cleanState.lightRGB.g,
        cleanState.lightRGB.b,
        PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
    cleanState.darkOp :=
      PaintOp.FromRGB (
        cleanState.darkRGB.r,
        cleanState.darkRGB.g,
        cleanState.darkRGB.b,
        PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);
        
    cleanState.fontMetrics := DefaultFontMetrics;
    cleanState.fontName := MetricsToName (cleanState.fontMetrics);
    cleanState.font := Font.FromName (ARRAY OF TEXT {cleanState.fontName});
    
    cleanState.labelFontMetrics := DefaultLabelFontMetrics;
    cleanState.labelFontName := MetricsToName (cleanState.labelFontMetrics);
    cleanState.labelFont :=
      Font.FromName (ARRAY OF TEXT {cleanState.labelFontName});
      
    cleanState.shadow :=
      Shadow.New (cleanState.shadowSz, 
                  cleanState.bgOp, cleanState.fgOp,
                  cleanState.lightOp, cleanState.darkOp);
    (* Initial state.zsplit are set in Init. *)
    
  END InitRuntime;

BEGIN
  (* From the FormsVBT language itself: *)
  qBOA := Atom.FromText ("BOA");
  qName := Atom.FromText ("Name");
  qValue := Atom.FromText ("Value");
  (* "Internal" symbols for macros: *)
  qBackquote := Atom.FromText (" backquote ");
  qComma := Atom.FromText (" comma ");
  qCommaAtsign := Atom.FromText (" comma-atsign ");
  qQuote := Atom.FromText (" quote ");
  InitParser ();
  InitRuntime ();
  Macro.Init ()
END FVRuntime.



