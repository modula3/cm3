(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 16 16:15:27 PDT 1996 by mhb                      *)
(*      modified on Mon Jan 30 14:48:34 PST 1995 by kalsow                   *)
(*      modified on Mon Jul 12 08:29:22 PDT 1993 by mcjones                  *)
(*      modified on Fri Jun 11 23:32:15 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:52 PDT 1992 by muller                   *)
(*      modified on Fri Mar 20 22:42:45 PST 1992 by steveg                   *)
(*      modified on Thu Apr  4 18:16:18 PST 1991 by brooks                   *)
<* PRAGMA LL                                                                 *>
<* PRAGMA EXPORTED                                                           *>

MODULE FileBrowserVBT;

IMPORT AnchorSplit, AnyEvent, ASCII, Atom, Axis, BorderedVBT, Cursor, File, Filter,
       Font, FS, HVSplit, ISOChar, Lex, ListVBT, MenuSwitchVBT, MultiFilter,
       MultiSplit, OSError, PaintOp, Pathname, Pixmap, Process, Rd, Rect,
       RegularFile, Shadow, ShadowedVBT, ShadowedFeedbackVBT, Split, Text,
       TextList, TextListSort, TextPort, TextRd, TextVBT, Thread, Time,
       TypeinVBT, VBT, WeakRef;

REVEAL
  T = Public BRANDED "FileBrowserVBT 4.0" OBJECT
        mu: MUTEX;
        <* LL = mu *>
        helper        : Helper;
        dirmenu       : DirMenu;
        suffixes      : TextList.T;
        readOnly      : BOOLEAN;
        dir           : Pathname.T;
        toSelect      : TEXT; (* if non-empty/NIL, select this string *)
        truthInHelper : BOOLEAN;   (* where to look for the value *)
        display_time  : Time.T;  (* last time we looked at this directory *)
        statThread    : Thread.T;
        isDir         : REF ARRAY OF BOOLEAN;
        topCell       : ListVBT.Cell;
      OVERRIDES
        init         := Init;
        selectItems  := SelectItems; (* no-op *)
        activateFile := ActivateFile; (* no-op *)
        activateDir  := ActivateDir;
        error        := DefaultError; (* no-op *)
        insertCells  := InsertCells;
        removeCells  := RemoveCells;
        reportVisible:= ReportVisible;
        getValue     := GetValue;
      END;
  Selector = ListVBT.MultiSelector BRANDED OBJECT
               v: T
             OVERRIDES
               insideClick := InsideClick
             END;
  Helper = TypeinVBT.T BRANDED OBJECT
             parent: T;
           OVERRIDES
             returnAction := HelperReturn;
             modified     := HelperModified
           END;
  DirMenu = PublicDirMenu BRANDED OBJECT
              font                       := Font.BuiltIn;
              shadow     : Shadow.T      := NIL; (* Shadow.None *)
              filebrowser: T;
              top        : TextVBT.T;
              vbox       : DirMenuVBox;
            OVERRIDES
              init := InitDirMenu;
              setFont := SetFontDirMenu;
            END;

TYPE
  (* The feedback on the DirMenu button is a DirMenuTop.  Its multi-child is a
     TextVBT. *)
  DirMenuTop = ShadowedFeedbackVBT.T OBJECT dm: DirMenu END;

  (* Each item in the vbox ("pathname component") is a DirMenuButton. *)
  DirMenuButton = MenuSwitchVBT.T OBJECT
                    dm: DirMenu
                  METHODS
                    init (text: TEXT): DirMenuButton := InitDirMenuButton;
                    put  (text: TEXT)                := DirMenuButtonPut;
                    get  (): TEXT                    := DirMenuButtonGet;
                  OVERRIDES
                    callback := DirMenuButtonCallback
                  END;

  (* The vbox of components needs to get its width from the DirMenu button. *)
  DirMenuVBox =
    HVSplit.T OBJECT dm: DirMenu OVERRIDES shape := DMVBoxShape END;

  (* We maintain a list of weak references to all initilialized filebrowsers,
     and we scan the list once a second, refreshing each one. *)
  FBList = REF RECORD
                 car: WeakRef.T;
                 cdr: FBList      := NIL
               END;

VAR
  tlock := NEW (MUTEX);
  <* LL = tlock *>
  fblist: FBList := NIL;
  fbcond         := NEW (Thread.Condition);

VAR (*CONST*)
  on_unix := Text.Equal (JoinPath ("a", "b"), "a/b");

CONST
  WindowsDelay = 300.0d0;  (* # seconds between directory updates on Windows *)

(****************************  Creation  ***************************)

PROCEDURE Init (v     : T;
                font  : Font.T            := Font.BuiltIn;
                colors: PaintOp.ColorQuad := NIL           ): T =
  BEGIN
    IF colors = NIL THEN colors := Shadow.None END;
    v.mu := NEW (MUTEX);
    TRY
      LOCK v.mu DO
        TYPECASE v.selector OF
        | NULL => v.selector := NEW (Selector, v := v).init (v)
        | Selector (s) => s.v := v
        ELSE                     <* ASSERT FALSE *>
        END;
        EVAL ListVBT.T.init (v, colors);
        TYPECASE v.painter OF
        | ListVBT.TextPainter (tp) => tp.setFont (v, font)
        ELSE
        END;
        v.helper := NIL;
        v.dirmenu := NIL;
        v.suffixes := NIL;
        v.readOnly := FALSE;
        v.toSelect := "";
        v.truthInHelper := FALSE;
        v.isDir := NEW (REF ARRAY OF BOOLEAN, 100);
        v.topCell := 0;
        v.statThread := NIL;
        LOCK tlock DO
          fblist := NEW (FBList, car := WeakRef.FromRef (v), cdr := fblist);
          Thread.Signal (fbcond)
        END;
        v.dir := Process.GetWorkingDirectory ();
      END
    EXCEPT
    | OSError.E (code) => CallError (v, code); v.dir := ""
    END;
    RETURN v
  END Init;


PROCEDURE InsertCells (v: T; at: ListVBT.Cell; n: CARDINAL) =
  (* Insert the "isDir" bits, too. *)
  VAR
    count   := v.count ();
    first   := MAX (0, MIN (at, count));
    oldbits := v.isDir;
    oldsize := NUMBER (oldbits^);
  BEGIN
    Public.insertCells (v, at, n);
    IF n + count > oldsize THEN
      v.isDir :=
        NEW (REF ARRAY OF BOOLEAN, MAX (n + count, oldsize + oldsize DIV 2));
      SUBARRAY (v.isDir^, 0, oldsize) := oldbits^
    END;
    SUBARRAY (v.isDir^, first + n, count - first) :=
      SUBARRAY (v.isDir^, first, count - first);
    FOR i := first TO first + n - 1 DO v.isDir [i] := FALSE END
  END InsertCells;
    
PROCEDURE RemoveCells (v: T; at: ListVBT.Cell; n: CARDINAL) =
  (* Delete (shift) the "isDir" bits, too. *)
  VAR
    count  := v.count ();
    first  := MAX (0, MIN (at, count));
    amount := MIN (at + n, count) - first;
    k      := count - (first + amount);
  BEGIN
    Public.removeCells (v, at, n);
    IF amount > 0 THEN
      SUBARRAY (v.isDir^, first, k) := SUBARRAY (v.isDir^, first + amount, k)
    END
  END RemoveCells;

PROCEDURE ReportVisible (v: T;  firstCell: ListVBT.Cell;
                         <*UNUSED*> num: CARDINAL) =
  (* LL.sup = v *)
  BEGIN
    v.topCell := firstCell;
  END ReportVisible;

PROCEDURE GetValue (v: T; this: ListVBT.Cell): REFANY =
  (* Strip off the directory marker if this is a directory. *)
  VAR val: Pathname.T := Public.getValue (v, this);
  BEGIN
    IF v.isDir [this] THEN
      val := Text.Sub (val, 0, Text.Length (val) - DirMarkerLength)
    END;
    RETURN val
  END GetValue;

<* EXPORTED *>
PROCEDURE Refresh (v: T) =
  <* LL = {} *>
  BEGIN
    LOCK v.mu DO
      IF VBT.Domain (v) = Rect.Empty THEN RETURN END;
      TRY
        IF DirChanged (v) THEN DisplayDir (v) END
      EXCEPT
      | OSError.E (code) =>
          CallError (v, code);
          v.dir := "";
          v.removeCells (0, LAST (CARDINAL));
      END
    END
  END Refresh;

PROCEDURE DirChanged (v: T): BOOLEAN
  RAISES {OSError.E} =
  BEGIN
    IF on_unix THEN
      RETURN FS.Status (v.dir).modificationTime > v.display_time;
    ELSE
      (* Windows doesn't maintain time stamps for its directories. No surprise! *)
      RETURN Time.Now () > v.display_time + WindowsDelay;
    END;
  END DirChanged;

PROCEDURE Watcher (<* UNUSED *> cl: Thread.Closure): REFANY =
  <* LL = {} *>
  (* This loops forever.  It waits until there are some filebrowsers, then it
     refreshes them all and sleeps for a second. *)
  VAR
    v   : T;
    list: FBList;
  BEGIN
    LOOP
      LOCK tlock DO
        WHILE fblist = NIL DO Thread.Wait (tlock, fbcond) END;
        list := fblist;
        v := WeakRef.ToRef (list.car);
        IF v = NIL THEN          (* The last one is gone. *)
          fblist := NIL
        ELSE
          Refresh (v);
          WHILE list.cdr # NIL DO (* Any more? *)
            v := WeakRef.ToRef (list.cdr.car);
            IF v = NIL THEN      (* It's gone. *)
              list.cdr := list.cdr.cdr (* (pop (cdr list)) *)
            ELSE
              list := list.cdr;  (* (pop list) *)
              Refresh (v)
            END                  (* IF *)
          END                    (* WHILE *)
        END                      (* IF *)
      END;                       (* LOCK *)
      Thread.Pause (1.0D0)
    END                          (* LOOP *)
  END Watcher;

<* EXPORTED *>
PROCEDURE SetHelper (v: T; helper: Helper) =
  BEGIN
    LOCK v.mu DO
      v.helper := helper;
      IF helper # NIL THEN helper.parent := v END
    END
  END SetHelper;

PROCEDURE InitDirMenu (dm    : DirMenu;
                       font  : Font.T    := Font.BuiltIn;
                       shadow: Shadow.T  := NIL; (* Shadow.None *)
                       n     : CARDINAL  := 0                      ):
  DirMenu =
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    dm.shadow := shadow;
    dm.font := font;
    dm.top := NEW (TextVBT.T).init ("");
    dm.vbox := NEW (DirMenuVBox, dm := dm).init (Axis.T.Ver);
    WITH feedback = NEW (DirMenuTop, dm := dm).init (NIL, shadow),
         menuFrame = NEW (ShadowedVBT.T).init (
                       NIL, shadow, Shadow.Style.Raised) DO
      EVAL AnchorSplit.T.init (dm, feedback, menuFrame, n);
      MultiSplit.AddChild (dm, dm.top);
      MultiSplit.AddChild (dm, dm.vbox);
      RETURN dm
    END
  END InitDirMenu;

PROCEDURE DMVBoxShape (vbox: DirMenuVBox; ax: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Ver THEN
      RETURN HVSplit.T.shape (vbox, ax, n)
    ELSE                         (* Match the width of the top button. *)
      VAR
        op          : PaintOp.T;     (* UNUSED *)
        txt         : Pixmap.T;      (* UNUSED *)
        borderSizeMM: REAL;
        borderedVBT : BorderedVBT.T := VBT.Parent (vbox);
      BEGIN
        BorderedVBT.Get (borderedVBT, borderSizeMM, op, txt);
        WITH borderSizeRealPixels = VBT.MMToPixels (vbox, borderSizeMM, ax),
             shadowSizeMM         = vbox.dm.shadow.size,
             shadowSizeRealPixels = VBT.MMToPixels (vbox, shadowSizeMM, ax),
             buttonWidth          = Rect.HorSize (VBT.Domain (vbox.dm)),
             w = ROUND (
                   FLOAT (buttonWidth)
                     - 2.0 * (borderSizeRealPixels + shadowSizeRealPixels)),
             myWidth = HVSplit.T.shape (vbox, ax, n).pref,
             width   = MAX (w, myWidth)                    DO
          RETURN VBT.SizeRange {width, width, width + 1}
        END
      END
    END
  END DMVBoxShape;

PROCEDURE SetFontDirMenu (dm: DirMenu; font: Font.T) =
  BEGIN
    dm.font := font;
  END SetFontDirMenu;

PROCEDURE SetDirMenu (v: T; dm: DirMenu) =
  BEGIN
    LOCK v.mu DO
      v.dirmenu := dm;
      IF dm # NIL THEN dm.filebrowser := v; END
    END
  END SetDirMenu;

(*************************  Client interface  ***********************)

<* EXPORTED *>
PROCEDURE SetReadOnly (v: T; readOnly: BOOLEAN) =
  BEGIN
    LOCK v.mu DO v.readOnly := readOnly END
  END SetReadOnly;

<* EXPORTED *>
PROCEDURE SetSuffixes (v: T; suffixes: TEXT) =
  BEGIN
    WITH list = ParseSuffixes (suffixes) DO
      LOCK v.mu DO
        v.suffixes := list;
        v.display_time := 0.0D0;       (* force true redisplay next chance *)
        VBT.Mark (v)
      END
    END
  END SetSuffixes;

PROCEDURE ParseSuffixes (suffixes: TEXT): TextList.T =
  VAR
    list  : TextList.T := NIL;
    rd                 := TextRd.New (suffixes);
    suffix: TEXT;
  <* FATAL Thread.Alerted *>
  BEGIN
    TRY
      TRY
        LOOP
          Lex.Skip (rd, ISOChar.All - ISOChar.AlphaNumerics);
          suffix := Lex.Scan (rd, ISOChar.AlphaNumerics);
          IF Text.Empty (suffix) THEN EXIT END;
          list := TextList.Cons (suffix, list)
        END
      FINALLY
        Rd.Close (rd)
      END
    EXCEPT
    | Rd.Failure =>
    END;
    RETURN list
  END ParseSuffixes;

<* EXPORTED *>
PROCEDURE Set (v: T; path: Pathname.T; time: VBT.TimeStamp := 0)
  RAISES {Error} =
  <* LL.sup = VBT.mu *>
  VAR file, abs: Pathname.T; type: File.Type;
  BEGIN
    LOCK v.mu DO
      TRY
        IF NOT Pathname.Absolute (path) THEN
          path := JoinPath (v.dir, path);
        END;
        TRY
          abs := FS.GetAbsolutePathname (path);
          type := FS.Status (abs).type;
          IF type = RegularFile.FileType THEN
            v.dir := Pathname.Prefix (abs);
            file := Pathname.Last (abs);
          ELSIF type = FS.DirectoryFileType THEN
            v.dir := abs; file := "";
          ELSE                   <* ASSERT FALSE *>
          END
        EXCEPT
        | OSError.E (c) =>
            (* That name failed, but maybe this isn't a readonly filebrowser,
               and it's a "new" filename in an existing directory.  Check the
               parent directory (prefix). *)
            IF v.readOnly THEN RAISE OSError.E (c) END; (* Nope. *)
            file := Pathname.Last (path);
            path := Pathname.Prefix (path);
            abs := FS.GetAbsolutePathname (path);
            (* If that failed, the parent-directory didn't exist, either, so
               let the caller handle this exception. *)
            IF FS.Status (abs).type = FS.DirectoryFileType THEN
              v.dir := abs
            ELSE
              (* The "parent" exists, but it isn't a directory. *)
              RaiseError (v, "Not a directory", path)
            END                  (* IF *)
        END                      (* inner TRY *)
      EXCEPT
      | OSError.E (c) => RaiseError (v, Atom.ToText (c.head), path)
      END;                       (* outer TRY *)
      v.toSelect := file;
      v.display_time := 0.0D0; (* trigger the Watcher to redisplay "v". *)
      ShowFileInHelper (v, file, time);
    END                          (* LOCK *)
  END Set;

<* EXPORTED *>
PROCEDURE Unselect (v: T) =
  BEGIN
    LOCK v.mu DO
      v.toSelect := "";
      v.selectNone ();
    END;
  END Unselect;

<* EXPORTED *>
PROCEDURE GetDir (v: T): Pathname.T =
  BEGIN
    LOCK v.mu DO RETURN v.dir END
  END GetDir;

<* EXPORTED *>
PROCEDURE GetFile (v: T): Pathname.T RAISES {Error} =
  BEGIN
    WITH files = GetFiles (v) DO
      IF files = NIL THEN RETURN "" ELSE RETURN files.head END
    END
  END GetFile;

<* EXPORTED *>
PROCEDURE GetFiles (v: T): TextList.T RAISES {Error} =
  BEGIN
    LOCK v.mu DO
      IF v.truthInHelper THEN
        VAR file := TextPort.GetText (v.helper);
        BEGIN
          IF NOT Pathname.Valid (file) THEN
            RaiseError (v, "Invalid pathname", file)
          ELSIF NOT Pathname.Absolute (file) THEN
            file := JoinPath (v.dir, file)
          END;
          RETURN TextList.List1 (file)
        END
      ELSIF Text.Empty (v.dir) THEN
        RETURN NIL
      ELSE
        VAR res: TextList.T := NIL;
        BEGIN
          FOR i := v.count () - 1 TO 0 BY -1 DO
            IF v.isSelected (i) THEN
              res := TextList.Cons (JoinPath (v.dir, v.getValue (i)), res)
            END
          END;
          RETURN res
        END
      END
    END
  END GetFiles;

(**********************  Displaying a directory  ***********************)

CONST DirMarker = " (dir)";
VAR DirMarkerLength := Text.Length (DirMarker);

PROCEDURE DisplayDir (v: T) =
  (* Display the directory v.dir, which might or might not really be
     accessible.  If it isn't accessible, call v.error. *)
  <* LL = v.mu *>
  VAR
    allfiles: TextList.T := NIL; (* Entire directory, except .  and .. *)
    satfiles: TextList.T := NIL; (* Files that have OK suffixes *)
  VAR
    oldCount := v.count ();
    newCount := 0;
    cl       := NEW (StatCl, v := v); (* Thread closure *)
  BEGIN
    IF v.statThread # NIL THEN Thread.Alert (v.statThread) END;
    v.display_time := Time.Now (); (* set it now in case we raise an error later *)
    VBT.SetCursor (v, Cursor.NotReady);
    TRY
      (* find the files that match the current suffix set *)
      allfiles := TextListSort.SortD (Directory (v.dir));
      cl.files := allfiles;
      IF v.suffixes = NIL THEN
        satfiles := allfiles
      ELSE
        WHILE allfiles # NIL DO
          IF SuffixMatch (allfiles.head, v.suffixes) THEN
            satfiles := TextList.Cons (allfiles.head, satfiles)
          END;
          allfiles := allfiles.tail
        END;
        satfiles := TextList.ReverseD (satfiles)
      END;

      (* make sure we have the right number of slots *)
      newCount := TextList.Length (satfiles) + 2;
      IF oldCount < newCount THEN
        v.insertCells (oldCount, newCount - oldCount)
      ELSIF newCount < oldCount THEN
        v.removeCells (newCount, oldCount - newCount)
      END;

      (* rebuild the list *)
      v.selectNone ();
      SetValue (v, 0, Pathname.Current, TRUE);
      SetValue (v, 1, Pathname.Parent,  TRUE);
      FOR i := 2 TO newCount - 1 DO
        SetValue (v, i, satfiles.head, FALSE);  (* assume isDir=FALSE for now... *)
        satfiles := satfiles.tail;
      END;
      v.scrollTo (v.topCell);

      ShowDirInMenu (v);
      v.statThread := Thread.Fork (cl);  (* add directories to the list lazily *)
    EXCEPT
    | OSError.E (e) => CallError (v, e)
    END
  END DisplayDir;

PROCEDURE Directory (dir: Pathname.T): TextList.T RAISES {OSError.E} =
  (* Return a list of all the files in the directory. *)
  VAR
    files: TextList.T := NIL;
    iter              := FS.Iterate (dir);
    name : Pathname.T;
  BEGIN
    TRY
      WHILE iter.next (name) DO files := TextList.Cons (name, files) END;
      RETURN files
    FINALLY
      iter.close ()
    END
  END Directory;

PROCEDURE SuffixMatch (file: Pathname.T;  suffixes: TextList.T): BOOLEAN =
  VAR ext := Pathname.LastExt (file);
  BEGIN
    IF Text.Empty (ext) THEN ext := "$" END;
    WHILE suffixes # NIL DO
      IF FileNameEq (ext, suffixes.head) THEN RETURN TRUE END;
      suffixes := suffixes.tail
    END;
    RETURN FALSE
  END SuffixMatch;

PROCEDURE SetValue (v: T;  index: INTEGER;  name: TEXT;  isDir: BOOLEAN) =
  CONST Tail = ARRAY BOOLEAN OF TEXT { "", DirMarker };
  BEGIN
    v.isDir [index] := isDir;
    v.setValue (index, name & Tail [isDir]);
    IF NOT Text.Empty (v.toSelect) AND FileNameEq (name, v.toSelect) THEN
      v.selectOnly (index);
    END;
  END SetValue;

TYPE
  StatCl = Thread.Closure OBJECT
             v    : T;
             files: TextList.T;
           OVERRIDES
             apply := DoStats
           END;

PROCEDURE DoStats (cl: StatCl): REFANY =
  (* Update the displayed file list to include any directories
     and fix any missing "DirMarker" tags. *)
  VAR
    file : Pathname.T;
    cmp  : INTEGER;
    i                 := 2;      (* We're skipping over Current and Parent *)
    v                 := cl.v;
    count             := v.count ();
  BEGIN
    TRY
      WHILE cl.files # NIL DO
        file := cl.files.head;
        cl.files := cl.files.tail;
        TRY
          IF FS.Status (JoinPath (v.dir, file)).type = FS.DirectoryFileType THEN
            LOCK v.mu DO
              IF Thread.TestAlert () THEN RETURN NIL END;
              LOOP
                IF (i = count) THEN (* end-of-list *) cmp := +1; EXIT; END;
                cmp := Text.Compare (v.getValue (i), file);
                IF (cmp >= 0) THEN EXIT; END;
                INC (i);
              END;
              IF (cmp # 0) THEN  v.insertCells (i, 1); INC (count); END;
              SetValue (v, i, file, TRUE); INC (i);
            END                  (* LOCK *)
          END                    (* IF *)
        EXCEPT
        | OSError.E (c) => CallError (v, c)
        END                      (* TRY *)
      END                        (* WHILE *)
    FINALLY
      VBT.SetCursor (v, Cursor.DontCare)
    END;                         (* TRY *)
    RETURN NIL
  END DoStats;
           
PROCEDURE InitDirMenuButton (dmb: DirMenuButton; text: TEXT): DirMenuButton =
  VAR
    textvbt := TextVBT.New (text, fnt := dmb.dm.font, bgFg := dmb.dm.shadow,
                            halign := 0.0, hmargin := 2.0);
    menubutton := ShadowedFeedbackVBT.NewMenu (textvbt, dmb.dm.shadow);
  BEGIN
    EVAL MenuSwitchVBT.T.init (dmb, menubutton);
    RETURN dmb
  END InitDirMenuButton;

PROCEDURE DirMenuButtonPut (dmb: DirMenuButton; text: TEXT) =
  VAR
    menubutton: ShadowedFeedbackVBT.T := Filter.Child (dmb);
    textvbt   : TextVBT.T             := MultiFilter.Child (menubutton);
  BEGIN
    TextVBT.SetFont (textvbt, dmb.dm.font, dmb.dm.shadow);
    TextVBT.Put (textvbt, text)
  END DirMenuButtonPut;

PROCEDURE DirMenuButtonGet (dmb: DirMenuButton): TEXT =
  VAR
    menubutton: ShadowedFeedbackVBT.T := Filter.Child (dmb);
    textvbt   : TextVBT.T             := MultiFilter.Child (menubutton);
  BEGIN
    RETURN TextVBT.Get (textvbt)
  END DirMenuButtonGet;

PROCEDURE DirMenuButtonCallback (         dmb: DirMenuButton;
                                 READONLY cd : VBT.MouseRec   ) =
  <* LL = VBT.mu *>
  <* FATAL Split.NotAChild, Pathname.Invalid *>
  VAR
    arcs := NEW(Pathname.Arcs).init();
    vbox := dmb.dm.vbox;
    next := dmb;
    pn: Pathname.T := "MaryHadALittleLamb";
  BEGIN
    arcs.addlo(dmb.get());
    LOOP
      next := Split.Succ(vbox, next);
      IF next = NIL THEN EXIT END;
      arcs.addlo(next.get());
    END;
    pn := Pathname.Compose(arcs);

    TRY
      Set(dmb.dm.filebrowser, pn, cd.time)
    EXCEPT Error (e) => 
      dmb.dm.filebrowser.error(e)
    END;
  END DirMenuButtonCallback;

(***************************  User interface  **************************)

PROCEDURE InsideClick (         s   : Selector;
                       READONLY cd  : VBT.MouseRec;
                                this: ListVBT.Cell  ) =
  <* LL = VBT.mu *>
  VAR v := s.v;
  VAR
    first: ListVBT.Cell;
    path : Pathname.T;
    isDir: BOOLEAN;
    event               := AnyEvent.FromMouse (cd);
  BEGIN
    ListVBT.MultiSelector.insideClick (s, cd, this);
    ShowFileInHelper (v, "", cd.time);
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      LOCK v.mu DO
        (* let the Watcher know about the new selection *)
        IF v.getFirstSelected (first) THEN v.toSelect := v.getValue (first); END;
      END;
      v.selectItems (event)
    ELSIF cd.clickType = VBT.ClickType.LastUp AND cd.clickCount = 3 THEN
      LOCK v.mu DO
        IF NOT v.getFirstSelected (first) THEN (* error? *) RETURN END;
        isDir := v.isDir [first];
        path := JoinPath (v.dir, v.getValue (first))
      END;
      IF isDir THEN
        v.activateDir (path, event)
      ELSE
        v.activateFile (path, event)
      END
    END
  END InsideClick;

PROCEDURE SelectItems (<* UNUSED *> v: T; <* UNUSED *> event: AnyEvent.T) =
  BEGIN
  END SelectItems;

PROCEDURE ActivateFile (<* UNUSED *> v       : T;
                        <* UNUSED *> filename: Pathname.T;
                        <* UNUSED *> event   : AnyEvent.T) =
  BEGIN
  END ActivateFile;

PROCEDURE ActivateDir (v: T; dirname: Pathname.T; event: AnyEvent.T) =
  <* LL.sup = VBT.mu *>
  VAR time := AnyEvent.TimeStamp (event);
  BEGIN
    TRY Set (v, dirname, time) EXCEPT Error (x) => v.error (x) END
  END ActivateDir;

PROCEDURE DefaultError (<* UNUSED *> v: T; <* UNUSED *> err: E) =
  BEGIN
  END DefaultError;

PROCEDURE ShowFileInHelper (v: T; file: Pathname.T; time: VBT.TimeStamp) =
  <* LL = v.mu *>
  VAR forHelper: Pathname.T;
  BEGIN
    IF v.helper = NIL THEN RETURN END;
    (* Prevent TextPort from calling "v.helper.modified ()" (which is
       HelperModified) when we do the following SetText.  HelperModified
       unselects everything and sets v.truthInHelper to TRUE. *)
    TextPort.SetModified (v.helper, TRUE);
    IF v.dirmenu = NIL OR Text.Empty (file) THEN
      forHelper := file
    ELSE
      forHelper := Pathname.Last (file)
    END;
    TextPort.SetText (v.helper, forHelper);
    v.truthInHelper := NOT Text.Empty(forHelper);
    IF time # 0 AND NOT Text.Empty (forHelper) THEN
      TextPort.Select (v.helper, time := time, replaceMode := TRUE)
    END;
    (* Re-enable "v.helper.modified()" *)
    TextPort.SetModified (v.helper, FALSE);
  END ShowFileInHelper;

PROCEDURE ShowDirInMenu (v: T) =
  <* LL = v.mu *>
  <* FATAL Split.NotAChild *>
  <* FATAL Pathname.Invalid *>
  BEGIN
    IF v.dirmenu = NIL THEN RETURN END;
    VAR
      top  := v.dirmenu.top;
      arcs := Pathname.Decompose(v.dir);
    BEGIN
      IF arcs = NIL THEN TextVBT.Put(top, "????"); RETURN END;
      TextVBT.SetFont(top, fnt := v.dirmenu.font, bgFg := v.dirmenu.shadow);

      (* remove trailing arcs that are empty or NIL *)
      VAR arc: TEXT; BEGIN
        LOOP
          IF (arcs.size() = 0) THEN
            TextVBT.Put(top, "????");
            EXIT;
          END;
          arc := arcs.remhi();
          IF (arc # NIL) AND NOT Text.Empty (arc) THEN
            TextVBT.Put(top, arc);
            EXIT;
          END;
        END;
      END;

      (* update the menu buttons *)
      VAR
        vbox     : HVSplit.T;
        arc      : TEXT;
        prevChild: VBT.T;
        thisChild: DirMenuButton;
      BEGIN
        vbox := v.dirmenu.vbox;
        prevChild := NIL;
        LOOP
          thisChild := Split.Succ(vbox, prevChild);
          IF arcs.size() = 0 THEN arc := NIL ELSE arc := arcs.remhi() END;
          IF thisChild = NIL AND arc = NIL THEN EXIT END;
          IF thisChild = NIL THEN (* new path longer than prev; add a
                                     child *)
            thisChild := NEW(DirMenuButton, dm := v.dirmenu).init(arc);
            Split.Insert(vbox, prevChild, thisChild);
            prevChild := thisChild
          ELSIF arc = NIL THEN   (* new path shorter than prev; delete a
                                    child *)
            Split.Delete(vbox, thisChild)
          ELSE                   (* change an arc *)
            thisChild.put(arc);
            prevChild := thisChild
          END
        END
      END
    END
  END ShowDirInMenu;

(*
PROCEDURE ShowDirInMenu (v: T) =
  <* LL = v.mu *>
  <* FATAL Split.NotAChild *>
  VAR
    dm                       := v.dirmenu;
    vbox     : HVSplit.T;
    prevChild: VBT.T         := NIL;
    thisChild: DirMenuButton;
    arcs     : Pathname.Arcs;
  <* FATAL Pathname.Invalid *>
  BEGIN
    IF dm = NIL THEN RETURN END;
    vbox := dm.vbox;
    arcs := Pathname.Decompose (v.dir);
    WITH curr = arcs.remhi () DO
      IF curr = NIL THEN 
        TextVBT.Put (dm.top, "????")
      ELSE
        TextVBT.Put (dm.top, curr)
      END
    END;
    LOOP
      thisChild := Split.Succ (vbox, prevChild);
      IF thisChild = NIL THEN
        IF arcs.size () = 0 THEN
          EXIT
        ELSE
          thisChild :=
            NEW (DirMenuButton, dm := dm).init (arcs.remhi ());
          Split.Insert (vbox, prevChild, thisChild);
          prevChild := thisChild
        END
      ELSIF arcs.size () = 0 THEN     (* delete remaining children *)
        Split.Delete (vbox, Split.Succ (vbox, prevChild))
      ELSE
        thisChild.put (arcs.remhi ());
        prevChild := thisChild
      END
    END
  END ShowDirInMenu;
*)

PROCEDURE HelperModified (hp: Helper) =
  <* LL = v.mu *>
  (* That's the locking level because this is the "modified" method of the
     Helper, which is invoked by TextPort.ReplaceInVText, which is called by
     TextPort.SetText, which is called by ShowFileInHelper and others. *)
  BEGIN
    WITH v = hp.parent DO v.selectNone (); v.truthInHelper := TRUE END
  END HelperModified;

PROCEDURE HelperReturn (hp: Helper; READONLY event: VBT.KeyRec) =
  <* LL = VBT.mu *>
  VAR
    v    := hp.parent;
    text := TextPort.GetText (hp);
  BEGIN
    TRY
      LOCK v.mu DO
        IF NOT Pathname.Valid (text) THEN
          RaiseError (v, "Invalid pathname", text)
        END;
        IF NOT Pathname.Absolute (text) THEN
          text := JoinPath (v.dir, text)
        END
      END;
      Set (v, text, event.time);
      text := TextPort.GetText(hp);
      IF NOT Text.Empty (text) THEN 
        v.activateFile(text, AnyEvent.FromKey(event))
      END
    EXCEPT
    | Error (x) => v.error (x)
    END
  END HelperReturn;

PROCEDURE JoinPath (dir, file: TEXT): TEXT =
  BEGIN
    IF (dir = NIL) THEN RETURN file; END;
    IF (file = NIL) OR Text.Empty (file) THEN RETURN dir; END;
    RETURN Pathname.Join (dir, file, NIL);
  END JoinPath;

PROCEDURE FileNameEq (a, b: TEXT): BOOLEAN =
  BEGIN
    IF on_unix
      THEN RETURN Text.Equal (a, b);
      ELSE RETURN CIEqual (a, b);
    END;
  END FileNameEq;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN =
  (* Case-insensitive TEXT comparisons *)
  VAR
    len1 := Text.Length (a);
    len2 := Text.Length (b);
    c1, c2: CHAR;
    b1, b2: ARRAY [0..63] OF CHAR;
  BEGIN
    IF (len1 # len2) THEN RETURN FALSE; END;
    len2 := 0;
    WHILE (len2 < len1) DO
      Text.SetChars (b1, Text.Sub (a, len2, LAST (CARDINAL)));
      Text.SetChars (b2, Text.Sub (b, len2, LAST (CARDINAL)));
      FOR i := 0 TO MIN (len1 - len2, NUMBER (b1))-1 DO
        c1 := ASCII.Upper [b1[i]];
        c2 := ASCII.Upper [b2[i]];
        IF (c1 # c2) THEN RETURN FALSE; END
      END;
      INC (len2, NUMBER (b1));
    END;
    RETURN TRUE;
  END CIEqual;

PROCEDURE RaiseError (v: T; text, path: TEXT := "") RAISES {Error} =
  BEGIN
    RAISE Error (NEW (E, v := v, text := text, path := path))
  END RaiseError;

PROCEDURE CallError (v: T; e: OSError.Code) =
  VAR text := "";
  BEGIN
    WHILE e # NIL DO
      text := text & Atom.ToText (e.head) & " ";
      e := e.tail
    END;
    v.error (NEW (E, v := v, text := text, path := v.dir))
  END CallError;

BEGIN
  EVAL Thread.Fork (NEW (Thread.Closure, apply := Watcher))
END FileBrowserVBT.
