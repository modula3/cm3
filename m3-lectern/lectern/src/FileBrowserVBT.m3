(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Aug 22 15:34:30 PDT 1994 by mhb                      *)
(*      modified on Mon Jul 12 08:29:22 PDT 1993 by mcjones                  *)
(*      modified on Fri Jun 11 23:32:15 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:52 PDT 1992 by muller                   *)
(*      modified on Fri Mar 20 22:42:45 PST 1992 by steveg                   *)
(*      modified on Thu Apr  4 18:16:18 PST 1991 by brooks                   *)
<* PRAGMA LL                                                                 *>
<* PRAGMA EXPORTED                                                           *>

MODULE FileBrowserVBT;

IMPORT AnchorSplit, AnyEvent, Atom, Axis, BorderedVBT, Cursor, File, Filter,
       Font, FS, HVSplit, ISOChar, Lex, ListVBT, MenuSwitchVBT, MultiFilter,
       MultiSplit, OSError, PaintOp, Pathname, Pixmap, Process, Rd, Rect,
       RegularFile, Shadow, ShadowedVBT, ShadowedFeedbackVBT, Split, Text,
       TextList, TextListSort, TextPort, TextRd, TextVBT, Thread, Time,
       TypeinVBT, VBT, WeakRef;

REVEAL
  T = Public BRANDED "FileBrowserVBT 4.0" OBJECT
        mu: MUTEX;
        <* LL = mu *>
        helper  : Helper;
        dirmenu : DirMenu;
        suffixes: TextList.T;
        readOnly: BOOLEAN;
        dir     : Pathname.T;
        toSelect: TEXT; (* if non-empty/NIL, select this string *)
        truthInHelper: BOOLEAN;   (* where to look for the value *)
        time         : Time.T;  (* last time we looked at this directory *)
        statThread   : Thread.T;
        isDir        : REF ARRAY OF BOOLEAN
      OVERRIDES
        init         := Init;
        selectItems  := SelectItems; (* no-op *)
        activateFile := ActivateFile; (* no-op *)
        activateDir  := ActivateDir;
        error        := DefaultError; (* no-op *)
        insertCells  := InsertCells;
        removeCells  := RemoveCells;
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
              init := InitDirMenu
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
        IF FS.Status (v.dir).modificationTime > v.time THEN DisplayDir (v) END
      EXCEPT
      | OSError.E (code) =>
          v.dir := "";
          v.removeCells (0, LAST (CARDINAL));
          CallError (v, code)
      END
    END
  END Refresh;

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
PROCEDURE SetHelper (v: T; helper: Helper) RAISES {Error} =
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
    dm.top := NEW (TextVBT.T).init ("", fnt := font, bgFg := shadow);
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

<* EXPORTED *>
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
        v.time := 0.0D0;             (* force true redisplay next chance *)
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
          path := Pathname.Join (v.dir, path, NIL)
        END;
        TRY
          abs := FS.GetAbsolutePathname (path);
          type := FS.Status (abs).type;
          IF type = RegularFile.FileType THEN
              v.dir := Pathname.Prefix (abs);
              file := Pathname.Last (abs)
          ELSIF type = FS.DirectoryFileType THEN
            v.dir := abs; file := ""
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
      v.time := 0.0D0;           (* That'll trigger the Watcher. *)
      ShowFileInHelper (v, file, time);
    END                          (* LOCK *)
  END Set;

<* EXPORTED *>
PROCEDURE Unselect (v: T) =
  BEGIN
    LOCK v.mu DO v.selectNone () END
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
            file := Pathname.Join (v.dir, file, NIL)
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
              res := TextList.Cons (
                       Pathname.Join (v.dir, v.getValue (i), NIL), res)
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
    this     := -1; (* entry to select *)
    cl       := NEW (StatCl, v := v); (* Thread closure *)
  PROCEDURE satisfies (file: Pathname.T): BOOLEAN =
    VAR
      ext      := Pathname.LastExt (file);
      suffixes := v.suffixes;
    BEGIN
      IF Text.Empty (ext) THEN ext := "$" END;
      WHILE suffixes # NIL DO
        IF Text.Equal (ext, suffixes.head) THEN RETURN TRUE END;
        suffixes := suffixes.tail
      END;
      RETURN FALSE
    END satisfies;
  BEGIN
    IF v.statThread # NIL THEN Thread.Alert (v.statThread) END;
    VBT.SetCursor (v, Cursor.NotReady);
    TRY
      allfiles := TextListSort.SortD (Directory (v.dir));
      cl.files := allfiles;
      IF v.suffixes = NIL THEN
        satfiles := allfiles
      ELSE
        WHILE allfiles # NIL DO
          IF satisfies (allfiles.head) THEN
            satfiles := TextList.Cons (allfiles.head, satfiles)
          END;
          allfiles := allfiles.tail
        END;
        satfiles := TextList.ReverseD (satfiles)
      END;
      newCount := TextList.Length (satfiles) + 2;
      IF oldCount < newCount THEN
        v.insertCells (oldCount, newCount - oldCount)
      ELSIF newCount < oldCount THEN
        v.removeCells (newCount, oldCount - newCount)
      END;
      v.isDir [0] := TRUE;       (* for Current *)
      v.isDir [1] := TRUE;       (* for Parent *)
      FOR i := 2 TO newCount - 1 DO v.isDir [i] := FALSE END;
      v.setValue (0, Pathname.Current & DirMarker);
      v.setValue (1, Pathname.Parent & DirMarker);
      FOR i := 2 TO newCount - 1 DO
        IF NOT Text.Empty (v.toSelect) AND 
          Text.Equal (satfiles.head, v.toSelect) THEN 
          this := i; v.toSelect := "";
        END;
        v.setValue (i, satfiles.head);
        satfiles := satfiles.tail;
      END;
      v.selectOnly (this);
      v.time := FS.Status (v.dir).modificationTime;
      ShowDirInMenu (v);
      v.statThread := Thread.Fork (cl)
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
    
TYPE
  StatCl = Thread.Closure OBJECT
             v    : T;
             files: TextList.T;
           OVERRIDES
             apply := DoStats
           END;

PROCEDURE DoStats (cl: StatCl): REFANY =
  VAR
    file : Pathname.T;
    i                 := 2;      (* We're skipping over Current and Parent *)
    v                 := cl.v;
    count             := v.count ();
  BEGIN
    TRY
      WHILE cl.files # NIL DO
        file := cl.files.head;
        cl.files := cl.files.tail;
        TRY
          IF FS.Status (Pathname.Join (v.dir, file, NIL)).type
               = FS.DirectoryFileType THEN
            LOCK v.mu DO
              IF Thread.TestAlert () THEN RETURN NIL END;
              LOOP
                IF i = count THEN
                  v.insertCells (count, 1);
                  v.setValue (count, file & DirMarker);
                  v.isDir [count] := TRUE;
                  INC (count);
                  INC (i);
                  EXIT
                ELSE
                  WITH t = Text.Compare (v.getValue (i), file) DO
                    IF t = -1 THEN
                      INC (i)
                    ELSIF t = 0 THEN
                      v.setValue (i, file & DirMarker);
                      v.isDir [i] := TRUE;
                      INC (i);
                      EXIT
                    ELSE
                      v.insertCells (i, 1);
                      v.setValue (i, file & DirMarker);
                      v.isDir [i] := TRUE;
                      INC (count);
                      INC (i);
                      EXIT
                    END          (* IF *)
                  END            (* WITH *)
                END              (* IF *)
              END                (* LOOP *)
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
    TextVBT.Put (textvbt, text)
  END DirMenuButtonPut;

PROCEDURE DirMenuButtonGet (dmb: DirMenuButton): TEXT =
  VAR
    menubutton: ShadowedFeedbackVBT.T := Filter.Child (dmb);
    textvbt   : TextVBT.T             := MultiFilter.Child (menubutton);
  BEGIN
    RETURN TextVBT.Get (textvbt)
  END DirMenuButtonGet;

PROCEDURE DirMenuButtonCallback (dmb: DirMenuButton; READONLY cd: VBT.MouseRec) =
  <* LL = VBT.mu *>
  VAR
    arcs := NEW(Pathname.Arcs).init();
    vbox := dmb.dm.vbox;
    next := dmb;
  BEGIN
    arcs.addlo (dmb.get ());
    TRY
      LOOP
        next := Split.Succ (vbox, next);
        IF next = NIL THEN EXIT END;
        arcs.addlo (next.get ())
      END;
      Set (dmb.dm.filebrowser, Pathname.Compose (arcs), cd.time)
    EXCEPT
    | Error (e) => dmb.dm.filebrowser.error (e)
    | Pathname.Invalid, Split.NotAChild => <* ASSERT FALSE *>
    END
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
      v.selectItems (event)
    ELSIF cd.clickType = VBT.ClickType.LastUp AND cd.clickCount = 3 THEN
      LOCK v.mu DO
        IF NOT v.getFirstSelected (first) THEN (* error? *) RETURN END;
        isDir := v.isDir [first];
        path := Pathname.Join (v.dir, v.getValue (first), NIL)
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
          text := Pathname.Join (v.dir, text, NIL)
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
