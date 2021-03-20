(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Nov  5 09:41:36 PST 1996 by najork                   *)
(* Last modified on Mon Nov  4 14:10:28 PST 1996 by najork                   *)
(*       Created on Mon Jan 16 10:05:17 PST 1995 by najork                   *)


UNSAFE MODULE WinTrestle;

IMPORT Axis, Batch, Cstring, Ctypes, Env, Fmt, M3toC, Point, ProperSplit,
       Rect, Region, RTHeapRep, RTParams, RTLinker,
       ScrnColorMap, ScrnCursor, ScrnPixmap, Split, Text, Thread,
       Trestle, TrestleClass, TrestleImpl, VBT, VBTClass, VBTRep, WinBase,
       WinDef, WinGDI, WinKey, WinMsg, WinPaint, WinScreenType,
       WinScreenTypePrivate, WinScrnColorMap, WinScrnCursor, WinScrnPixmap,
       WinUser, Word;

IMPORT RTIO;

EXCEPTION FatalError;

CONST
  False = 0;
  True  = 1;

CONST
  DesktopID = 0; (* The ScreenID of the Windows desktop. *)

REVEAL
  T = Trestle.T BRANDED "WinTrestle.T" OBJECT
    screen    : WinScreenType.T;
    coverage  : CARDINAL := 0;
    current   : VBT.T    := NIL;  (* The child that is touched by the pointer,
                                     or NIL if there is no such child. *)
    mouseFocus: VBT.T    := NIL;  (* The child that has received a FirstDown
                                     but no corresponding LastUp, or NIL if
                                     there is no such child. *)
    hwnd       : WinDef.HWND;
    timerId    : WinDef.UINT;
    lastPos    := WinDef.POINT {-1, -1};
    n_cages    := 0;
    queue_status : WinDef.DWORD := 0;


  OVERRIDES
    redisplay        := Redisplay;
    beChild          := BeChild;
    replace          := Replace;
    setcage          := SetCage;
    setcursor        := SetCursor;
    paintbatch       := PaintBatch;
    sync             := Sync;
    capture          := Capture;
    screenOf         := ScreenOf;
    newShape         := NewShape;
    acquire          := Acquire;
    release          := Release;
    put              := Put;
    forge            := Forge;
    readUp           := ReadUp;
    writeUp          := WriteUp;
    attach           := Attach;
    decorate         := Decorate;
    iconize          := Iconize;
    overlap          := Overlap;
    moveNear         := MoveNear;
    installOffscreen := InstallOffScreen;
    setColorMap      := SetColorMap;
    getScreens       := GetScreens;
    captureScreen    := CaptureScreen;
    allCeded         := AllCeded;
    tickTime         := TickTime;
    trestleId        := TrestleID;
    windowId         := WindowID;
    updateChalk      := UpdateChalk;
    updateBuddies    := UpdateBuddies;
  END;

REVEAL
  Child = ProperSplit.Child BRANDED "WinTrestle.Child" OBJECT
    uid         := 0;     (* index of this child in the "roots" array *)
    offScreen   := FALSE; (* TRUE for an off-screen window *)
    cageCovered := FALSE; (* TRUE during delivery of a button click,
                              to avoid setting the cage twice. *)
    decorated   := FALSE; (* TRUE if the window is normal, FALSE if
                             override-redirect; only valid after w is
                             created. (same as in xvbt) *)
    dead_vbt    := FALSE; (* TRUE if the VBT and associated window
                             are scheduled for deletion. *)
    cageSet     := FALSE; (* TRUE if the VBT wants mouse events *)

    badR        := Region.Empty;  (* Region known to be bad to inside WinTrestle,
                              but not necessarily known in Trestle or Windows. *)

    last         : Last;                     (* last button/mouse click info *)
    title_string : Ctypes.char_star := NIL;  (* the installed title string *)
    hpal         : WinDef.HPALETTE := NIL;   (* the palette handle or NIL *)
    hwnd         : WinDef.HWND := NIL;       (* the window handle *)
    hdc          : WinDef.HDC := NIL;        (* the device context *)
    sh, sv       : VBT.SizeRange;            (* hor. and vert. window sizes *)
    trsl         : T := NIL;                 (* the Trestle on which the window is
                                                installed *)
  END;
  (* The fields of a child record are only modified via WindowProc()
     which is running in a single thread.  I guess you could say their
     LL = WindowProc. *)

(*------------------------------------------------------- Trestle methods ---*)
(* These procedures are "on the Trestle side".  They run while holding
   various Trestle and VBT locks.  If they want to perform any non-trivial
   Win32 operations, they enqueue them for the thread running WindowProc.
*)

PROCEDURE Redisplay (self: T) =
  (* LL = VBT.mu *)
  <*FATAL Split.NotAChild*>
  VAR v := Split.Succ (self, NIL);
  BEGIN
    WHILE v # NIL DO
      IF VBTClass.HasNewShape (v) AND v.st # NIL THEN
        SetShape (self, v);
      END;
      v := Split.Succ (self, v);
    END;
  END Redisplay;

PROCEDURE SetShape (trsl: T; v: VBT.T) =
  (* LL = VBT.mu *)
  VAR
    sizeChange   : BOOLEAN;
    width, height: CARDINAL := 0;
    ur           : Child    := v.upRef;
    new_shape    : BOOLEAN  := SetSizeHints (ur, width, height);
    status       : WinDef.BOOL;
    rect         : WinDef.RECT;
    a            : Arg;
  BEGIN
    (* If the window is not yet installed, bail out ... *)
    IF ur.hwnd = NIL THEN RETURN; END;
    IF NOT new_shape THEN RETURN; END;

    LOCK trsl DO
      (* Determine the current size of the window. *)
      status := WinUser.GetClientRect (ur.hwnd, ADR(rect));
      <* ASSERT status # False *>
    END;

    sizeChange := width # rect.right - rect.left OR
                  height # rect.bottom - rect.top;

    (*********
    IF (sizeChange) THEN
      DEBUG ("size change:  target size: "
              & Fmt.Int (width) & " x " & Fmt.Int (height)
              & "  current size: "
              & Fmt.Int (rect.right - rect.left) & " x "
              & Fmt.Int (rect.bottom - rect.top) & "\n");
    END;
    **********)

    IF sizeChange AND width # 0 AND height # 0 THEN
      a := NewArg ();
      a.width := width;
      a.height := height;
      PostMsg (ur, WinMsg.RESHAPE_VBT, a);
    END;
  END SetShape;

PROCEDURE BeChild (self: T; ch: VBT.T) =
  VAR ur: Child;
  BEGIN
    IF ch.upRef = NIL THEN
      ur := NewChild (self);
      ch.upRef := ur;
      ur.ch := ch;
    ELSE
      ur := NARROW (ch.upRef, Child);
      <*ASSERT ur.ch = ch *>
    END;
    ur.dead_vbt := FALSE;
    ch.parent := self;
  END BeChild;

PROCEDURE Replace (<*UNUSED*> self: T; v, new: VBT.T) =
  <* FATAL FatalError *>
  VAR ur: Child := v.upRef;
  BEGIN
    IF new # NIL THEN RAISE FatalError; END;
    DiscardVBT (ur);
    PostMsg (ur, WinMsg.DELETE_VBT, NewArg ());
  END Replace;

PROCEDURE DiscardVBT (ur: Child) =
  BEGIN
    IF ur.dead_vbt THEN RETURN; END;
    ur.dead_vbt := TRUE;
    ProperSplit.Delete (ur.trsl, ur);
    VBTClass.Misc(ur.ch, VBT.MiscRec{VBT.Deleted, VBT.NullDetail, 0, VBT.NilSel});
    VBT.Discard(ur.ch);
  END DiscardVBT;

PROCEDURE SetCage (self: T;  v: VBT.T) =
  VAR
    ur   : Child := v.upRef;
    cage := VBTClass.Cage (v);
  BEGIN
    IF v.st = NIL OR ur = NIL OR v.parent # self THEN
      IF NOT (TRUE IN cage.inOut) THEN VBTClass.ForceEscape (v) END;
      RETURN
    END;
    LOCK self DO
      IF ur.cageCovered THEN RETURN; END;
      IF NOT ur.cageSet THEN INC (self.n_cages); END;
      ur.cageSet := TRUE;
    END;
  END SetCage;

PROCEDURE SetCursor (self: T; v: VBT.T) =
  VAR ur: Child := v.upRef;
  BEGIN
    IF ur.hwnd = NIL THEN RETURN; END;
    WITH cs = v.getcursor() DO
      LOCK self DO
        WinScrnCursor.SetCursor (cs);
      END;
    END;
  END SetCursor;

PROCEDURE PaintBatch (self: T;  v: VBT.T;  ba: Batch.T) =
  (* LL.sup = self *)
  VAR
    ur : Child := v.upRef;
    a  : Arg;
  BEGIN
    IF (ur.hdc = NIL) OR (v.st = NIL) THEN
      (* the window hasn't been created yet... *)
      Batch.Free (ba);
      RETURN;
    END;

    a := NewArg ();
    a.trsl  := self;
    a.vbt   := v;
    a.batch := ba;
    PostMsg (ur, WinMsg.PAINTBATCH_VBT, a);
    EVAL WinGDI.GdiFlush ();
  END PaintBatch;

(*******************************************
PROCEDURE PaintBatch (self: T;  v: VBT.T;  ba: Batch.T) =
  (* LL.sup = self *)
  VAR
    ur     : Child := v.upRef;
    hdc    := ur.hdc;
    hwnd   := ur.hwnd;
    status : WinDef.BOOL;
  BEGIN
    IF (hdc = NIL) THEN
      (* the window hasn't been created yet... *)
      Batch.Free (ba);
      RETURN;
    END;

    ur.badR := WinPaint.PaintBatch (self, v, ba, hdc, ur.badR);
    IF NOT Region.IsEmpty (ur.badR) THEN
      (* tell trestle to repaint the new bad region *)
      VBTClass.ForceRepaint (v, ur.badR, TRUE);
      ur.badR := Region.Empty;
      (* Did we reset the bad region too soon?  What if Capture() is
         called before the bad region is really painted?? -- WKK 11/11/97 *)
    END;

    (*
     * Commenting out these two lines breaks "Fours" (the Trstle version
     * of Tetris) for unknown reasons. "hdc" is a private DC, and according
     * to the Win32 documentation, ReleaseDC "has no effect on class or
     * private DC's".
     *)
    IF (hwnd # NIL) THEN
      status := WinUser.ReleaseDC (hwnd, hdc);
      <* ASSERT status = True *>
    END;

    status := WinGDI.GdiFlush();
    <* ASSERT status # False *>
  END PaintBatch;
************************************)

(* Windows maintains batches of paint requests on a per-thread (as opposed to
   per-window) basis.  Batches are flushed by calling "GdiFlush".  Since
   "Sync" can be called by a thread different from the ones that do the
   painting (WindowProc), it is not sufficient for "Sync" to call "GdiFlush".
   Instead, we ask the WindowProc thread to call "GdiFlush" and then
   flush the current thread's batch.
*)

PROCEDURE Sync (<*UNUSED*> self: T;  v: VBT.T;  <*UNUSED*> wait: BOOLEAN) =
  VAR ur : Child := v.upRef;
  BEGIN
    IF (v.st = NIL) THEN RETURN; END;
    PostMsg (ur, WinMsg.SYNC_VBT, NewArg ());
    EVAL WinGDI.GdiFlush ();
  END Sync;


(* "Capture" combines "XPaint.Capture" and "XPaint.CapturePM". The X code
   does some pretty elaborate stuff to determine a ``bad region'' "br".
   I don't do any of that.  I guess that the sticky point is what happens
   if I try to capture a minimized window. *)

PROCEDURE Capture (            self: T;
                               v   : VBT.T;
                   READONLY    rect: Rect.T;
                   VAR (*out*) br  : Region.T): ScrnPixmap.T =
  VAR
    ur    : Child := v.upRef;
    dstDc : WinDef.HDC;
    oldBmp: WinDef.HBITMAP;
    dstBmp: WinDef.HBITMAP;
    status: WinDef.BOOL;
  BEGIN
    IF (rect.west >= rect.east) OR (v.st = NIL) OR (ur.hdc = NIL) THEN
      (* empty region, or the window hasn't been created yet... *)
      br := Region.FromRect(rect);
      RETURN NIL;
    END;

    LOCK self DO
      br := ur.badR;

      dstDc := WinGDI.CreateCompatibleDC (ur.hdc);
      <* ASSERT dstDc # NIL *>

      dstBmp := WinGDI.CreateCompatibleBitmap (ur.hdc,
                                               rect.east - rect.west,
                                               rect.south - rect.north);
      <* ASSERT dstBmp # NIL *>

      oldBmp := WinGDI.SelectObject (dstDc, dstBmp);
      <* ASSERT oldBmp # NIL *>

      status := WinGDI.BitBlt ((* hdcDest *) dstDc,
                               (* nXDest  *) 0,
                               (* nYDest  *) 0,
                               (* nWidth  *) rect.east - rect.west,
                               (* nHeight *) rect.south - rect.north,
                               (* hdcSrc  *) ur.hdc,
                               (* nXSrc   *) rect.west,
                               (* nYSrc   *) rect.north,
                               (* dwRop   *) WinGDI.SRCCOPY);
      <* ASSERT status # False *>

      status := WinGDI.DeleteDC (dstDc);
      <* ASSERT status # False *>

      RETURN WinScrnPixmap.NewPixmap (v.st, dstBmp, rect, v.st.depth);
    END;
  END Capture;


PROCEDURE ScreenOf (         self: T;
                             v   : VBT.T;
                    READONLY pt  : Point.T): Trestle.ScreenOfRec =
  VAR
    ur : Child               := v.upRef;
    st : WinScreenType.T     := v.st;
    res: Trestle.ScreenOfRec;
  BEGIN
    res.trsl := self;
    IF st = NIL OR ur = NIL OR ur.offScreen THEN
      res.id := Trestle.NoScreen
    ELSE
      LOCK self DO
        res.id := DesktopID;
        res.dom := st.rootDom;
        res.q := pt;
        IF ur.hwnd # NIL THEN
          res.q := Point.Add (pt, NorthWest(ur));
        END;
      END;
    END;
    RETURN res;
  END ScreenOf;

(* Note: The Trestle "self" must not be locked by the calling thread. *)

PROCEDURE NewShape (self: T; ch: VBT.T) =
  BEGIN
    IF ch.st # NIL THEN
      VBT.Mark(self);
    END;
  END NewShape;


PROCEDURE Fmt_Selection (s: VBT.Selection): TEXT =
  BEGIN
    IF    s = VBT.NilSel  THEN RETURN "VBT.NilSel"
    ELSIF s = VBT.Forgery THEN RETURN "VBT.Forgery"
    ELSIF s = VBT.KBFocus THEN RETURN "VBT.KBFocus"
    ELSIF s = VBT.Target  THEN RETURN "VBT.Target"
    ELSIF s = VBT.Source  THEN RETURN "VBT.Source"
    ELSE                       RETURN "Selection{" & Fmt.Int(s.sel) & "}";
    END;
  END Fmt_Selection;


PROCEDURE Acquire (<*UNUSED*> self: T;
                              v   : VBT.T;
                   <*UNUSED*> w   : VBT.T;
                              s   : VBT.Selection;
                              ts  : VBT.TimeStamp)
    (** RAISES {VBT.Error}**) =

  PROCEDURE AcquireClipboard (v: VBT.T) =
    VAR
      ur    : Child := v.upRef;
      status: WinDef.BOOL;
      handle: WinDef.HANDLE;
      mu := ARRAY [0..1] OF MUTEX { NIL, NIL };
      imu := 0;
    BEGIN

      IF ur = NIL OR ur.hwnd = NIL THEN
        RETURN;
      END;

      IF WinUser.OpenClipboard (ur.hwnd) = False THEN
        RETURN;
      END;

      TRY
        Thread.Release (v);
        mu[imu] := v;
        INC (imu);
        Thread.Release (VBT.mu);
        mu[imu] := VBT.mu;
        INC (imu);

        status := WinUser.EmptyClipboard ();
        <* ASSERT status # False *>

        handle := WinUser.SetClipboardData (WinUser.CF_TEXT, NIL);
        (* ASSERT handle # NIL *> *)

      FINALLY
        status := WinUser.CloseClipboard ();
        <* ASSERT status # False *>
        WHILE imu # 0 DO
          Thread.Acquire (mu[imu - 1]);
          DEC (imu);
        END;
      END;
    END AcquireClipboard;

  BEGIN
    IF s = VBT.Source THEN
      AcquireClipboard (v);
    ELSIF s = VBT.KBFocus THEN
      (* do nothing *)
    ELSIF s = VBT.Target THEN
      (* do nothing *)
    ELSE
      DEBUG ("Called WinTrestle.Acquire:  s = " & Fmt_Selection (s) &
            "  ts= " & Fmt.Int (ts) & "\n");
    END;
  END Acquire;


PROCEDURE Release (<*UNUSED*> self: T;
                   <*UNUSED*> v: VBT.T;
                   <*UNUSED*> w: VBT.T;
                   <*UNUSED*> s: VBT.Selection) =
  BEGIN
    (***
    DEBUG ("WARNING: WinTrestle.Release is not yet implemented \n");
    ***)
  END Release;


PROCEDURE Put (<*UNUSED*>          self  : T;
               <*UNUSED*>          ch    : VBT.T;
               <*UNUSED*>          w     : VBT.T;
               <*UNUSED*>          s     : VBT.Selection;
               <*UNUSED*>          ts    : VBT.TimeStamp;
               <*UNUSED*>          type  : VBT.MiscCodeType;
               <*UNUSED*> READONLY detail: VBT.MiscCodeDetail)
    (** RAISES {VBT.Error} **) =
  BEGIN
    <* ASSERT FALSE *> (* not yet implemented *)
  END Put;

PROCEDURE Forge (                    self  : T;
                                     v     : VBT.T;
                 <*UNUSED*>          w     : VBT.T;
                 <*UNUSED*>          type  : VBT.MiscCodeType;
                 <*UNUSED*> READONLY detail: VBT.MiscCodeDetail)
    RAISES {VBT.Error} =
  VAR ur: Child := v.upRef;
  BEGIN
    LOCK self DO
      IF ur = NIL OR ur.offScreen THEN
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      END;
      PostMsg (ur, WinMsg.FORGE_VBT, NewArg ());
    END;
  END Forge;

PROCEDURE ReadUp(<*UNUSED*> self: T;
                            ch  : VBT.T;
                 <*UNUSED*> w   : VBT.T;
                            s   : VBT.Selection;
                 <*UNUSED*> ts  : VBT.TimeStamp;
                 <*UNUSED*> tc  : CARDINAL): VBT.Value RAISES {VBT.Error} =

  PROCEDURE GetClipboard (v: VBT.T): TEXT RAISES {VBT.Error} =
    VAR
      ur     : Child := v.upRef;
      status : WinDef.BOOL;
      hglb   : WinDef.HGLOBAL;
      lptstr : Ctypes.char_star;
      res    : TEXT;
    BEGIN
      (* If ch's window handle is NIL, raise an exception. *)
      IF ur.hwnd = NIL THEN
        RAISE VBT.Error (VBT.ErrorCode.Uninstalled);
      END;

      (* Open clipboard.  If this is not possible, raise an exception. *)
      IF WinUser.OpenClipboard (ur.hwnd) = False THEN
        RAISE VBT.Error (VBT.ErrorCode.Unreadable);
      END;

      TRY
        (* Get the content of the clipboard, as an ANSI string. *)
        IF WinUser.IsClipboardFormatAvailable(WinUser.CF_TEXT) = False THEN
          RAISE VBT.Error (VBT.ErrorCode.WrongType);
        END;
        hglb := WinUser.GetClipboardData (WinUser.CF_TEXT);
        IF hglb = NIL THEN
          RAISE VBT.Error (VBT.ErrorCode.WrongType);
        ELSE
          (* Convert the ANSI C string into a VBT.Value. *)
          lptstr := WinBase.GlobalLock(hglb);
          IF lptstr # NIL THEN
            res := M3toC.CopyStoT (lptstr);
            EVAL WinBase.GlobalUnlock(hglb);
            RETURN res;
          ELSE
            RAISE VBT.Error (VBT.ErrorCode.Unreadable);
          END;
        END;
      FINALLY
        (* Close the clipboard. *)
        status := WinUser.CloseClipboard ();
        <* ASSERT status # False *>
      END;
    END GetClipboard;

  BEGIN
    IF s = VBT.Source THEN
      RETURN VBT.FromRef(GetClipboard(ch));
    ELSE
      RAISE VBT.Error (VBT.ErrorCode.Unreadable);
    END;
  END ReadUp;


PROCEDURE WriteUp (<*UNUSED*> self: T;
                   <*UNUSED*> ch  : VBT.T;
                   <*UNUSED*> w   : VBT.T;
                   <*UNUSED*> s   : VBT.Selection;
                   <*UNUSED*> ts  : VBT.TimeStamp;
                   <*UNUSED*> val : VBT.Value;
                   <*UNUSED*> tc  : CARDINAL)
  (*** RAISES {VBT.Error} ***) =
  BEGIN
    <* ASSERT FALSE *> (* not yet implemented *)
  END WriteUp;

PROCEDURE Attach (self: T; v: VBT.T) =
  BEGIN
    LOCK v DO
      LOCK self DO
        ProperSplit.Insert (self, NIL, v);
      END;
    END;
  END Attach;

(*-----------------------------------------------------------------------------
   The "decorate" method is introduced by "TrestleClass.Public".
   It is called when the decoration of "v" has changed from "old" to "new".
   There is no specification.
-----------------------------------------------------------------------------*)

PROCEDURE Decorate (<*UNUSED*> self: T;  v: VBT.T;
                    old, new: TrestleClass.Decoration) =
  VAR a: Arg;
  BEGIN
    TYPECASE v.upRef OF
    | NULL =>
        (* skip *)
    | Child (ur) =>
        a := NewArg ();
        a.old_dec := old;
        a.new_dec := new;
        PostMsg (ur, WinMsg.RETITLE_VBT, a);
    ELSE
        (* skip *)
    END;
  END Decorate;

PROCEDURE Iconize (<*UNUSED*> self: T;  v: VBT.T) =
  (* LL = VBT.mu *)
  BEGIN
    PostMsg (v.upRef, WinMsg.ICONIZE_VBT, NewArg ());
    EVAL WinGDI.GdiFlush (); (* encourage WinProc to hurry up *)
  END Iconize;

PROCEDURE Overlap (<*UNUSED*> trsl: T;
                              v   : VBT.T;
                   <*UNUSED*> id  : Trestle.ScreenID;
                     READONLY nw  : Point.T) =
  BEGIN
    InnerOverlap(v, nw, TRUE)
  END Overlap;

PROCEDURE MoveNear (self: T; v, w: VBT.T) =
  (* LL = VBT.mu *)
  VAR
    trsl: Trestle.T;
    ch  : Child;
    st  : WinScreenType.T;
    nw  := Point.T {50, 50};
  BEGIN
    (* The beginning of this procedure is a bit different from its
       counterpart in xvbt. The xvbt version has a (pretty mysterious)
       loop here. *)

    IF w # NIL THEN
      IF NOT TrestleImpl.RootChild (w, trsl, w) THEN
        w := NIL;  (* w is not installed in any trestle *)
      ELSE
        <* ASSERT self = trsl *>  (* ... just a little sanity check *)
      END;
    END;
    (* Assert: w = NIL OR w.parent = self *)

    IF w = v THEN w := NIL; END;
    (* Assert: w = NIL OR (v # w AND w.parent = self) *)

    IF w # NIL THEN
      ch := w.upRef;
      IF w.st = NIL OR ch.offScreen THEN
        w := NIL;
      END;
    END;
    (* w is NIL, or a different on-screen VBT with well-defined screen type *)

    IF w # NIL THEN
      st := w.st;
      LOCK self DO
        nw := Point.Add (nw, NorthWest (ch));
      END;
    END;

    InnerOverlap (v, nw, w # NIL);
  END MoveNear;

PROCEDURE InnerOverlap (v: VBT.T;  READONLY nw: Point.T;  knownPos: BOOLEAN) =
  (* LL = VBT.mu *)
  VAR a := NewArg ();
  BEGIN
    a.pt := nw;
    a.bool := knownPos;
    PostMsg (v.upRef, WinMsg.OVERLAP_VBT, a);
    EVAL WinGDI.GdiFlush (); (* encourage WinProc to hurry up *)
  END InnerOverlap;

PROCEDURE InstallOffScreen (self          : T;
                            v             : VBT.T;
                            width, height : CARDINAL;
                            prefst        : VBT.ScreenType) =
  VAR st : WinScreenType.T;  (** a: Arg; **)
  BEGIN
    (* Determine the screen type *)
    IF prefst.depth = 1
      THEN st := self.screen.bits;
      ELSE st := self.screen;
    END;
    TYPECASE prefst OF
    | NULL =>
    | WinScreenType.T (wst) => IF wst.trsl = self THEN st := wst END;
    ELSE
    END;

    (* Rescreen the VBT *)
    VBTClass.Rescreen(v, st);

    (* Ask the Trestle thread to create the invisible window *)
    (**
    a := NewArg ();
    a.width := width;
    a.height := height;
    PostMsg (v.upRef, WinMsg.CREATE_OFFSCREEN_VBT, a);
    ***)
    CreateOffscreen (v.upRef, width, height);

    (* Reshape the VBT *)
    VBTClass.Reshape (v, Rect.FromSize(width, height), Rect.Empty);
  END InstallOffScreen;


(*-----------------------------------------------------------------------------
   TrestleClass.Public introduces a method "setColorMap". There is no
   specification for this method. The X version of Trestle binds a procedure
   XClient.SetColorMap to the method.

   Trestle.SetColorMap is the only place within Trestle that calls this method.
   Trestle.SetColorMap is dead code, it is neither exported nor used within
   Trestle.m3. So, it appears to be safe to not provide an implementation.
-----------------------------------------------------------------------------*)

PROCEDURE SetColorMap (<*UNUSED*> self: T;
                       <*UNUSED*> v: VBT.T;
                       <*UNUSED*> cm: ScrnColorMap.T) =
  BEGIN
    <* ASSERT FALSE *>  (* not yet implemented *)
  END SetColorMap;


PROCEDURE GetScreens (self: T): Trestle.ScreenArray =
  BEGIN
    LOCK self DO
      WITH res = NEW (Trestle.ScreenArray, 1) DO
        res[0] := Trestle.Screen{
                      id    := DesktopID,
                      dom   := self.screen.rootDom,
                      delta := Point.Origin,
                      type  := self.screen};
        RETURN res
      END;
    END;
  END GetScreens;

PROCEDURE CaptureScreen (              self: T;
                         <*UNUSED*>    id  : VBT.ScreenID;
                         READONLY      clip: Rect.T;
                         VAR (* out *) br  : Region.T): ScrnPixmap.T =
  VAR
    st    := trsl.screen;
    rect  := Rect.Meet (clip, st.rootDom);
    hwnd  : WinDef.HWND;
    srcDc : WinDef.HDC;
    dstDc : WinDef.HDC;
    oldBmp: WinDef.HBITMAP;
    dstBmp: WinDef.HBITMAP;
    status: WinDef.BOOL;
  BEGIN
    br := Region.Difference (Region.FromRect (clip), Region.FromRect (rect));

    IF rect.west >= rect.east THEN
      RETURN NIL;
    END;

    LOCK self DO
      hwnd := WinUser.GetDesktopWindow ();

      srcDc := WinUser.GetDC (hwnd);
      <* ASSERT srcDc # NIL *>

      dstDc := WinGDI.CreateCompatibleDC (srcDc);
      <* ASSERT dstDc # NIL *>

      dstBmp := WinGDI.CreateCompatibleBitmap (srcDc,
                                               rect.east - rect.west,
                                               rect.south - rect.north);
      <* ASSERT dstBmp # NIL *>

      oldBmp := WinGDI.SelectObject (dstDc, dstBmp);
      <* ASSERT oldBmp # NIL *>

      status := WinGDI.BitBlt ((* hdcDest *) dstDc,
                               (* nXDest  *) 0,
                               (* nYDest  *) 0,
                               (* nWidth  *) rect.east - rect.west,
                               (* nHeight *) rect.south - rect.north,
                               (* hdcSrc  *) srcDc,
                               (* nXSrc   *) rect.west,
                               (* nYSrc   *) rect.north,
                               (* dwRop   *) WinGDI.SRCCOPY);
      <* ASSERT status # False *>

      status := WinUser.ReleaseDC (hwnd, srcDc);
      <* ASSERT status = True *>

      status := WinGDI.DeleteDC (dstDc);
      <* ASSERT status # False *>

      RETURN WinScrnPixmap.NewPixmap (st, dstBmp, rect, st.depth);
    END;
  END CaptureScreen;

PROCEDURE AllCeded (self: T): BOOLEAN =
  BEGIN
    RETURN (self.queue_status = 0);
  END AllCeded;

PROCEDURE TickTime (<*UNUSED*> self: T): INTEGER =
  BEGIN
    RETURN 1000;
  END TickTime;

PROCEDURE TrestleID (<*UNUSED*> self: T;  <*UNUSED*> v: VBT.T): TEXT =
  BEGIN
    RETURN "Default Trestle"
  END TrestleID;

PROCEDURE WindowID (<*UNUSED*> self: T;  v: VBT.T): TEXT =
  VAR num := LOOPHOLE (WindowHandle (v), INTEGER);
  BEGIN
    RETURN Fmt.Unsigned (num, base := 16);
  END WindowID;

PROCEDURE WindowHandle (v: VBT.T): WinDef.HWND =
  (* Return the window handle associated with a VBT (or NIL) *)
  BEGIN
    WHILE v # NIL DO
      TYPECASE v.upRef OF
      | NULL       => (* skip *)
      | Child (ur) => RETURN ur.hwnd
      ELSE            (* skip *)
      END;
      v := v.parent;
    END;
    RETURN NIL;
  END WindowHandle;

(*-----------------------------------------------------------------------------
   These methods are used by Shared Trestle. According to msm, we can make
   them no-ops in Windows world,where we don't have network transparency,
   much less sharing.
-----------------------------------------------------------------------------*)

PROCEDURE UpdateChalk (<*UNUSED*> t: T;
                       <*UNUSED*> v: VBT.T;
                       <*UNUSED*> chalk: TEXT) =
  BEGIN
    (* do nothing *)
  END UpdateChalk;


PROCEDURE UpdateBuddies (<*UNUSED*>          self      : T;
                         <*UNUSED*>          v         : VBT.T;
                         <*UNUSED*> READONLY trsls, ids: ARRAY OF TEXT) =
  BEGIN
    (* do nothing *)
  END UpdateBuddies;

(*****************************************************************************)

VAR
  trsl : T  := NIL;
  trslThread: Thread.T;  (* for debugging purposes ... *)

PROCEDURE DoConnect (<*UNUSED*> self     : TrestleClass.ConnectClosure;
                     <*UNUSED*> inst     : TEXT;
                     <*UNUSED*> localOnly: BOOLEAN;
                     VAR (*OUT*) t       : Trestle.T): BOOLEAN =
  BEGIN
    t := trsl;
    RETURN TRUE;
  END DoConnect;

PROCEDURE Init () =
  BEGIN
    TrestleClass.RegisterConnectClosure(
      NEW(TrestleClass.ConnectClosure, apply := DoConnect))
  END Init;

(*****************************************************************************)

(********
PROCEDURE Reshape (ch: VBT.T; width, height: CARDINAL; sendMoved := FALSE) =
  (* Reshape ch to new width and height.  If this is a no-op, but sendMoved
     is true, then send a miscellaneous code.  LL = VBT.mu *)
  BEGIN
    IF (ch.domain.east # width) OR (ch.domain.south # height) THEN
      WITH new = Rect.FromSize(width, height) DO
        VBTClass.Reshape(ch, new, Rect.Meet(ch.domain, new))
      END
    ELSIF sendMoved THEN
      VBTClass.Misc(
        ch, VBT.MiscRec{VBT.Moved, VBT.NullDetail, 0, VBT.NilSel})
    END
  END Reshape;
********)

PROCEDURE SetSizeHints (ur: Child;  VAR width, height: CARDINAL): BOOLEAN =
  VAR
    v  := ur.ch;
    s  := VBTClass.GetShapes (v);
    sh := s[Axis.T.Hor];
    sv := s[Axis.T.Ver];
    st := NARROW (v.st, WinScreenType.T);
    max_width  := MAX(MIN(sh.hi - 1, Rect.HorSize(st.rootDom)), sh.lo);
    max_height := MAX(MIN(sv.hi - 1, Rect.VerSize(st.rootDom)), sv.lo);
    changed := (sh # ur.sh) OR (sv # ur.sv);
  BEGIN
    IF (changed) THEN  ur.sh := sh; ur.sv := sv;  END;
    IF sh.pref # 0 THEN
      width := MIN (sh.pref, max_width)
    ELSIF sh.hi > 1 AND sh.hi <= width THEN
      width := max_width
    END;
    IF sv.pref # 0 THEN
      height := MIN (sv.pref, max_height)
    ELSIF sv.hi > 1 AND sv.hi <= height THEN
      height := max_height
    END;
    RETURN changed;
  END SetSizeHints;


(* "NorthWest" serves a similar purpose as the "ValidateNW" function in the
   X version. The X counterpart of "WinTrestle.T" maintains a cache "nw" for
   the northwest corner), and a flag "nwValid" that indicates whether the
   cache entry is valid. "ValidateNW" will contact the X server only if the
   cache entry is stale.

   I assume that the call to "GetWindowRect" is cheap enough to use it
   liberally. Given that, the code gets a lot simpler. *)

PROCEDURE NorthWest (ch: Child): Point.T =
  VAR
    status    :  WinDef.BOOL;
    screenPos := WinDef.POINT {0, 0};
  BEGIN
    status := WinUser.ClientToScreen (ch.hwnd, ADR (screenPos));
    <* ASSERT status # False *>
    RETURN Point.T {screenPos.x, screenPos.y};
  END NorthWest;

(*-------------------------------------- Trestle -> WinProc communication ---*)
(* Since the Trestle threads may be holding arbitrary locks and various
   Win32 calls (e.g. CreateThread and ShowWindow) send and wait for
   synchronous messages on the Win32 message queues, any non-trivial
   Win32 calls will be performed by WindowProc which is running at LL=0.

   "Arg"s are used to pass information from Trestle threads to WindowProc.
   Since they contain traced references, they must be traced.  Since they're
   passed on the Win32 message queue which is untraced, we assign each
   one a unique ID and pass the ID instead of the REF.
*)

(****************
TYPE
  ArgResult = MUTEX OBJECT
    done: BOOLEAN           := FALSE;
    cond: Thread.Condition  := NIL;
  END;

PROCEDURE NewResult (): ArgResult =
  BEGIN
    RETURN NEW (ArgResult, done := FALSE, cond := NEW (Thread.Condition));
  END NewResult;

PROCEDURE WaitForResult (res: ArgResult) =
  BEGIN
DEBUG ("waiting for result...\n");
    LOCK res DO
      WHILE NOT res.done DO Thread.Wait (res, res.cond); END;
    END;
DEBUG ("done.\n");
  END WaitForResult;
************)

TYPE
  Arg = REF RECORD
    next    : Arg;
    ch      : Child;
    width   : INTEGER;
    height  : INTEGER;
    uid     : INTEGER;
    pt      : Point.T;
    bool    : BOOLEAN;
    old_dec : TrestleClass.Decoration;
    new_dec : TrestleClass.Decoration;
    batch   : Batch.T;
    vbt     : VBT.T;
    trsl    : T;
    (** result  : ArgResult; **)
  END;

VAR (* LL = arg_mu *)
  arg_mu    : MUTEX   := NEW (MUTEX);
  free_args : Arg     := NIL;
  arg_head  : Arg     := NIL;
  arg_tail  : Arg     := NIL;
  n_args    : INTEGER := 0;

PROCEDURE NewArg (): Arg =
  VAR a: Arg;
  BEGIN
    LOCK arg_mu DO
      IF free_args # NIL THEN
        a := free_args;
        free_args := a.next;
      ELSE
        INC (n_args);
        a := NEW (Arg);
        a.uid := n_args;
      END;
    END;
    a.next := NIL;
    RETURN a;
  END NewArg;

PROCEDURE PostMsg (ch: Child;  msg: INTEGER;  a: Arg) =
  VAR hwnd := ch.hwnd;
  BEGIN
    IF (hwnd = NIL) THEN hwnd := trsl.hwnd; END;

    <*ASSERT a.next = NIL*>
    a.ch := ch;

    (* put the args where the WindowProc can find them... *)
    LOCK arg_mu DO
      IF (arg_head = NIL)
        THEN arg_head := a;
        ELSE arg_tail.next := a;
      END;
      arg_tail := a;
    END;

    (* and enqueue the message *)
    WHILE WinUser.PostMessage (hwnd, msg, 0, a.uid) = False DO
      Thread.Pause (0.05d0);
    END;
  END PostMsg;

PROCEDURE GetArg (id: INTEGER): Arg =
  (* Since Win32 messages are usually delivered in-order and the
     set of outstanding messages is small, we don't expect the linear
     search to take long. *)
  VAR a, b: Arg := NIL;
  BEGIN
    LOCK arg_mu DO
      a := arg_head;
      WHILE (a.uid # id) DO b := a;  a := a.next; END;
      IF (b = NIL)
        THEN arg_head := a.next;
        ELSE b.next := a.next;
      END;
      IF a = arg_tail THEN arg_tail := b; END;
      a.next := NIL;
    END;
    RETURN a;
  END GetArg;

PROCEDURE RecycleArg (a: Arg) =
  BEGIN
    <*ASSERT a.next = NIL*>
    LOCK arg_mu DO
      a.ch      := NIL;
      a.old_dec := NIL;
      a.new_dec := NIL;
      (** a.result  := NIL; **)
      a.next    := free_args;
      free_args := a;
    END;
  END RecycleArg;

(*-------------------------------------------------- raw seething windows ---*)
(* NOTE: The helper procedures called by WindowProc lock VBT.mu when calling
   various Trestle procedures.  They do not hold locks while calling Win32
   because it knows nothing about Modula-3 locks and it can, on a whim, call
   WindowProc to do something.  The only reason this scheme might work is
   because we have a single Modula-3 thread that's pulling on the Win32
   message queue and calling WindowProc.

   Similarly, we don't bother locking around updates to Child records.
   They are updated by the single Modula-3/WindowProc thread.
*)

VAR
  hInst           : WinDef.HINSTANCE;
  hAccelTable     : WinDef.HANDLE;
  windowclassName := M3toC.CopyTtoS ("Trestle VBT");

VAR
  titlebar_y  := WinUser.GetSystemMetrics (WinUser.SM_CYCAPTION);
  (*** NOPE: on Win95 the FULLSCREEN value depends on whether the
    task bar is at the bottom of the screen or not!  If it is, we'll
    be tricked into thinking the titlebar is about twice as big as it
    should be...   -- WKK 6/24/97
  titlebar_y  := WinUser.GetSystemMetrics (WinUser.SM_CYSCREEN) -
                 WinUser.GetSystemMetrics (WinUser.SM_CYFULLSCREEN) - 1;
  ******)
  nonclient_x := 2 * WinUser.GetSystemMetrics (WinUser.SM_CXFRAME);
  nonclient_y := 2 * WinUser.GetSystemMetrics (WinUser.SM_CYFRAME) +
                     titlebar_y;
  screen_x    := 2 * WinUser.GetSystemMetrics (WinUser.SM_CXFRAME) +
                     WinUser.GetSystemMetrics (WinUser.SM_CXSCREEN);
  screen_y    := 2 * WinUser.GetSystemMetrics (WinUser.SM_CYFRAME) +
                     WinUser.GetSystemMetrics (WinUser.SM_CYSCREEN);

VAR
  trace_msgs := RTParams.IsPresent ("TraceWinMsgs");
  slow_trace := RTParams.IsPresent ("SlowTrace");

<*CALLBACK*>
PROCEDURE WindowProc (hwnd   : WinDef.HWND;
                      message: WinDef.UINT;
                      wParam : WinDef.WPARAM;
                      lParam : WinDef.LPARAM  ): WinDef.LRESULT =
  CONST InterestingEvents = WinUser.QS_ALLINPUT - WinUser.QS_TIMER;
  VAR a: Arg := NIL;  result := 0;  debug_id: INTEGER;
  BEGIN
    <* ASSERT Thread.Self() = trslThread *>
    IF trace_msgs THEN debug_id := PrintMessageType(message, 0); END;

    (* let others know if we've cleared the queue... *)
    trsl.queue_status := WinUser.GetQueueStatus (InterestingEvents);

    CASE message OF

    | WinMsg.CREATE_OFFSCREEN_VBT =>
        a := GetArg (lParam);
        CreateOffscreen (a.ch, a.width, a.height);

    | WinMsg.RESHAPE_VBT =>
        a := GetArg (lParam);
        ReshapeVBT (a.ch, a.width, a.height);

    | WinMsg.SYNC_VBT =>
        a := GetArg (lParam);
        EVAL WinGDI.GdiFlush ();

    | WinMsg.FORGE_VBT =>
        a := GetArg (lParam);
        ForgeVBTEvent (a.ch);

    | WinMsg.ICONIZE_VBT =>
        a := GetArg (lParam);
        IconizeVBT (a.ch);

    | WinMsg.OVERLAP_VBT =>
        a := GetArg (lParam);
        OverlapVBT (a.ch, a.pt,  a.bool);

    | WinMsg.RETITLE_VBT =>
        a := GetArg (lParam);
        RetitleVBT (a.ch, a.old_dec, a.new_dec);

    | WinMsg.DELETE_VBT =>
        a := GetArg (lParam);
        DeleteVBT (a.ch);

    | WinMsg.PAINTBATCH_VBT =>
        a := GetArg (lParam);
        PaintBatchVBT (a.ch, a.trsl, a.vbt, a.batch);

    | WinUser.WM_DESTROY =>
        DestroyVBT (GetChild (hwnd));
        result := WinUser.WM_DESTROY;

    | WinUser.WM_GETMINMAXINFO =>
        GetVBTSize (hwnd, lParam);

    | WinUser.WM_PAINT =>
        PaintVBT (hwnd);

    | WinUser.WM_WINDOWPOSCHANGED =>
        MoveVBT (hwnd);

    | WinUser.WM_ACTIVATE =>
        ActivateVBT (hwnd, wParam);

    | WinUser.WM_SETCURSOR =>
        IF WinDef.LOWORD (lParam) # WinUser.HTCLIENT
          THEN result := WinUser.DefWindowProc (hwnd, message, wParam, lParam);
          ELSE SetVBTCursor (hwnd);
        END;

    | WinUser.WM_QUERYNEWPALETTE =>
        QueryVBTPalette (hwnd);
        result := True;

    | WinUser.WM_KEYDOWN =>
        VBTKeyPress (hwnd, wParam, TRUE);

    | WinUser.WM_KEYUP =>
        VBTKeyPress (hwnd, wParam, FALSE);

    | WinUser.WM_CHAR =>
        VBTCharPress (hwnd, wParam);

    | WinUser.WM_LBUTTONDOWN =>
        ButtonEvent (hwnd, lParam, wParam, Button.Left, Transition.Press);

    | WinUser.WM_LBUTTONUP =>
        ButtonEvent (hwnd, lParam, wParam, Button.Left, Transition.Release);

    | WinUser.WM_MBUTTONDOWN =>
        ButtonEvent (hwnd, lParam, wParam, Button.Middle, Transition.Press);

    | WinUser.WM_MBUTTONUP =>
        ButtonEvent (hwnd, lParam, wParam, Button.Middle, Transition.Release);

    | WinUser.WM_RBUTTONDOWN =>
        ButtonEvent (hwnd, lParam, wParam, Button.Right, Transition.Press);

    | WinUser.WM_RBUTTONUP =>
        ButtonEvent (hwnd, lParam, wParam, Button.Right, Transition.Release);

    | WinUser.WM_MOUSEMOVE =>
        DeliverMousePos (hwnd, lParam, wParam);

    | WinUser.WM_TIMER =>
        TimerTick (hwnd);

    | WinUser.WM_DESTROYCLIPBOARD =>
        LostClipboard (hwnd);

    | WinUser.WM_RENDERALLFORMATS =>
        RealizeClipboard (hwnd);

    | WinUser.WM_RENDERFORMAT =>
        IF wParam # WinUser.CF_TEXT THEN RETURN 1 END;
        RealizeClipboard (hwnd);

    ELSE
        result := WinUser.DefWindowProc (hwnd, message, wParam, lParam);
    END;

    IF (a # NIL) THEN
      (****
      IF (a.result # NIL) THEN
        a.result.done := TRUE;
        Thread.Broadcast (a.result.cond);
      END;
      ****)
      RecycleArg (a);
    END;
    IF trace_msgs THEN EVAL PrintMessageType(message, debug_id); END;
    RETURN result;
  END WindowProc;

VAR (* HACK, HACK, HACK....  (See CreateWindow() and GetVBTSize()) *)
  create_child: Child := NIL;
  create_width: CARDINAL;
  create_height: CARDINAL;

PROCEDURE CreateVBT (ur: Child;  st: WinScreenType.T;
                     READONLY nw: Point.T;  iconic: BOOLEAN) =
  CONST
    DefaultWidth  = 133.0;  (* millimeters *)
    DefaultHeight = 100.0;
  VAR
    v      : VBT.T           := ur.ch;
    dec    : TrestleClass.Decoration;
    width  : CARDINAL;
    height : CARDINAL;
  BEGIN
    LOCK VBT.mu DO
      VBTClass.Rescreen (v, st);

      width  := ROUND(VBT.MMToPixels(v, DefaultWidth, Axis.T.Hor));
      height := ROUND(VBT.MMToPixels(v, DefaultHeight, Axis.T.Ver));
      EVAL SetSizeHints (ur, width, height);

      dec := VBT.GetProp (v, TYPECODE(TrestleClass.Decoration));
    END;

    (* create the window *)
    <*ASSERT create_child = NIL *>
    create_child := ur;
    create_width := width;
    create_height := height;
    IF dec = NIL THEN
      ur.title_string := NIL;
      ur.hwnd := WinUser.CreateWindowEx (WinUser.WS_EX_TOPMOST,
                   windowclassName, NIL, WinUser.WS_POPUP,
                   nw.h, nw.v, width, height, NIL, NIL, hInst, NIL);
    ELSE
      IF iconic
        THEN ur.title_string := M3toC.CopyTtoS (dec.iconTitle);
        ELSE ur.title_string := M3toC.CopyTtoS (dec.windowTitle);
      END;
      INC (width, nonclient_x);
      INC (height, nonclient_y);
      ur.hwnd := WinUser.CreateWindow (windowclassName,
                   ur.title_string, WinUser.WS_OVERLAPPEDWINDOW,
                   nw.h, nw.v, width, height, NIL, NIL, hInst, NIL);

    END;
    create_child := NIL;
    <* ASSERT ur.hwnd # NIL *>

    (* Cache the device context in the "Child" record. Note that we
       can do this only because we declared the device context to be
       private ("CS_OWNDC"). *)
    ur.hdc := WinUser.GetDC (ur.hwnd);
    <* ASSERT ur.hdc # NIL *>

    InstallDefaultPalette (ur);

    ur.decorated := dec # NIL;
    RetitleVBT (ur, NIL, dec);

    EVAL WinUser.ShowWindow (ur.hwnd, WinUser.SW_SHOWDEFAULT);

(***** -- didn't the preceding ShowWindow do this??  - WKK 5/17/96
    (* Update the window (repaint its client area) *)
    status := WinUser.UpdateWindow (ur.hwnd);
    <* ASSERT status # 0 *>
****************************************************************)

  END CreateVBT;

PROCEDURE CreateOffscreen (ur: Child;  width, height: INTEGER) =
  BEGIN
    ur.decorated    := FALSE;
    ur.offScreen    := TRUE;
    ur.hwnd         := NIL;
    ur.title_string := NIL;
    ur.hdc          := CreateMemoryDC (width, height);
    InstallDefaultPalette (ur);
  END CreateOffscreen;

PROCEDURE CreateMemoryDC (width, height: INTEGER): WinDef.HDC =
  VAR
    deskHwnd : WinDef.HWND;
    deskHdc  : WinDef.HDC;
    memHdc   : WinDef.HDC;
    newHbmp  : WinDef.HBITMAP;
    oldHbmp  : WinDef.HBITMAP;
  BEGIN
    deskHwnd := WinUser.GetDesktopWindow ();

    deskHdc := WinUser.GetDC (deskHwnd);
    <* ASSERT deskHdc # NIL *>

    memHdc := WinGDI.CreateCompatibleDC (deskHdc);
    <* ASSERT memHdc # NIL *>

    newHbmp := WinGDI.CreateCompatibleBitmap (deskHdc, width, height);
    <* ASSERT newHbmp # NIL *>

    oldHbmp := WinGDI.SelectObject (memHdc, newHbmp);
    <* ASSERT oldHbmp # NIL *>

    RETURN memHdc;
  END CreateMemoryDC;

PROCEDURE InstallDefaultPalette (ur: Child) =
  (* Select the default palette into the device context, and realize it. *)
  VAR
    numCols: INTEGER;
    oldPal : WinDef.HPALETTE;
  BEGIN
    ur.hpal := WinScrnColorMap.DefaultPalette ();

    oldPal := WinGDI.SelectPalette (ur.hdc, ur.hpal, False);
    <* ASSERT oldPal # NIL *>

    numCols := WinGDI.RealizePalette (ur.hdc);
    <* ASSERT numCols # WinGDI.GDI_ERROR *>
  END InstallDefaultPalette;

PROCEDURE ReshapeVBT (ur: Child;  width, height: INTEGER) =
  CONST Flags = WinUser.SWP_NOMOVE + WinUser.SWP_NOZORDER + WinUser.SWP_NOACTIVATE;
  BEGIN
    IF ur.hwnd # NIL THEN
      EVAL WinUser.SetWindowPos (ur.hwnd, NIL, 0, 0, width, height, Flags);
    END;
  END ReshapeVBT;

PROCEDURE DeleteVBT (ur: Child) =
  VAR status: WinDef.BOOL;
  BEGIN
    IF (ur = NIL) THEN RETURN END;
    <*ASSERT ur.dead_vbt*>
    LOCK ur.trsl DO
      IF ur.offScreen THEN
        FreeGDIObjects (ur);
        ur.hwnd := NIL;
        DeleteChild (ur);
      ELSE
        (* make sure Windows gets rid of this guy too! *)
        status := WinUser.PostMessage (ur.hwnd, WinUser.WM_CLOSE, 0, 0);
        <* ASSERT status # False *>
      END;
    END;
  END DeleteVBT;

PROCEDURE DestroyVBT (ur: Child) =
  BEGIN
    IF (ur = NIL) THEN RETURN END;

    IF (NOT ur.dead_vbt) THEN
      (* Windows called us, not Trestle *)
      LOCK VBT.mu DO DiscardVBT (ur); END;
    END;

    LOCK ur.trsl DO
      FreeGDIObjects (ur);
      ur.hwnd := NIL;
      DeleteChild (ur);
    END;
  END DestroyVBT;

PROCEDURE FreeGDIObjects (ur: Child) =
  VAR status: WinDef.BOOL;  hbmp: WinDef.HBITMAP;
  BEGIN
    IF (ur.hdc # NIL) THEN
      IF (ur.offScreen) THEN
        hbmp := WinGDI.GetCurrentObject (ur.hdc, WinGDI.OBJ_BITMAP);
        IF (hbmp # NIL) THEN
          status := WinGDI.DeleteObject (hbmp);
          <* ASSERT status # False *>
        END;

        status := WinGDI.DeleteDC (ur.hdc);
        <* ASSERT status # False *>
      END;
      ur.hdc := NIL;
    END;

    IF (ur.hpal # NIL) THEN
      (**** WinScrnColorMap.DefaultPalette returns a single global one...
      status := WinGDI.DeleteObject (ur.hpal);
      <* ASSERT status # False *>
      ******************************************************************)
      ur.hpal := NIL;
    END;
  END FreeGDIObjects;

PROCEDURE ForgeVBTEvent (ur: Child) =
  BEGIN
    LOCK VBT.mu DO
      VBTClass.Misc (ur.ch, VBT.MiscRec {VBT.TrestleInternal,
                                         VBT.NullDetail,
                                         WinBase.GetTickCount (),
                                         VBT.Forgery});
    END;
  END ForgeVBTEvent;

PROCEDURE IconizeVBT (ur: Child) =
  CONST DefaultNW = Point.T {50, 50};
  CONST NewView = ARRAY BOOLEAN OF INTEGER {
                    WinUser.SW_HIDE, WinUser.SW_MINIMIZE };
  BEGIN
    IF ur.ch.st # NIL THEN
      EVAL WinUser.ShowWindow (ur.hwnd, NewView [ur.decorated]);
    ELSE
      CreateVBT (ur, NIL, DefaultNW, iconic := TRUE);
    END;
  END IconizeVBT;

PROCEDURE OverlapVBT (ur       : Child;
             READONLY nw       : Point.T;
                      knownPos : BOOLEAN) =
  CONST NOSIZE = WinUser.SWP_NOSIZE;  NOMOVE = WinUser.SWP_NOMOVE;
  CONST Flags = ARRAY BOOLEAN OF INTEGER { NOSIZE + NOMOVE, NOSIZE };
  VAR status: WinDef.BOOL;
  BEGIN
    IF ur.ch.st # NIL THEN
      (* The VBT is already mapped onto the screen *)
      status := WinUser.SetWindowPos (ur.hwnd, WinUser.HWND_TOP,
                                      nw.h, nw.v, 0, 0, Flags[knownPos]);
      <* ASSERT status # False *>
    ELSE
      (* The VBT is not yet mapped onto the screen *)
      CreateVBT (ur, trsl.screen, nw, iconic := FALSE);
    END;
  END OverlapVBT;

PROCEDURE RetitleVBT (ur: Child;  old, new: TrestleClass.Decoration) =
  (* The decorations for hwnd have changed from old to new; this procedure
     relays this change to Windows. *)
  BEGIN
    IF new = NIL OR ur.hwnd = NIL THEN RETURN; END;

    IF WinUser.IsIconic (ur.hwnd) = 0 THEN
      (* window is not iconized *)
      IF old = NIL OR NOT Text.Equal (old.windowTitle, new.windowTitle) THEN
        SetWindowText (ur, new.windowTitle);
      END;
    ELSE
      (* window is iconized *)
      IF old = NIL OR NOT Text.Equal (old.iconTitle, new.iconTitle) THEN
        SetWindowText (ur, new.iconTitle);
      END;
    END;
  END RetitleVBT;

PROCEDURE SetWindowText (ur: Child; title: TEXT) =
  VAR status: WinDef.BOOL;
  BEGIN
    IF (ur.title_string # NIL) THEN M3toC.FreeCopiedS (ur.title_string); END;
    ur.title_string := M3toC.CopyTtoS (title);
    status := WinUser.SetWindowText (ur.hwnd, ur.title_string);
    <* ASSERT status # 0 *>
  END SetWindowText;

PROCEDURE GetVBTSize (hwnd   : WinDef.HWND;
                      lParam : WinDef.LPARAM) =
  (* LL = 0 *)
  (* This code is taken almost verbatim from Steve. It determines the
     shape of the VBT corresponding to hwnd, and tells Windows to
     constrain the window accordingly. *)
  VAR
    v    : VBT.T;
    sizes: ARRAY Axis.T OF VBT.SizeRange;
    got_sizes := FALSE;
    info := LOOPHOLE (lParam, WinUser.LPMINMAXINFO);
    (* lParam points to a windows structure. So, assigning to this
       structure changes a Windows data structure. In effect, lParam
       is an OUT parameter. *)
  BEGIN
    (* If "trsl.hwnd" is NIL, then we are right now in the process of
       creating the "null window" that represents the Trestle. In this
       case, we simply return. *)
    IF trsl.hwnd = NIL THEN RETURN; END;

    v:= GetVBT (hwnd);
    IF v = NIL THEN
      (* Windows insists on sending a WM_GETMINMAXINFO message for a freshly
         created window before returning that window's handle.  As a hack,
         we'll capture the handle now.  If we're wrong, the child record
         will get corrected when the CreateWindow() call returns... *)
      IF (create_child # NIL) AND (create_child.hwnd = NIL) THEN
        create_child.hwnd := hwnd;
        v := create_child.ch;
        sizes[Axis.T.Hor].lo := create_width;
        sizes[Axis.T.Hor].hi := create_width;
        sizes[Axis.T.Ver].lo := create_height;
        sizes[Axis.T.Ver].hi := create_height;
        got_sizes := TRUE;
      END;
      IF v = NIL THEN RETURN; END;
    END;

    IF NOT got_sizes THEN
      LOCK VBT.mu DO
        sizes := VBTClass.GetShapes (v);
      END;
    END;

    info.ptMaxSize.x      := MIN (sizes[Axis.T.Hor].hi - 1 + nonclient_x, screen_x);
    info.ptMaxSize.y      := MIN (sizes[Axis.T.Ver].hi - 1 + nonclient_y, screen_y);
    info.ptMinTrackSize.x := sizes[Axis.T.Hor].lo + nonclient_x;
    info.ptMinTrackSize.y := sizes[Axis.T.Ver].lo + nonclient_y;
    info.ptMaxTrackSize.x := info.ptMaxSize.x;
    info.ptMaxTrackSize.y := info.ptMaxSize.y;
  END GetVBTSize;

PROCEDURE PaintBatchVBT (ur: Child;  self: T;  v: VBT.T;  ba: Batch.T) =
  VAR
    hdc    := ur.hdc;
    status : WinDef.BOOL;
  BEGIN
    IF (hdc = NIL) THEN
      (* the window hasn't been created yet... *)
      Batch.Free (ba);
      RETURN;
    END;

    LOCK VBT.mu DO
      LOCK v DO
        ur.badR := WinPaint.PaintBatch (self, v, ba, hdc, ur.badR);
        IF NOT Region.IsEmpty (ur.badR) THEN
          (* tell trestle to repaint the new bad region *)
          VBTClass.ForceRepaint (v, ur.badR, TRUE);
          ur.badR := Region.Empty;
          (* Did we reset the bad region too soon?  What if Capture() is
             called before the bad region is really painted?? -- WKK 11/11/97 *)
        END;
      END;
    END;

    status := WinGDI.GdiFlush();
    <* ASSERT status # False *>
  END PaintBatchVBT;

PROCEDURE PaintVBT (hwnd: WinDef.HWND) =
  (* Repaint the damaged portion of the window *)
  VAR
    ur     := GetChild (hwnd);
    v      := ur.ch;
    info   : WinUser.PAINTSTRUCT;
    hdc    : WinDef.HDC;
    bad    : WinDef.RECT;
    rect   : Rect.T;
    rgn    : Region.T;
    status : WinDef.BOOL;
(** rc: WinDef.RECT; **)
  BEGIN
(******
DEBUG ("Paint: hwnd = " & Fmt.Unsigned(LOOPHOLE(hwnd, INTEGER)));
DEBUG ("  hdc = " & Fmt.Unsigned(LOOPHOLE(ur.hdc, INTEGER)));
EVAL WinUser.GetClientRect (hwnd, ADR(rc));
DEBUG ("  dom = "); PrintRect (ToRect (rc));  DEBUG ("\n");
******)

    (* find the bits that Windows wants us to repaint. *)
    status := WinUser.GetUpdateRect (hwnd, ADR(bad), (*bErase :=*) False);
    <*ASSERT status # False*>

    (* Invalidate the entire Trestle window so that Windows will
       let us repaint some of the non-bad bits, if we want to... *)
    status := WinUser.InvalidateRect (hwnd, NIL, (*bErase :=*) False);
    <*ASSERT status # False *>

    hdc := WinUser.BeginPaint (hwnd, ADR (info));
    IF (hdc # info.hdc) THEN DEBUG ("WM_PAINT: BeginPaint HDC # info HDC\n"); END;

    IF (hdc # NIL) AND (ur # NIL) AND (v # NIL) THEN
      (* there's still a vbt to paint... *)
      IF (hdc # ur.hdc) THEN DEBUG ("WM_PAINT: BeginPaint HDC # ur.hdc\n"); END;
      rect := ToRect (bad); (** info.rcPaint == full Trestle window **)
(** DEBUG ("paint "); PrintRect (rect); DEBUG ("\n");***)
      IF NOT Rect.IsEmpty (rect) THEN
        rgn := Region.FromRect (rect);
        LOCK VBT.mu DO VBTClass.Repaint (v, rgn); END;
      END;
    END;

    EVAL WinUser.EndPaint (hwnd, ADR (info));
  END PaintVBT;

(*******************
PROCEDURE PaintVBT (hwnd: WinDef.HWND) =
  (* Repaint the damaged portion of the window *)
  VAR
    r     : WinDef.RECT;
    status: WinDef.BOOL;
    v     := GetVBT (hwnd);
    rgn   : Region.T;
  BEGIN
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL) THEN RETURN; END;

    IF WinUser.GetUpdateRect (hwnd, ADR(r), False) # False THEN
      status := WinUser.ValidateRect (hwnd, ADR(r));
      <* ASSERT status # False *>
      rgn := Region.FromRect (ToRect (r));
      LOCK VBT.mu DO  VBTClass.Repaint (v, rgn);  END;
    END;
  END PaintVBT;
***************)

PROCEDURE MoveVBT (hwnd: WinDef.HWND) =
  VAR
    v     := GetVBT (hwnd);
    rc    : WinDef.RECT;
    new   : Rect.T;
    status: WinDef.BOOL;
  BEGIN
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL) THEN RETURN; END;

    status := WinUser.GetClientRect (hwnd, ADR(rc));
    <* ASSERT status # False *>
    new := ToRect (rc);
    LOCK VBT.mu DO
      IF v.domain # new THEN
        VBTClass.Reshape (v, new, Rect.Meet(v.domain, new));
      ELSE
        VBTClass.Misc (v, VBT.MiscRec{VBT.Moved, VBT.NullDetail, 0, VBT.NilSel});
      END;
    END;
  END MoveVBT;

PROCEDURE ActivateVBT (hwnd: WinDef.HWND;  wParam: WinDef.WPARAM) =
  (* This is derived from "XMessenger.EnterLeave".  The original
     procedure does a lot more ... *)
  VAR
    v    := GetVBT (hwnd);
    time := WinUser.GetMessageTime () + 1;
  BEGIN
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL) THEN RETURN; END;

    EVAL WinUser.SetFocus (hwnd);

    LOCK VBT.mu DO
      IF WinDef.LOWORD (wParam) = WinUser.WA_INACTIVE THEN
        VBTClass.Misc(v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0, VBT.KBFocus})
      ELSE
        VBTClass.Misc(v, VBT.MiscRec{VBT.TakeSelection, VBT.NullDetail,
                                     time, VBT.KBFocus});
      END;
    END;
  END ActivateVBT;

PROCEDURE SetVBTCursor (hwnd: WinDef.HWND) =
  VAR v := GetVBT (hwnd);  cs: ScrnCursor.T;
  BEGIN
    IF (v # NIL) THEN
      LOCK VBT.mu DO
        LOCK v DO  cs := v.getcursor();  END;
      END;
      WinScrnCursor.SetCursor (cs);
    END;
  END SetVBTCursor;

PROCEDURE QueryVBTPalette (hwnd: WinDef.HWND) =
  VAR
    v       := GetVBT (hwnd);
    ur      : Child;
    numCols : INTEGER;
    status  : WinDef.BOOL;
    oldPal  : WinDef.HPALETTE;
  BEGIN
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL) THEN RETURN; END;

    ur := v.upRef;
    IF (ur # NIL) AND (ur.hpal # NIL) THEN
      oldPal := WinGDI.SelectPalette (ur.hdc, ur.hpal, False);
      <* ASSERT oldPal # NIL *>

      status := WinGDI.UnrealizeObject (ur.hpal);
      <* ASSERT status # False *>

      numCols := WinGDI.RealizePalette (ur.hdc);
      <* ASSERT numCols # WinGDI.GDI_ERROR *>
    END;
  END QueryVBTPalette;

PROCEDURE VBTKeyPress (hwnd: WinDef.HWND;  wParam: WinDef.WPARAM;  down: BOOLEAN) =
  (* need to update the per-Trestle modifier set and translate the Windows
     virtual key into a Trestle KeySym. *)
  VAR
    v         := GetVBT (hwnd);
    keysym    := WinKey.Translate (wParam);
    time      := WinUser.GetMessageTime();
    modifiers := GetModifiers ();
  BEGIN
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL) THEN RETURN; END;

    LOCK VBT.mu DO
      VBTClass.Key (v, VBT.KeyRec {keysym, time, down, modifiers});
    END;
  END VBTKeyPress;

PROCEDURE VBTCharPress (hwnd: WinDef.HWND;  wParam: WinDef.WPARAM) =
  (* need to update the per-Trestle modifier set and translate the Windows
     virtual key into a Trestle KeySym. *)
  VAR
    v         := GetVBT (hwnd);
    keysym    := wParam;
    time      := WinUser.GetMessageTime();
    modifiers := GetModifiers ();
  BEGIN
    (* ------ uncomment to debug character input ---------
    IF keysym < 256 THEN
      DEBUG("WM_CHAR: code = " & Fmt.Int(keysym) & " char = " &
            Text.FromChar(VAL(keysym, CHAR)) &
            ModifiersToText(modifiers) & "\n");
    ELSE
      DEBUG("WM_CHAR: code = " & Fmt.Int(keysym) &
            ModifiersToText(modifiers) & "\n");
    END;
       ------ *)
    (*** If the VBT is already deleted, bail out ***)
    IF (v = NIL)THEN RETURN; END;

    modifiers := modifiers - VBT.Modifiers{VBT.Modifier.Control};
    (* simulate the key up and down events for Trestle *)
    IF useEvent_WM_CHAR THEN
      LOCK VBT.mu DO
        VBTClass.Key (v, VBT.KeyRec {keysym, time, TRUE, modifiers});
        VBTClass.Key (v, VBT.KeyRec {keysym, time, FALSE, modifiers});
      END;
    END;
  END VBTCharPress;

PROCEDURE ModifiersToText(m : VBT.Modifiers) : TEXT = <*NOWARN*>
  VAR res := "";
  BEGIN
    IF VBT.Modifier.Shift IN m THEN
      res := res & " shift";
    END;
    IF VBT.Modifier.Lock IN m THEN
      res := res & " lock";
    END;
    IF VBT.Modifier.Control IN m THEN
      res := res & " control";
    END;
    IF VBT.Modifier.Option IN m THEN
      res := res & " option";
    END;
    IF VBT.Modifier.Mod0 IN m THEN
      res := res & " mod0";
    END;
    IF VBT.Modifier.Mod1 IN m THEN
      res := res & " mod1";
    END;
    IF VBT.Modifier.Mod2 IN m THEN
      res := res & " mod2";
    END;
    IF VBT.Modifier.Mod3 IN m THEN
      res := res & " mod3";
    END;
    RETURN res;
  END ModifiersToText;

TYPE
  Button = {None, Left, Middle, Right};
  Transition = {Press, Release};
  Last = RECORD
           x, y                : INTEGER     := 0;
           time                : WinDef.LONG := 0;
           button              : Button      := Button.None;
           clickCount          : CARDINAL    := 0;
           safetyRadius        : CARDINAL    := 3;
           doubleClickInterval : CARDINAL    := 500;
         END;
(* last.{x,y} = position of last mouseclick; last.time = time of last mouseClick;
   last.clickCount = clickcount of last mouseclick, as defined in the VBT
   interface; last.button = button that last went up or down. *)

PROCEDURE ButtonEvent (hwnd  : WinDef.HWND;
                       lParam: WinDef.LPARAM;
                       wParam: WinDef.WPARAM;
                       button: Button;
                       trans : Transition) =
  VAR
    oldFocus  := trsl.mouseFocus;
    time      := WinUser.GetMessageTime ();
    clientPos := WinDef.POINT {WinDef.LOWORD (lParam), WinDef.HIWORD (lParam)};
    screenPos := clientPos;
    focusPos  := clientPos;
    status    : WinDef.BOOL;
    v         : VBT.T;
    ur        : Child;
    cd        : VBT.MouseRec;
  CONST
    NonButtons = VBT.Modifiers{FIRST(VBT.Modifier).. LAST(VBT.Modifier)}
                   - VBT.Buttons;
  BEGIN
    status := WinUser.ClientToScreen (hwnd, ADR (screenPos));
    <* ASSERT status # False *>

    (* If "hwnd" refers to the window that has captured the mouse (as opposed
       to the topmost window beneath the mouse cursor), we determine what
       window (if any) is below the cursor. If there is one, we set "hwnd"
       to be the window handle of this window, and translate "clientPos" to
       be in the coordinate space of this window. *)
    IF trsl.mouseFocus # NIL THEN
      WITH topHwnd = WinUser.WindowFromPointWorkaround (screenPos) DO
        IF topHwnd # NIL THEN
          hwnd := topHwnd;
          clientPos := screenPos;
          status := WinUser.ScreenToClient (hwnd, ADR (clientPos));
          <* ASSERT status # False *>
        END;
      END;
    END;
    v := GetVBT (hwnd);

    (* Determine "cd.button", "cd.modifiers", and "cd.clickType". *)
    cd.modifiers := ExtractModifiers (wParam);
    CASE button OF
    | Button.None   => <* ASSERT FALSE *>
    | Button.Left   => cd.whatChanged := VBT.Modifier.MouseL;
    | Button.Middle => cd.whatChanged := VBT.Modifier.MouseM;
    | Button.Right  => cd.whatChanged := VBT.Modifier.MouseR;
    END;
    IF trans = Transition.Press THEN
      cd.modifiers := cd.modifiers - VBT.Modifiers{cd.whatChanged};
      IF cd.modifiers <= NonButtons THEN
        cd.clickType := VBT.ClickType.FirstDown;
        trsl.mouseFocus := v;
        IF v # NIL THEN
          EVAL WinUser.SetCapture (hwnd);
        END;
      ELSE
        cd.clickType := VBT.ClickType.OtherDown
      END
    ELSE
      IF cd.modifiers <= NonButtons THEN
        cd.clickType := VBT.ClickType.LastUp;
        trsl.mouseFocus := NIL;
        status := WinUser.ReleaseCapture ();
        <* ASSERT status # False *>
      ELSE
        cd.clickType := VBT.ClickType.OtherUp
      END;
      cd.modifiers := cd.modifiers + VBT.Modifiers{cd.whatChanged};
    END;

    cd.time := time;

    IF v # NIL THEN
      ur := v.upRef;
      IF Word.Minus(time, ur.last.time) <= ur.last.doubleClickInterval
        AND ABS(ur.last.x - clientPos.x) <= ur.last.safetyRadius
        AND ABS(ur.last.y - clientPos.y) <= ur.last.safetyRadius
        AND ur.last.button = button THEN
        INC(ur.last.clickCount)
      ELSE
        ur.last.clickCount := 0;
        ur.last.x          := clientPos.x;
        ur.last.y          := clientPos.y;
        ur.last.button     := button
      END;
      ur.last.time := time;

      SetCursorPosition (clientPos.x, clientPos.y, hwnd, cd.cp);
      cd.clickCount := ur.last.clickCount;

      LOCK VBT.mu DO
        ur.cageCovered := TRUE;
        VBTClass.Position (v, VBT.PositionRec{cd.cp, cd.time, cd.modifiers});
        VBTClass.Mouse(v, cd);
        ur.cageCovered := FALSE;
      END;

      LOCK v DO
        trsl.setcage (v);
      END;
    END;

    IF oldFocus # NIL AND oldFocus # v THEN
      cd.cp.offScreen := FALSE;
      cd.cp.pt.h      := focusPos.x;
      cd.cp.pt.v      := focusPos.y;
      cd.cp.gone      := TRUE;
      LOCK VBT.mu DO
        VBTClass.Mouse(oldFocus, cd);
      END;
    END;
(****
    Enter(trsl);
    TRY
      FOR s := FIRST(trsl.sel^) TO LAST(trsl.sel^) DO
        WITH sr = trsl.sel[s] DO
          IF s = VBT.KBFocus.sel THEN
            IF sr.v = v AND ur.isXFocus THEN
              X.XSetInputFocus(trsl.dpy, ur.w, X.RevertToParent, time);
              sr.ts := time
            END
          ELSIF sr.v = v THEN
            X.XSetSelectionOwner(trsl.dpy, sr.name, ur.w, time);
            sr.ts := time
          END
        END
      END
    FINALLY
      Exit(trsl)
    END
****)
  END ButtonEvent;


(* "ExtractModifiers" takes a "WinDef.WPARAM" that was typically delivered by
   a Windows Mouse Input Message (e.g. WM_MOUSEMOVE or WM_LBUTTONDOWN), and
   converts it into a Trestle "VBT.Modifiers", that is, into a set of modifier
   keys and buttons.

   Note: I handle only 5 out of 12 modifiers. In particular, I don't handle
   "Option" and "Shift Lock". *)

PROCEDURE ExtractModifiers (wParam: WinDef.WPARAM): VBT.Modifiers =
  VAR mods := VBT.Modifiers {};
  BEGIN
    IF Word.And (wParam, WinUser.MK_SHIFT) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Shift};
    END;
    IF Word.And (wParam, WinUser.MK_CONTROL) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Control};
    END;
    IF Word.And (wParam, WinUser.MK_LBUTTON) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseL};
    END;
    IF Word.And (wParam, WinUser.MK_MBUTTON) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseM};
    END;
    IF Word.And (wParam, WinUser.MK_RBUTTON) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseR};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_LMENU),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Option};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_CAPITAL),16_0001) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Lock};
    END;
    RETURN mods;
  END ExtractModifiers;

PROCEDURE GetModifiers (): VBT.Modifiers =
  VAR mods := VBT.Modifiers {};
  BEGIN
    IF Word.And (WinUser.GetKeyState (WinUser.VK_CONTROL),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Control};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_SHIFT),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Shift};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_LMENU),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Option};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_CAPITAL),16_0001) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.Lock};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_LBUTTON),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseL};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_MBUTTON),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseM};
    END;
    IF Word.And (WinUser.GetKeyState (WinUser.VK_RBUTTON),16_8000) # 0 THEN
      mods := mods + VBT.Modifiers {VBT.Modifier.MouseR};
    END;
    RETURN mods;
  END GetModifiers;

PROCEDURE SetCursorPosition (x, y: INTEGER;  hwnd: WinDef.HWND;
                            VAR(*OUT*) cp: VBT.CursorPosition) =
  VAR
    r      : WinDef.RECT;
    status := WinUser.GetClientRect (hwnd, ADR (r));
  BEGIN
    <* ASSERT status # False *>
    cp.pt.h      := x;
    cp.pt.v      := y;
    cp.screen    := DesktopID;
    cp.offScreen := FALSE;
    cp.gone      := x < r.left OR x >= r.right OR y < r.top  OR y >= r.bottom;
  END SetCursorPosition;

PROCEDURE TimerTick (hwnd: WinDef.HWND) =
   VAR
    status   : WinDef.BOOL;
    screenPos: WinDef.POINT;
    shortPos : WinDef.POINTS; (* s is for 16bit short *)
    lParam   : WinDef.LPARAM;
  BEGIN
    IF trsl.mouseFocus = NIL THEN
      status := WinUser.GetCursorPos (ADR (screenPos));
      <* ASSERT status # False *>
      shortPos.x := screenPos.x; (* truncate 32bit to 16bit *)
      shortPos.y := screenPos.y;
      lParam := WinGDI.PointsToLParam(ADR (shortPos));
      DeliverMousePos (hwnd, lParam, 0);
    END;
  END TimerTick;

PROCEDURE LostClipboard (hwnd: WinDef.HWND) =
  VAR
    v  := GetVBT (hwnd);
    ts := WinBase.GetTickCount ();
  BEGIN
    LOCK VBT.mu DO
      VBTClass.Misc (v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, ts, VBT.Source});
    END;
  END LostClipboard;

PROCEDURE RealizeClipboard (hwnd: WinDef.HWND) =
  VAR
    v     := GetVBT (hwnd);
    tc    := TYPECODE (TEXT);
    ts    := WinBase.GetTickCount ();
    hglb  : WinDef.HGLOBAL;
    lptstr: Ctypes.char_star;
    txtstr: Ctypes.char_star;
    txt   : TEXT;
    len   : INTEGER;
  BEGIN
    LOCK VBT.mu DO
      TRY
        txt := NARROW (v.read (VBT.Source, tc).toRef(), TEXT);
      EXCEPT VBT.Error =>
        RETURN; (* things went badly ... ignore *)
      END;

      (* According to the Win32 documentation, the WM_RENDERALLFORMATS
         and WM_RENDERFORMAT messages are only sent to the window that
         already owns the clipboard and therefore it's an error to
         reopen it.  --- So, I guess we won't... *)

      len := Text.Length (txt) + 1;
      hglb := WinBase.GlobalAlloc (WinBase.GMEM_MOVEABLE+WinBase.GMEM_DDESHARE,
                                   len);
      <* ASSERT hglb # NIL *>

      lptstr := WinBase.GlobalLock (hglb);
      <* ASSERT lptstr # NIL *>

      txtstr := M3toC.SharedTtoS (txt);
      EVAL Cstring.memcpy (lptstr, txtstr, len);
      M3toC.FreeSharedS (txt, txtstr);

      EVAL WinBase.GlobalUnlock(hglb);

      EVAL WinUser.SetClipboardData (WinUser.CF_TEXT, hglb);
      (* we don't care if it worked or not, we gave it our best shot... *)

      VBTClass.Misc (v, VBT.MiscRec {VBT.Lost, VBT.NullDetail, ts, VBT.Source});
    END;
  END RealizeClipboard;

PROCEDURE DeliverMousePos (hwnd  : WinDef.HWND;
                           lParam: WinDef.LPARAM;
                           wParam: WinDef.WPARAM) =
  (* LL = VBT.mu *)
  VAR
    screenPos := WinDef.POINT {WinDef.LOWORD (lParam), WinDef.HIWORD (lParam)};
    clientPos : WinDef.POINT;
    status    : WinDef.BOOL;
    copy      : RootList;
    ur        : Child;
  BEGIN
    IF hwnd # trsl.hwnd THEN
      status := WinUser.ClientToScreen (hwnd, ADR (screenPos));
      <* ASSERT status # False *>
    END;

    LOCK trsl DO
      IF (trsl.lastPos = screenPos) AND (trsl.n_cages <= 0) THEN
        (* the mouse didn't move and nobody cares... *)
        RETURN;
      ELSE
        (* reset the existing cages and deliver the new position *)
        trsl.n_cages := 0;
        trsl.lastPos := screenPos;
      END;
    END;

    (* grab a copy of the existing roots *)
    copy := CopyRoots ();
    FOR i := FIRST (copy^) TO LAST (copy^) DO
      ur := copy[i];
      IF (ur # NIL) (* AND (ur.cageSet)*) THEN
        ur.cageSet := FALSE;
        IF (ur.hwnd # NIL) AND (ur.ch # NIL) THEN
          clientPos := screenPos;
          status := WinUser.ScreenToClient (ur.hwnd, ADR (clientPos));
          <* ASSERT status # False *>
          MouseMotion (ur.hwnd, ur.ch, clientPos, wParam);
        END;
      END;
    END;
    RecycleCopy (copy);
  END DeliverMousePos;

(* Note: This procedure may not be called with trsl being held, since the call
   to "VBTClass.Position" might lead to a call back into "WinTrestle" and an
   attempt to acquire "trsl". *)

PROCEDURE MouseMotion (hwnd     : WinDef.HWND;
                       v        : VBT.T;
                       clientPos: WinDef.POINT;
                       wParam   : WinDef.WPARAM ) =
  (* LL = 0 *)
  VAR cd: VBT.PositionRec;
  BEGIN
    cd.time      := WinUser.GetMessageTime ();
    cd.modifiers := ExtractModifiers (wParam);
    SetCursorPosition (clientPos.x, clientPos.y, hwnd, cd.cp);

    IF cd.cp.gone AND v = trsl.current THEN
      trsl.current := NIL;
    ELSIF NOT cd.cp.gone AND v # NIL THEN
      trsl.current := v
    END;

    LOCK VBT.mu DO
      VBTClass.Position (v, cd);
    END;

(****
    IF ur # NIL AND lost THEN
      LOCK trsl DO
        XProperties.ExtendOwns(ur.owns, VBT.KBFocus);
        ur.owns[VBT.KBFocus.sel] := FALSE;
        IF trsl.sel[VBT.KBFocus.sel].v = v THEN
          trsl.sel[VBT.KBFocus.sel].v := NIL
        END
      END;
      VBTClass.Misc(
        v, VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0, VBT.KBFocus})
    ELSIF takeFocus THEN
      LOCK trsl DO ur.recentlyOutside := FALSE END;
      VBTClass.Misc(v, VBT.MiscRec{VBT.TakeSelection, VBT.NullDetail,
                                   time, VBT.KBFocus})
    END
****)
  END MouseMotion;

PROCEDURE ToRect (READONLY r: WinDef.RECT): Rect.T =
  BEGIN
    RETURN Rect.T{west  := r.left,
                  east  := r.right,
                  north := r.top,
                  south := r.bottom}
  END ToRect;

(*------------------------------ VBT <-> Child <-> Window Handle mappings ---*)

TYPE
  RootList = REF ARRAY OF Child;

VAR
  root_mu    := NEW (MUTEX);
  n_roots    := 0;
  roots      := NEW (RootList, 10);
  spare_copy : RootList := NIL;
  (* "roots" provides the mapping from child uids to children.  It is
     also searched to convert a window handle to a VBT.  The following
     relationships hold:  "roots[ur.uid] = ur = ur.ch.upRef".  LL = root_mu *)

PROCEDURE NewChild (trsl: T): Child =
  (* LL < root_mu *)
  VAR ch := NEW (Child);  i := 0;
  BEGIN
    LOCK root_mu DO
      IF (n_roots >= NUMBER (roots^)) THEN ExpandRoots (); END;
      WHILE (roots[i] # NIL) DO INC (i); END;
      ch.uid := i;
      ch.trsl := trsl;
      roots[i] := ch;
      INC (n_roots);
    END;
    RETURN ch;
  END NewChild;

PROCEDURE ExpandRoots () =
  (* LL = root_mu *)
  VAR n := NUMBER (roots^);  new := NEW (RootList, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := roots^;
    roots := new;
  END ExpandRoots;

PROCEDURE DeleteChild (ch: Child) =
  (* LL < root_mu *)
  BEGIN
    LOCK root_mu DO
      roots [ch.uid] := NIL;
      ch.uid := -1;
      ch.trsl := NIL;
    END;
  END DeleteChild;

PROCEDURE GetChild (hwnd: WinDef.HWND): Child =
  (* LL < root_mu *)
  VAR ch: Child;
  BEGIN
    LOCK root_mu DO
      FOR i := 0 TO LAST (roots^) DO
        ch := roots[i];
        IF (ch # NIL) AND (ch.hwnd = hwnd) THEN RETURN ch; END;
      END;
    END;
    RETURN NIL;
  END GetChild;

PROCEDURE GetVBT (hwnd: WinDef.HWND): VBT.T =
  VAR ch := GetChild (hwnd);
  BEGIN
    IF (ch # NIL) AND (ch.ch # NIL) THEN
      RETURN ch.ch;
    END;

    (*****
    DEBUG ("Could not map window handle " &
           Fmt.Unsigned (LOOPHOLE (hwnd, INTEGER)));
    IF (ch = NIL)
      THEN DEBUG (" to a WinTrestle.Child ...\n");
      ELSE DEBUG (" to a VBT.T ...\n");
    END;
    ******)

    RETURN NIL;
  END GetVBT;

PROCEDURE CopyRoots (): RootList =
  VAR copy: RootList;  n := 0;  ch: Child;
  BEGIN
    LOCK root_mu DO
      copy := spare_copy;  spare_copy := NIL;
      IF (copy = NIL) OR (NUMBER (copy^) < n_roots) THEN
        copy := NEW (RootList, NUMBER (roots^))
      END;
      FOR i := FIRST (roots^) TO LAST (roots^) DO
        ch := roots[i];
        IF (ch # NIL) THEN copy[n] := ch; INC (n); END;
      END;
    END;
    FOR i := n TO LAST (copy^) DO copy[i] := NIL; END;
    RETURN copy;
  END CopyRoots;

PROCEDURE RecycleCopy (copy: RootList) =
  BEGIN
    IF (copy = NIL) THEN RETURN; END;
    LOCK root_mu DO
      IF (spare_copy = NIL) OR (NUMBER (copy^) > NUMBER (spare_copy^)) THEN
        spare_copy := copy;
      END;
    END;
  END RecycleCopy;

(*****************************************************************************)
(* Garbage-Collection Cursor                                                 *)
(*****************************************************************************)

VAR
  showGC := RTParams.IsPresent("StarTrek");
(* If showGC is TRUE, the cursor of every installed window will change to the
   Star Trek cursor whenever the garbage collector is running.  At runtime,
   you can force the StarTrek cursor by running your program @M3StarTrek. *)

TYPE
  GCClosure = RTHeapRep.MonitorClosure OBJECT
    trsl: T;
  OVERRIDES
    before := HackOn;
    after  := HackOff
  END;

PROCEDURE DoHackInit (trsl: T) =
  BEGIN
    IF showGC THEN
      RTHeapRep.RegisterMonitor(NEW(GCClosure, trsl := trsl))
    END;
  END DoHackInit;


PROCEDURE HackOn (cl: GCClosure) =
  BEGIN
    HackToggle(cl.trsl, TRUE);
    hacking := TRUE
  END HackOn;

PROCEDURE HackOff (cl: GCClosure) =
  BEGIN
    IF hacking THEN
      HackToggle(cl.trsl, FALSE);
      hacking := FALSE
    END
  END HackOff;


VAR
  hacking   := FALSE;
  (** oldCursor : WinDef.HCURSOR; **)
  (** gcCursor  : WinDef.HCURSOR; **)

PROCEDURE HackToggle (<*UNUSED*> trsl: T;   <*UNUSED*> on: BOOLEAN) =
  BEGIN
  END HackToggle;

(*******
PROCEDURE HackToggle (trsl: T; on: BOOLEAN) =
  <*FATAL Split.NotAChild*>
  BEGIN
    IF on THEN
      oldCursor := WinUser.SetCursor (gcCursor);
      DEBUG ("Starting GC ................................\n");
    ELSE
      EVAL WinUser.SetCursor (oldCursor);
      DEBUG ("................................ Finished GC\n");
    END;
    IF NOT trsl.dead THEN
      VAR v := Split.Succ(trsl, NIL);
      BEGIN
        WHILE v # NIL DO
          VAR ur: Child := v.upRef;
          BEGIN
            IF ur # NIL AND ur.hwnd # NIL AND ur.xcage # X.None THEN
              IF on THEN
                EVAL Win.SetCursor (ur.X.XDefineCursor(dpy, ur.w, trsl.gcCursor)
              ELSE
                X.XDefineCursor(dpy, ur.w, ur.csid)
              END
            END
          END;
          v := Split.Succ(trsl, v)
        END
      END;
    END
  END HackToggle;
*****)


(*****************************************************************************)
(* Window-creation and message-handling thread                               *)
(*****************************************************************************)

VAR
  messenger_started := FALSE;
  cond := NEW (Thread.Condition);
        (* used to signal the main thread that "trsl.hwnd" has been created. *)

PROCEDURE CreateTrestle () =
  VAR mu := NEW (MUTEX);
  BEGIN
    trsl := NEW(T);
    DoHackInit(trsl);

    trsl.st := NEW(VBT.ScreenType);
    (* The st is irrelevant except that it must be non-NIL so that
       marking the trsl for redisplay is not a noop. *)

    trsl.screen := WinScreenType.New(trsl);

    EVAL Thread.Fork (NEW (Thread.Closure, apply := MessengerApply));

    (* wait for the messenger thread to finish its setup *)
    LOCK mu DO
      WHILE NOT messenger_started DO Thread.Wait (mu, cond); END;
    END;
  END CreateTrestle;

PROCEDURE MessengerApply (<*UNUSED*> cl: Thread.Closure): REFANY =
  VAR
    class := M3toC.CopyTtoS("Trestle Desktop");
    msg   : WinUser.MSG;
  BEGIN
    trslThread := Thread.Self ();

    (* First, we have to register a window class for the "null window". *)
    RegisterWindowClass (class, topLevel := TRUE);

    (* Now, we can actually create the "null window" *)
    trsl.hwnd := WinUser.CreateWindow(
                    class, NIL, WinUser.WS_DISABLED,
                    WinUser.CW_USEDEFAULT, WinUser.CW_USEDEFAULT,
                    WinUser.CW_USEDEFAULT, WinUser.CW_USEDEFAULT,
                    NIL, NIL, hInst, NIL);
    <* ASSERT trsl.hwnd # NIL *>

    (* Register a class for the rest of the Trestle windows *)
    RegisterWindowClass (windowclassName, topLevel := FALSE);

    (* Signal "CreateTrestle" that the null window is created. *)
    messenger_started := TRUE;
    Thread.Signal (cond);

    (* Start a Windows Timer with 0.1 sec clicks *)
    trsl.timerId := WinUser.SetTimer (trsl.hwnd, 1, 100, NIL);

    (* start the message loop for all windows belonging to this Trestle *)
    WHILE WinUser.GetMessage (ADR(msg), NIL, 0, 0) = True DO
      EVAL WinUser.TranslateMessage (ADR(msg));
      EVAL WinUser.DispatchMessage (ADR(msg));
    END;

    (* received WM_QUIT message -- exiting *)
    RETURN NIL;
  END MessengerApply;

PROCEDURE RegisterWindowClass (name: Ctypes.char_star;  topLevel: BOOLEAN) =
  VAR
    wc    : WinUser.WNDCLASS;
    status: WinDef.BOOL;
  BEGIN
    hInst := RTLinker.instance;

    wc.style         := WinUser.CS_HREDRAW + WinUser.CS_VREDRAW;
    wc.lpfnWndProc   := WindowProc;
    wc.cbClsExtra    := 0;
    wc.cbWndExtra    := 0;
    wc.hInstance     := hInst;
    wc.hIcon         := WinUser.LoadIcon (NIL, WinUser.IDI_APPLICATION);
    wc.hCursor       := NIL;
    wc.hbrBackground := NIL;
    wc.lpszMenuName  := NIL;
    wc.lpszClassName := name;

    IF topLevel THEN
      wc.hCursor := WinUser.LoadCursor (NIL, WinUser.IDC_ARROW);
    ELSE
      hAccelTable := WinUser.LoadAccelerators(hInst, windowclassName);
      INC (wc.style, WinUser.CS_OWNDC);
      (* other styles to consider: CS_GLOBALCLASS, CS_PARENTDC, CS_SAVEBITS *)
      (** gcCursor := WinUser.LoadCursor (NIL, WinUser.IDC_APPSTARTING); **)
    END;

    status := WinUser.RegisterClass (ADR(wc));
    <* ASSERT status # 0 *>
  END RegisterWindowClass;

(*------------------------------------------------------------- Debugging ---*)

PROCEDURE DEBUG (msg: TEXT) =
  BEGIN
    RTIO.PutText (msg);
    RTIO.Flush ();
  END DEBUG;

(************
PROCEDURE PrintChild (ur: Child) =
  BEGIN
    DEBUG (Fmt.Unsigned (LOOPHOLE (ur, INTEGER))
           & "{ ch: " & Fmt.Unsigned (LOOPHOLE (ur.ch, INTEGER))
           & "  hwnd: " & Fmt.Unsigned (LOOPHOLE (ur.hwnd, INTEGER))
           & "  hdc: " & Fmt.Unsigned (LOOPHOLE (ur.hdc, INTEGER))
           & "  offS: " & Fmt.Bool (ur.offScreen)
           & " }");
  END PrintChild;
************)

(*******
PROCEDURE PrintRect (READONLY r: Rect.T) =
  BEGIN
    DEBUG ("[" & Fmt.Int (r.west) & ".." & Fmt.Int (r.east)
         & " x " & Fmt.Int (r.north) & ".." & Fmt.Int (r.south)
         & "]");
  END PrintRect;
*******)

VAR
  msg_indent := 0;
  msg_uid    := 1;

PROCEDURE PrintMessageType (message: WinDef.UINT;  debug_id: INTEGER): INTEGER =
  VAR txt: TEXT;
  BEGIN
    IF (message = WinUser.WM_TIMER) THEN RETURN msg_uid; END;
    IF (debug_id # 0) THEN DEC (msg_indent); END;
    IF (debug_id # msg_uid) THEN
      FOR i := 1 TO msg_indent DO DEBUG (" | "); END;
      DEBUG("msg " & Fmt.Int(message) & " = ");
      txt := WinMsg.ToText (message);
      IF (txt # NIL)
        THEN DEBUG (txt);
        ELSE DEBUG ("???");
      END;
      DEBUG("\n");
      IF slow_trace THEN Thread.Pause (1.0d0); END;
    END;
    IF (debug_id = 0) THEN INC (msg_indent); END;
    INC (msg_uid);
    RETURN msg_uid;
  END PrintMessageType;

(*************
PROCEDURE DumpSystemPalette (hdc : WinDef.HDC) =
  TYPE
    PaletteList = REF ARRAY OF WinGDI.PALETTEENTRY;
  VAR
    num1, num2 : INTEGER;
    entries : PaletteList;
  BEGIN
    (* Determine size of system palette *)
    num1 := WinGDI.GetSystemPaletteEntries (hdc, 0, 256, NIL);
    <* ASSERT num1 # 0 *>

    (* Get the system palette entries *)
    entries := NEW (PaletteList, num1);
    num2 := WinGDI.GetSystemPaletteEntries (hdc, 0, num1, ADR(entries[0]));
    <* ASSERT num2 = num1 *>

    FOR i := 0 TO num2 - 1 DO
      DEBUG ("entry[" & Fmt.Int (i) &"] = {" &
              Fmt.Int (entries[i].peRed) & "," &
              Fmt.Int (entries[i].peGreen) & "," &
              Fmt.Int (entries[i].peBlue) & "," &
              Fmt.Int (entries[i].peFlags) & "}\n");
    END;
  END DumpSystemPalette;
************)

(*-------------------------------------------------------- initialization ---*)

VAR
  useEvent_WM_CHAR := FALSE;
BEGIN
  WITH v = Env.Get("USE_EVENT_WM_CHAR") DO
    IF v # NIL THEN
      useEvent_WM_CHAR := Text.Length(v) = 0 OR
                          Text.GetChar(v, 0) = '1' OR
                          Text.GetChar(v, 0) = 'y' OR
                          Text.GetChar(v, 0) = 'Y';
    END;
  END;
  CreateTrestle ();
END WinTrestle.
