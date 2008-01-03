(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 17:05:36 PST 1994 by kalsow    *)

UNSAFE MODULE SilWindow;

IMPORT Pathname, Rect, Point, RTMisc, Text, TextWr, Wr, Word;
IMPORT M3toC, Fmt, Cstring, Ctypes, RTLinker, Process;
IMPORT WinBase, WinDef, WinGDI, WinUser, CommDlg;
IMPORT SilObject, SilFont, SilArc, SilLine, SilBox, SilString;
IMPORT SilRd, SilWr, SilError, SilMacro, SilHelp;

CONST
  (* Resource IDs *)

  (* Menu command IDs *)
  cm_Open      = 101;
  cm_Save      = 102;
  cm_SaveAs    = 103;
  cm_Print     = 104;
  cm_Exit      = 105;
  cm_Fonts     = 200;
  cm_Zoom      = 300;
  cm_LineWidth = 400;
  cm_Grid      = 500;
  cm_Copy      = 601;
  cm_Group     = 602;
  cm_UnGroup   = 603;
  cm_Boxes     = 604;
  cm_Erase     = 605;
  cm_Help      = 700;

CONST (* positions of menus in the main menu bar *)
(*FileMenuPos     = 0;*)
  FontMenuPos     = 1;
(*SettingsMenuPos = 2;*)
(*EditMenuPos     = 3;*)
  HelpMenuPos     = 4;

(*------------------------------------------------------ top-level window ---*)

REVEAL
  T = Tx BRANDED "SilWindow" OBJECT
    hInstance     : WinDef.HINSTANCE;
    hWindow       : WinDef.HWND;  (**WinDef.HGDIOBJ;**)
    hMenu         : WinDef.HMENU;

    winOrigin     : Point.T;
    winExtent     : Point.T;
    viewOrigin    : Point.T;
    viewExtent    : Point.T;

    LB_down       : BOOLEAN;
    MB_down       : BOOLEAN;
    RB_down       : BOOLEAN;

    thePen        : WinDef.HPEN;
    theFont       : SilFont.T;
    xFont         : WinDef.HFONT;
    fileName      : Pathname.T;

    markStack     : ARRAY[0..3] OF Point.T;
    numMarks      : INTEGER;

    first, last   : SilObject.T; (*display list*)
    oldROp2       : INTEGER;

    p0            : Point.T;  (*temporary mark.  Logical coordinates*)
    penSize       : INTEGER;
    gridMask      : INTEGER;
    gridSize      : INTEGER;

    tl            : ARRAY [0..1] OF Point.T; (*temporary line*)
    xscroll       : INTEGER;
    yscroll       : INTEGER;
    toScreen      : BOOLEAN;
    zoom          : INTEGER;
    msgWr         : TextWr.T;
    title         : Ctypes.char_star;
    drawBoxes     : BOOLEAN;
    isDirty       : BOOLEAN;
    charLast      : BOOLEAN;
      (* the last thing the user did was type a character.
      If this flag is true and the user left-clicks, any selected string
      is deSelected *)
  OVERRIDES
    init    := Init;
    run     := RunWindow;
  END;

PROCEDURE InitWindow (t: T;  class, title: TEXT) =
  VAR
    cl   : WinUser.WNDCLASS;
    inst : WinDef.HINSTANCE := RTLinker.info.instance;
  BEGIN
    (* The window class must be registered. We assume that Windows 95 
       (like Windows NT) does not do sharing of window classes between 
       processes. We also assume that RegisterClass doesn't capture
       the pointer to the class description, but rather its contents. *)

    cl.lpszClassName := M3toC.CopyTtoS(class);   (* window class name *)
    cl.hInstance     := inst;                    (* handle to this instance *)
    cl.lpfnWndProc   := WndProc;                 (* window function *)
    cl.hCursor       := WinUser.LoadCursor(NIL, WinUser.IDC_ARROW);
(*
    cl.hIcon         := WinUser.LoadIcon(NIL, WinUser.IDI_APPLICATION);
*)
    cl.hIcon         := WinUser.LoadIcon(inst, M3toC.TtoS ("ICON_1"));
    cl.lpszMenuName  := NIL;                     (* no main menu *)

    (* make the background of window white *)
    cl.hbrBackground := WinGDI.GetStockObject (WinGDI.WHITE_BRUSH);
    cl.style         := 0; (* use default window style *)
    cl.cbClsExtra    := 0; (* no extra info *)
    cl.cbWndExtra    := BYTESIZE(t); (* per window extra info *)

    (* register the window class *)
    IF WinUser.RegisterClass(ADR (cl)) = 0 THEN
      Process.Crash ("unable to register window class");
    END;


    (* Now that window has been registered, a window can be created *)
    t.hInstance := inst;
    t.hMenu := WinUser.LoadMenu (inst, M3toC.TtoS("SILMENU")); 
    t.hWindow := WinUser.CreateWindowExA (
        WinUser.WS_OVERLAPPED (*+ WinUser.WS_MAXIMIZE*),  (* style *)
        cl.lpszClassName,            (* name of window class *)
        M3toC.CopyTtoS (title),      (* title *)
        WinUser.WS_OVERLAPPEDWINDOW, (* normal window style *)
        WinUser.CW_USEDEFAULT, (* X coordinate - let Windows decide *)
        WinUser.CW_USEDEFAULT, (* Y coordinate - let Windows decide *)
        WinUser.CW_USEDEFAULT, (* width - let Windows decide *)
        WinUser.CW_USEDEFAULT, (* height - let Windows decide *)
        NIL,                   (* no parent window *)
        t.hMenu,               (* main menu *)
        inst,                  (* handle of this instance *)
        NIL                    (* no additional arguments *)
        );


    (* OUCH!  The only reason that this might work is because the
       main program thread keeps 't' on its stack, so the collector
       can never move it. *)
    EVAL WinUser.SetWindowLong (t.hWindow, 0, LOOPHOLE (t, WinDef.LONG));
  END InitWindow;

PROCEDURE RunWindow (t: T) =
  VAR msg: WinUser.MSG;
  BEGIN
    (* Display the window *)
    EVAL WinUser.ShowWindow (t.hWindow, WinUser.SW_SHOWMAXIMIZED);
    EVAL WinUser.UpdateWindow (t.hWindow);

    (* Create the message loop *)
    WHILE WinUser.GetMessage(ADR(msg), NIL, 0, 0) # 0 DO
      EVAL WinUser.TranslateMessage(ADR(msg)); (* allow use of keyboard *)
      EVAL WinUser.DispatchMessage(ADR(msg));  (* return control to Windows *)
    END;

    Process.Exit (msg.wParam);
  END RunWindow;

<*CALLBACK*>
PROCEDURE WndProc (hWnd   : WinDef.HWND;
                   msg    : WinDef.UINT;
                   wParam : WinDef.WPARAM;
                   lParam : WinDef.LPARAM  ): WinDef.LRESULT =
  VAR t := LOOPHOLE (WinUser.GetWindowLong (hWnd, 0), T); (*!*)
  BEGIN
    CASE msg OF
    | WinUser.WM_DESTROY     => WinUser.PostQuitMessage (0);
    | WinUser.WM_PAINT       => Repaint (t);
    | WinUser.WM_CHAR        => WMChar (t, wParam);
    | WinUser.WM_KEYDOWN     => WMKeyDown (t, wParam);
    | WinUser.WM_LBUTTONDOWN => WMLButtonDown (t, lParam);
    | WinUser.WM_LBUTTONUP   => WMLButtonUp (t);
    | WinUser.WM_MBUTTONDOWN => WMMButtonDown (t, lParam);
    | WinUser.WM_MBUTTONUP   => WMMButtonUp (t, wParam);
    | WinUser.WM_RBUTTONDOWN => WMRButtonDown (t, lParam);
    | WinUser.WM_RBUTTONUP   => WMRButtonUp (t, wParam);
    | WinUser.WM_MOUSEMOVE   => WMMouseMove (t, lParam);
    | WinUser.WM_COMMAND     => MenuCommand (t, Word.And (wParam, 16_ffff));
    ELSE 
      (* Let Windows process all other messages *)
      RETURN WinUser.DefWindowProc(hWnd, msg, wParam, lParam);
    END;
    RETURN 0;
  END WndProc;

PROCEDURE MenuCommand (t: T;  cmd: INTEGER) =
  BEGIN
    CASE cmd OF
    | cm_Open     => FileOpen (t);
    | cm_Save     => FileSave (t);
    | cm_SaveAs   => FileSaveAs (t);
    | cm_Print    => Print (t);
    | cm_Exit     => Exit (t);
    | cm_Copy     => Copy (t);
    | cm_Group    => Group (t);
    | cm_UnGroup  => UnGroup (t);
    | cm_Boxes    => Boxes (t);        
    | cm_Erase    => Erase (t);
    | cm_Fonts    => SelectFonts (t);
    | cm_Fonts+1 .. cm_Fonts+19 => SelectFont (t, cmd - cm_Fonts - 1);
    | cm_Help .. cm_Help+49     => SilHelp.Show (t.hWindow, cmd - cm_Help);
    | cm_Zoom .. cm_Zoom+9      => SetZoom (t, cmd - cm_Zoom + 1);
    | cm_Grid .. cm_Grid+19     => SetGrid (t, cmd - cm_Grid + 1);
    | cm_LineWidth .. cm_LineWidth+19 =>
        SetLineWidth (t, cmd - cm_LineWidth + 1);
    ELSE (*ignore*)
    END;
  END MenuCommand;

PROCEDURE MsgPoint (t: T;  arg: INTEGER): Point.T =
  TYPE TwoWords = RECORD lo, hi: BITS 16 FOR [-16_8000 .. 16_7fff]; END;
  VAR p: Point.T;  pp: WinDef.POINT;  z: TwoWords;
  BEGIN
    z := LOOPHOLE (arg, TwoWords);
    pp.x := z.lo;
    pp.y := z.hi;
    EVAL WinGDI.DPtoLP (t.dc, ADR (pp), 1);
    p.h := Word.And (pp.x, t.gridMask);
    p.v := Word.And (pp.y, t.gridMask);
    RETURN p;
  END MsgPoint;

(*------------------------------------------------------------------- SIL ---*)

PROCEDURE Init (t: T;  title: TEXT): T=
  BEGIN
    InitWindow (t, "Sil", title);

    t.winOrigin    := Point.Origin;
    t.viewOrigin   := Point.Origin;
    t.winExtent.h  := 1600;  t.winExtent.v := 1200;
    t.viewExtent.h := 600;   t.viewExtent.v := 450;

    t.LB_down      := FALSE;
    t.MB_down      := FALSE;
    t.RB_down      := FALSE;

    t.penSize      := 1;
    t.gridMask     := Word.Not (15);
    t.gridSize     := 16;
    t.thePen       := WinGDI.CreatePen (WinGDI.PS_SOLID, t.penSize, 0);
    t.numMarks     := 0;
    t.theFont      := NIL;
    t.first        := NIL;
    t.last         := NIL;
    t.fileName     := "";
    t.drawBoxes    := FALSE;
    t.isDirty      := FALSE;
    t.toScreen     := TRUE;
    t.zoom         := 1;
    t.xscroll      := 600;
    t.yscroll      := 400;
    t.charLast     := FALSE;
    t.msgWr        := NIL;
    t.title        := NIL;

    BuildHelpMenu (t);

    RETURN t;
  END Init;

PROCEDURE BuildHelpMenu (t: T) =
  VAR np := WinUser.CreatePopupMenu ();  txt: TEXT;
  BEGIN
    FOR x := 0 TO 99 DO
      txt := SilHelp.GetMenuEntry (x);
      IF txt = NIL THEN EXIT END;
      EVAL WinUser.AppendMenu (np, WinUser.MF_ENABLED + WinUser.MF_STRING,
                               cm_Help + x, M3toC.CopyTtoS (txt));
    END;

    EVAL WinUser.DeleteMenu (t.hMenu, HelpMenuPos, WinUser.MF_BYPOSITION);
    EVAL WinUser.InsertMenu (t.hMenu, HelpMenuPos,
                       WinUser.MF_POPUP+WinUser.MF_BYPOSITION,
                       LOOPHOLE (np, WinDef.UINT),
                       M3toC.TtoS ("&Help"));
    EVAL WinUser.DrawMenuBar (t.hWindow);
  END BuildHelpMenu;

PROCEDURE Exit (t: T) =
  BEGIN
    IF CanClose (t) THEN
      Cleanup (t);
      WinUser.PostQuitMessage (0);
    END;
  END Exit;

PROCEDURE Erase (t: T) =
  VAR response: INTEGER;
  BEGIN
    response := WinUser.MessageBox (t.hWindow,
                                    M3toC.TtoS ("Erase everything?"),
                                    M3toC.TtoS ("Erase screen"),
                                    WinUser.MB_OKCANCEL);
    IF (response = WinUser.IDOK) THEN
      Cleanup (t);
      BuildFontMenu (t);
      UpdateTitle (t, 0, 0);
      InvalidateWindow (t);
    END;
  END Erase;

PROCEDURE Cleanup (t: T) =
  BEGIN
    t.isDirty  := FALSE;
    t.fileName := "";
    t.first    := NIL;    (* release the top-level objects *)
    t.last     := NIL;
    t.theFont  := NIL;
    t.numMarks := 0;
    SilFont.Reset ();  (* release the font handles *)
  END Cleanup;

PROCEDURE CanClose (t: T): BOOLEAN =
  VAR reply : INTEGER;
  BEGIN
    IF t.isDirty THEN
      reply := WinUser.MessageBox (t.hWindow,
                 M3toC.TtoS ("Do you want to Save?"),
                 M3toC.TtoS ("Drawing has changed"),
                 WinUser.MB_YESNO + WinUser.MB_ICONQUESTION);
      IF reply = WinUser.IDYES THEN
        FileSaveAs (t);
        RETURN NOT t.isDirty;
      END;
    END;
    RETURN TRUE;
  END CanClose;

PROCEDURE Boxes (t: T) =
  BEGIN
    t.drawBoxes := NOT t.drawBoxes;
    InvalidateWindow (t);
  END Boxes;

PROCEDURE UpdateTitle (t: T;  x,y: INTEGER) =
  <*FATAL ANY*>
  VAR wr := t.msgWr;
  BEGIN
    IF (wr = NIL) THEN wr := TextWr.New ();  t.msgWr := wr;  END;
    IF (t.title # NIL) THEN M3toC.FreeCopiedS (t.title); END;
    Wr.PutText (wr, "WindowSIL: ");
    IF (t.fileName # NIL) THEN
      Wr.PutText (wr, t.fileName);
    END;
    IF (x # 0) OR (y # 0) THEN
      Wr.PutText (wr, "    (");
      Wr.PutText (wr, Fmt.Int (x));
      Wr.PutText (wr, ", ");
      Wr.PutText (wr, Fmt.Int (y));
      Wr.PutText (wr, ")");
    END;
    Wr.PutText (wr, "  [grid=");
    Wr.PutText (wr, Fmt.Int (t.gridSize));
    Wr.PutText (wr, "  pen=");
    Wr.PutText (wr, Fmt.Int (t.penSize));
    Wr.PutText (wr, "]");
    t.title := M3toC.CopyTtoS (TextWr.ToText (wr));
    EVAL WinUser.SetWindowText (t.hWindow, t.title);
  END UpdateTitle;

PROCEDURE InvalidateRect (t: T;  READONLY r: Rect.T) =
  VAR rr: WinDef.RECT;
  BEGIN
    (* convert Modula-3 Rect.T to Windows WinDef.RECT *)
    rr.left := r.west;   rr.right  := r.east;
    rr.top  := r.north;  rr.bottom := r.south;

    (* convert to display coordinates *)
    EVAL WinGDI.LPtoDP (t.dc, ADR (rr), 2);
    (*!!! LOOPHOLE(RECT => ARRAY OF POINT)  !!!*)

    (* add 1 pixel of slop around the rectangle *)
    DEC (rr.left);   INC (rr.right);
    DEC (rr.top);    INC (rr.bottom);

    EVAL WinUser.InvalidateRect (t.hWindow, ADR (rr), ORD (TRUE));
  END InvalidateRect;

PROCEDURE InvalidateWindow (t: T) =
  BEGIN
    EVAL WinUser.InvalidateRect (t.hWindow, NIL, ORD (TRUE));
  END InvalidateWindow;

PROCEDURE ResizeDC (t: T;  dc: WinDef.HDC) =
  BEGIN
    EVAL WinGDI.SetMapMode (dc, WinGDI.MM_ANISOTROPIC);
    EVAL WinGDI.SetWindowExtEx (dc, t.winExtent.h, t.winExtent.v, NIL);
    EVAL WinGDI.SetWindowOrgEx (dc, t.winOrigin.h, t.winOrigin.v, NIL);
    EVAL WinGDI.SetViewportExtEx (dc, t.viewExtent.h, t.viewExtent.v, NIL);
    EVAL WinGDI.SetViewportOrgEx (dc, t.viewOrigin.h, t.viewOrigin.v, NIL);
  END ResizeDC;

PROCEDURE Resize (t: T) =
  BEGIN
    t.dc := WinUser.GetDC (t.hWindow);
    ResizeDC (t, t.dc);
  END Resize;

PROCEDURE DrawLine (t: T;  READONLY a, b: Point.T) =
  BEGIN
    DrawLineXY (t, a.h, a.v, b.h, b.v);
  END DrawLine;

PROCEDURE DrawLineXY (t: T;  x0, y0, x1, y1: INTEGER) =
  BEGIN
    EVAL WinGDI.MoveToEx (t.dc, x0, y0, NIL);
    EVAL WinGDI.LineTo   (t.dc, x1, y1);
  END DrawLineXY;

PROCEDURE WMKeyDown (t: T;  key: INTEGER) =
  VAR moved := TRUE;
  BEGIN
    CASE Word.And (key, 127) OF
    | WinUser.VK_LEFT  => t.winOrigin.h := MAX (t.winOrigin.h - t.xscroll, 0);
    | WinUser.VK_RIGHT => t.winOrigin.h := t.winOrigin.h + t.xscroll;
    | WinUser.VK_UP    => t.winOrigin.v := MAX (t.winOrigin.v - t.yscroll, 0);
    | WinUser.VK_DOWN  => t.winOrigin.v := t.winOrigin.v + t.yscroll;
    | WinUser.VK_HOME  => t.winOrigin   := Point.Origin;
    ELSE moved := FALSE;
    END;
    IF moved THEN InvalidateWindow (t) END;
  END WMKeyDown;

PROCEDURE WMChar(t: T;  cmd: INTEGER) =
  VAR
    obj, oldLast: SilObject.T;
    r: Rect.T;
    pt0, pt1, pt2: Point.T;
    dx, dy: INTEGER;
  
  PROCEDURE unSel () =
    VAR obj: SilObject.T;
    BEGIN
      obj := t.first;
      WHILE obj # NIL DO
        IF obj.state = SilObject.Selected THEN
          obj.state := SilObject.Visible;
          InvalidateRect (t, obj.bbox);
        END;
        obj := obj.next
      END;
    END unSel;
  
  BEGIN
    Resize (t);

    cmd := Word.And (cmd, 127);
    CASE cmd OF
    | 1 => (*^a: arc*)
        t.charLast := FALSE;
        IF t.numMarks >= 2 THEN
          IF t.numMarks >= 3 THEN
            PopMark (t, pt0); (* start *)
            PopMark (t, pt1); (* center *)
            PopMark (t, pt2); (* stop *)
            ClearMarkStack (t);
          ELSE (*only 2 marks: start, center*)
            PopMark (t, pt1);
            PopMark (t, pt2);
            pt0 := pt1
          END;
          unSel ();
          VAR arc := NEW (SilArc.T).init (pt2, pt1, pt0, t.penSize); BEGIN
            DrawObj (t, arc);
            Insert (t, arc);
          END;
        END;
  
    | 3 => (*^c: copy selected objects to the point given
                 by the distance between the first two marks*)
        t.charLast := FALSE;
        IF t.numMarks > 1 THEN
          PopMark (t, pt0); (*destination*)
          PopMark (t, pt1); (*source*)
          ClearMarkStack (t);
          dx := pt0.h - pt1.h;
          dy := pt0.v - pt1.v;
          (*push a new mark corresponding to the original source
            displaced by dx, dy*)
          PushMark (t, pt0);
          oldLast := t.last;
          obj := t.first;
          WHILE (obj # NIL) DO
            IF obj.state = SilObject.Selected THEN
              obj.state := SilObject.Visible;
              DrawObj (t, obj);
              VAR oo := obj.clone (); BEGIN
                oo.bbox.west  := obj.bbox.west + dx;
                oo.bbox.east  := obj.bbox.east + dx;
                oo.bbox.north := obj.bbox.north + dy;
                oo.bbox.south := obj.bbox.south + dy;
                oo.state      := SilObject.Selected;
                oo.next       := NIL;
                Insert (t, oo);
                DrawObj (t, oo);
              END;
            END;
            IF obj = oldLast THEN EXIT END;
            obj := obj.next
          END;
        END;
  
    | 4 => (*^d: delete selected objects*)
        t.charLast := FALSE;
        obj := t.first;
        WHILE obj # NIL DO
          IF obj.deleteSelected(r) THEN InvalidateRect (t, r); END;
          obj := obj.next
        END;
        ClearMarkStack (t);
        t.isDirty := TRUE;
  
    | 13 => (*cr*)
        t.charLast := FALSE;
        obj := t.first;
        WHILE (obj # NIL) DO
          IF obj.takeChar (VAL (cmd, CHAR), t, r) THEN
            (*deselect object, clear marks, move mark down by one grid unit*)
            obj.state := SilObject.Visible;
            pt0.h := r.west;  pt0.v := r.south + t.gridSize;
            InvalidateRect (t, r);
            ClearMarkStack (t);
            PushMark (t, pt0);
            DrawMark (t, pt0);
            EXIT;
          END;
          obj:= obj.next;
        END;
  
    | 18 => (*^r: draw a rectangle*)
        t.charLast := FALSE;
        IF t.numMarks > 1 THEN
          unSel ();
          PopMark (t, pt0);
          PopMark (t, pt1);
          ClearMarkStack (t);
          VAR box := NEW (SilBox.T).init (pt0, pt1, t.penSize); BEGIN
            DrawObj (t, box);
            Insert (t, box)
          END;
        END;
  
    | 8, 32..127 => (*edit strings*)
        IF (t.theFont # NIL) THEN
          obj := t.first;
          LOOP
            IF (obj = NIL) THEN
              IF (cmd # 8) AND (t.numMarks > 0) THEN
                PopMark (t, pt0);
                VAR
                  txt := Text.FromChar (VAL (cmd, CHAR));
                  str := NEW (SilString.T).init (pt0, txt, t.theFont, t);
                BEGIN
                  DrawObj (t, str);
                  Insert (t, str);
                END;
              END;
              EXIT;
            ELSIF obj.takeChar (VAL (cmd, CHAR), t, r) THEN
              t.isDirty := TRUE;
              InvalidateRect (t, r);
              EXIT;
            ELSE
              obj := obj.next;
            END;
          END;
          t.charLast := TRUE;
        END;
  
    | 21 => (*^u:  undelete one level*)
        t.charLast := FALSE;
        obj := t.first;
        WHILE obj # NIL DO
          IF obj.state < SilObject.Visible THEN
            INC (obj.state);
            IF obj.state = SilObject.Visible THEN
              DrawObj (t, obj);
            END;
          END;
          obj := obj.next;
        END;
        t.isDirty := TRUE;
  
    | 22 => (*^v: move selected objects by the distance
                  between the first two marks*)
        t.charLast := FALSE;
        IF t.numMarks > 1 THEN
          PopMark (t, pt0); (*destination*)
          PopMark (t, pt1); (*source*)
          ClearMarkStack (t);
          dx := pt0.h - pt1.h;
          dy := pt0.v - pt1.v;
          (*push a new mark corresponding to the original source
            displaced by dx, dy*)
          PushMark (t, pt0);
          obj := t.first;
          WHILE obj # NIL DO
            IF obj.state = SilObject.Selected THEN
              InvalidateRect (t, obj.bbox);
              INC (obj.bbox.west,  dx);
              INC (obj.bbox.east,  dx);
              INC (obj.bbox.north, dy);
              INC (obj.bbox.south, dy);
              InvalidateRect (t, obj.bbox);
            END;
            obj := obj.next
          END;
          t.isDirty := TRUE;
        END;

    | 24 => (*^x: form a group *)
        t.charLast := FALSE;
        Group (t);

    | 25 => (*^y: ungroup the current selection *)
        t.charLast := FALSE;
        UnGroup (t);

    ELSE (* ignore it *)
    END; (*CASE cmd OF*)

    EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
  END WMChar;

PROCEDURE Print (t: T) =
  VAR
    pd: CommDlg.PRINTDLG;
    oldWinOrigin, oldWinExtent: Point.T;
    oldViewOrigin, oldViewExtent: Point.T;
    doc_info: WinGDI.DOCINFO;
  BEGIN
    ClearMarkStack (t);

    RTMisc.Zero (ADR (pd), BYTESIZE (pd));
    pd.hwndOwner   := t.hWindow;
    pd.Flags       := CommDlg.PD_RETURNDC;
    pd.lStructSize := 66 (*** BYTESIZE(pd) = 68 !! ***);
    pd.nCopies     := 1;

    IF CommDlg.PrintDlg (ADR (pd)) # 0 THEN
      oldWinOrigin  := t.winOrigin;    oldWinExtent := t.winExtent;
      oldViewOrigin := t.viewOrigin;   oldViewExtent := t.viewExtent;

      t.winOrigin := Point.Origin;
      t.viewOrigin := Point.Origin;
      t.winExtent.h  := 1600;  t.winExtent.v  := 1200;
      t.viewExtent.h := 3104;  t.viewExtent.v := 2328;

      doc_info.cbSize      := BYTESIZE (doc_info);
      doc_info.lpszDocName := M3toC.TtoS ("Sil.out");
      doc_info.lpszOutput  := NIL;

      EVAL WinGDI.StartDoc (pd.hDC, ADR (doc_info));
      Paint (t, pd.hDC);
      EVAL WinGDI.EndDoc (pd.hDC);
      EVAL WinUser.ReleaseDC (t.hWindow, pd.hDC);
      
      t.winOrigin  := oldWinOrigin;    t.winExtent := oldWinExtent;
      t.viewOrigin := oldViewOrigin;   t.viewExtent := oldViewExtent;
    END;
  END Print;

PROCEDURE SetZoom (t: T;  z: INTEGER) =
  VAR obj: SilObject.T;
  BEGIN
    IF (z = 1) THEN t.viewOrigin := Point.Origin; END;
    t.viewExtent.h     := 600 * z;
    t.viewExtent.v     := 450 * z;
    t.zoom    := z;
    t.xscroll := 600 DIV z;
    t.yscroll := 400 DIV z;
    Resize (t);

    (*When the zoom factor is changed, the bounding boxes for strings
      are no longer correct (the reason is a mystery), so we must zip
      around and fix them up *)
    obj := t.first;
    WHILE obj # NIL DO
      TYPECASE obj OF
      | SilString.T(s) => SilString.SetBBox (s, t);
      ELSE (*skip*)
      END;
      obj := obj.next;
    END;

    EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
    InvalidateWindow (t);
  END SetZoom;

PROCEDURE SetLineWidth (t: T;  width: INTEGER) =
  VAR obj := t.first;  found := FALSE;
  BEGIN
    (*set the linewidth for selected objects.  If none, change the pen size*)
    WHILE obj # NIL DO
      IF obj.setWidth (width, FALSE) THEN found := TRUE; END;
      obj := obj.next
    END;
    IF NOT found
      THEN SetPenSize (t, width);
      ELSE InvalidateWindow (t);
    END;
    UpdateTitle (t, 0, 0);
  END SetLineWidth;

PROCEDURE SetPenSize (t: T;  newSize: INTEGER) =
  BEGIN
    EVAL WinGDI.DeleteObject (t.thePen);
    t.thePen  := WinGDI.CreatePen (WinGDI.PS_SOLID, newSize, 0);
    t.penSize := newSize
  END SetPenSize;

PROCEDURE SetGrid (t: T;  width: INTEGER) =
  BEGIN
    t.gridSize := Word.LeftShift (1, width - 1);
    t.gridMask := Word.Not (t.gridSize - 1);
    UpdateTitle (t, 0, 0);
  END SetGrid;

PROCEDURE WMLButtonDown (t: T;  mouse: INTEGER) =
  VAR cc: INTEGER;
  BEGIN
    IF NOT t.LB_down THEN
      t.LB_down := TRUE;
      EVAL WinUser.SetCapture (t.hWindow);
      REPEAT cc := WinUser.ShowCursor (ORD (FALSE)) UNTIL cc < 0;
      Resize (t);
      EVAL WinGDI.SelectObject (t.dc, WinGDI.CreatePen (WinGDI.PS_SOLID,1,0));
      t.p0 := MsgPoint (t, mouse);
      DrawMark (t, t.p0);
      UpdateTitle (t, t.p0.h, t.p0.v);
    END;
  END WMLButtonDown;

PROCEDURE WMMButtonDown (t: T;  mouse: INTEGER) =
  VAR p0: Point.T;  cc: INTEGER;
  BEGIN
    IF (NOT t.MB_down) AND (t.numMarks > 0) THEN
      t.MB_down := TRUE;
      EVAL WinUser.SetCapture (t.hWindow);
      REPEAT cc := WinUser.ShowCursor (ORD (FALSE)) UNTIL cc < 0;
      Resize (t);
      EVAL WinGDI.SelectObject (t.dc, t.thePen);
      PopMark (t, p0);
      ClearMarkStack (t);
      t.tl[0] := p0;
      t.tl[1] := MsgPoint (t, mouse);
      t.oldROp2 := WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
      DrawLine (t, t.tl[0], t.tl[1]);
      UpdateTitle (t, t.tl[1].h, t.tl[1].v);
    END;
  END WMMButtonDown;

PROCEDURE WMMouseMove (t: T;  mouse: INTEGER) =
  VAR op: Point.T;
  BEGIN
    IF t.LB_down THEN
      op := t.p0;
      t.p0 := MsgPoint (t, mouse);
      IF (op.h # t.p0.h) OR (op.v # t.p0.v) THEN
        DrawMark (t, op);
        DrawMark (t, t.p0);
        UpdateTitle (t, t.p0.h, t.p0.v);
      END;
    END;
  
    IF (t.MB_down) THEN
      op := t.tl[1];
      t.tl[1] := MsgPoint (t, mouse);
      IF (op # t.tl[1]) THEN
        EVAL WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
        DrawLine (t, t.tl[0], op);       (* erase old *)
        DrawLine (t, t.tl[0], t.tl[1]);  (* draw new *)
        UpdateTitle (t, t.tl[1].h, t.tl[1].v)
      END;
    END;
  
    IF (t.RB_down) THEN
      op := t.tl[1];
      t.tl[1] := MsgPoint (t, mouse);
      IF (op # t.tl[1]) THEN
        EVAL WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
        DrawRect (t, t.tl[0], op);       (* erase old *)
        DrawRect (t, t.tl[0], t.tl[1]);  (* draw new *)
      END;
    END;
  END WMMouseMove;

PROCEDURE WMLButtonUp (t: T) =
  VAR obj: SilObject.T;
  BEGIN
    IF t.LB_down THEN
      t.LB_down := FALSE;
      EVAL WinUser.ReleaseCapture ();
      PushMark (t, t.p0);
  
      EVAL WinUser.ShowCursor (ORD (TRUE));
      IF t.charLast THEN
        t.charLast := FALSE;
        (* deselect any selected strings *)
        obj := t.first;
        WHILE obj # NIL DO
          IF (obj.state = SilObject.Selected)
            AND  ISTYPE (obj, SilString.T) THEN
            obj.state := SilObject.Visible;
            DrawObj (t, obj);
          END;
          obj := obj.next
        END;
      END;
      EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
    END;
  END WMLButtonUp;

PROCEDURE WMMButtonUp (t: T;  key: INTEGER) =
  VAR obj: SilObject.T;  line: SilLine.T;
  BEGIN
    IF t.MB_down THEN
      t.charLast := FALSE;
      t.MB_down  := FALSE;
      EVAL WinUser.ReleaseCapture ();

      (*erase old temporary line*)
      EVAL WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
      DrawLine (t, t.tl[0], t.tl[1]);
      EVAL WinGDI.SetROP2 (t.dc, t.oldROp2);

      obj := t.first;
      WHILE obj # NIL DO (*deselect selected objects*)
        IF obj.state = SilObject.Selected THEN
          obj.state := SilObject.Visible;
          InvalidateRect (t, obj.bbox);
        END;
        obj := obj.next
      END;

      IF Word.And (key, WinUser.MK_SHIFT) = 0 THEN
        IF ABS (t.tl[1].h - t.tl[0].h) >= ABS (t.tl[1].v - t.tl[0].v)
          THEN t.tl[1].v := t.tl[0].v; (* horizontal *)
          ELSE t.tl[1].h := t.tl[0].h; (* vertical *)
        END;
      END;

      line := NEW (SilLine.T).init (t.tl[0], t.tl[1], t.penSize);
      (* lines are visible when created *)
      line.state := SilObject.Selected;
      DrawObj (t, line);
      Insert (t, line);
      PushMark (t, t.tl[1]);
      DrawMark (t, t.tl[1]);

      UpdateTitle (t, t.tl[1].h, t.tl[1].v);
      EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
      EVAL WinUser.ShowCursor (ORD (TRUE));
    END;
  END WMMButtonUp;

PROCEDURE WMRButtonDown (t: T;  mouse: INTEGER) =
  VAR cc: INTEGER;
  BEGIN
    IF NOT t.RB_down THEN
      t.RB_down := TRUE;
      EVAL WinUser.SetCapture (t.hWindow);
      REPEAT cc := WinUser.ShowCursor (ORD (FALSE)) UNTIL cc < 0;
      Resize (t);
      EVAL WinGDI.SelectObject (t.dc, WinGDI.GetStockObject(WinGDI.BLACK_PEN));
      ClearMarkStack (t);
      t.tl[0] := MsgPoint (t, mouse);
      t.tl[1] := t.tl[0];
      t.oldROp2 := WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
      DrawRect (t, t.tl[0], t.tl[1]);
    END;
  END WMRButtonDown;

PROCEDURE WMRButtonUp (t: T;  key: INTEGER) =
  VAR
    p0, p1: Point.T;
    point, dso: BOOLEAN;
    obj: SilObject.T;
    sel, r: Rect.T;
  BEGIN
    IF t.RB_down THEN
      t.charLast := FALSE;
      t.RB_down := FALSE;
      EVAL WinUser.ReleaseCapture ();

      p0 := t.tl[0];
      p1 := t.tl[1];

      (*erase old temporary rectangle*)
      EVAL WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
      DrawRect (t, p0, p1);
      EVAL WinGDI.SetROP2 (t.dc, t.oldROp2);

      sel.north := MIN (p0.v, p1.v);  sel.west := MIN (p0.h, p1.h);
      sel.south := MAX (p0.v, p1.v);  sel.east := MAX (p0.h, p1.h);
      point := ((sel.south - sel.north) <= 2) OR ((sel.east - sel.west) <= 2);
      dso := Word.And (key, WinUser.MK_SHIFT) = 0;

      obj := t.first;
      WHILE obj # NIL DO
        IF obj.select (sel, point, dso, r) THEN
          InvalidateRect (t, r);
        END;
        obj := obj.next;
      END;

      EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
      EVAL WinUser.ShowCursor (ORD (TRUE));
      UpdateTitle (t, 0, 0);
    END;
  END WMRButtonUp;

PROCEDURE DrawRect (t: T;  READONLY a, b: Point.T) =
  BEGIN
    DrawLineXY (t, a.h, a.v, a.h, b.v);
    DrawLineXY (t, a.h, b.v, b.h, b.v);
    DrawLineXY (t, b.h, b.v, b.h, a.v);
    DrawLineXY (t, b.h, a.v, a.h, a.v);
  END DrawRect;
  
PROCEDURE Repaint (t: T) =
  VAR ps: WinUser.PAINTSTRUCT;  dc: WinDef.HDC;
  BEGIN
    dc := WinUser.BeginPaint (t.hWindow, ADR(ps));
    Paint (t, dc);
    EVAL WinUser.EndPaint (t.hWindow, ADR(ps));
  END Repaint;

PROCEDURE Paint (t: T;  PaintDC: WinDef.HDC) =
  VAR obj: SilObject.T;  oldDC: WinDef.HDC;
  BEGIN
    oldDC := t.dc;
    t.dc := PaintDC;

    EVAL WinGDI.SetMapMode (t.dc, WinGDI.MM_ANISOTROPIC);
    IF t.toScreen THEN
      ResizeDC (t, t.dc);
      FOR i := 0 TO t.numMarks-1 DO DrawMark (t, t.markStack[i]); END;
      IF t.MB_down THEN
        EVAL WinGDI.SelectObject (t.dc, t.thePen);
        DrawLine (t, t.tl[0], t.tl[1]);
      END;
    END;
  
    obj := t.first;
    WHILE obj # NIL DO
      DrawObj (t, obj);
      obj := obj.next;
    END;

    t.dc := oldDC;
  END Paint;
  
PROCEDURE DrawObj (t: T;  obj: SilObject.T) =
  BEGIN
    IF obj.state >= SilObject.Visible THEN
      obj.draw (Point.Origin, FALSE, t);
      IF t.drawBoxes THEN DrawBBox (t, obj.bbox); END;
    END;
  END DrawObj;

PROCEDURE DrawBBox (t: T;  READONLY bbox: Rect.T) =
  VAR new: WinDef.HPEN;  old: WinDef.HGDIOBJ;
  BEGIN
    new := WinGDI.CreatePen (WinGDI.PS_SOLID, 1, 16_ff00(*green*));
    old := WinGDI.SelectObject (t.dc, new);
    EVAL WinGDI.SelectObject (t.dc,
            WinGDI.GetStockObject (WinGDI.HOLLOW_BRUSH));
    EVAL WinGDI.Rectangle (t.dc, bbox.west,
                                 bbox.north,
                                 bbox.east,
                                 bbox.south);
    EVAL WinGDI.DeleteObject (new);
    EVAL WinGDI.SelectObject (t.dc, old);
  END DrawBBox;

PROCEDURE Copy (t: T) =
  (*copy to the clipboard *)
  VAR
    br     : INTEGER := 0;
    bb     : INTEGER := 0;
    mf     : WinDef.HANDLE;
    mfdc   : WinDef.HDC;
    mfp    : WinGDI.LPMETAFILEPICT;
    gh     : WinDef.HANDLE;
    obj    : SilObject.T;
  BEGIN
    (* find the image size *)
    obj := t.first;
    WHILE (obj # NIL) DO
      IF obj.state >= SilObject.Visible THEN
        br := MAX (br, obj.bbox.east);
        bb := MAX (bb, obj.bbox.south);
      END;
      obj := obj.next
    END;

    (* paint the image to memory *)
    mfdc := WinGDI.CreateMetaFile (NIL);
    EVAL WinGDI.SetWindowExtEx (mfdc, br, bb, NIL);
    EVAL WinGDI.SetWindowOrgEx (mfdc, 0, 0, NIL);
    t.toScreen := FALSE;
    Paint(t, mfdc);
    t.toScreen := TRUE;
    mf := WinGDI.CloseMetaFile(mfdc);

    IF WinUser.OpenClipboard (t.hWindow) # 0 THEN
      gh := WinBase.GlobalAlloc (WinBase.GMEM_MOVEABLE, BYTESIZE(mfp^));
      IF gh # NIL THEN
        mfp := WinBase.GlobalLock (gh);
        mfp.mm := WinGDI.MM_ANISOTROPIC;
        (* a logical unit should wind up as 1/150 inch.  The metafile
           sizes are in hiMetric units (.01mm).  (25.4/150)/.01 = 17. *)
        mfp.xExt := 17*br + 10;
        mfp.yExt := 17*bb + 10;
        mfp.hMF  := mf;
        EVAL WinBase.GlobalUnlock (gh);
        EVAL WinUser.EmptyClipboard ();
        EVAL WinUser.SetClipboardData (WinUser.CF_METAFILEPICT, gh);
        EVAL WinUser.CloseClipboard ();
      END;
    END;
  END Copy;
  
PROCEDURE Group (t: T) =
  (*make a macro of all selected objects*)
  VAR
    bbox : Rect.T;
    defn : SilMacro.Defn;
    macro : SilObject.T := NIL;
    obj, next: SilObject.T;
  BEGIN
    (* build an empty bounding box that can grow *)
    bbox.north := LAST (INTEGER);
    bbox.south := FIRST (INTEGER);
    bbox.west  := LAST (INTEGER);
    bbox.east  := FIRST (INTEGER);

    (* put the selected items on the 'macro' list and remove them
       from the list of top-level items. *)
    obj := t.first;
    t.first := NIL;
    t.last  := NIL;
    WHILE (obj # NIL) DO
      next := obj.next;
      obj.next := NIL;
      IF (obj.state = SilObject.Selected) THEN
        (*add obj to the list of macro objects*)
        obj.next := macro;
        obj.state := SilObject.Visible;
        macro := obj;
        (* update the bounding box *)
        bbox.west  := MIN (bbox.west,  obj.bbox.west);
        bbox.east  := MAX (bbox.east,  obj.bbox.east);
        bbox.north := MIN (bbox.north, obj.bbox.north);
        bbox.south := MAX (bbox.south, obj.bbox.south);
      ELSIF (t.first = NIL) THEN
        t.first := obj;
        t.last  := obj;
      ELSE
        t.last.next := obj;
        t.last  := obj;
      END;
      obj := next;
    END;

    (*make the bbox of each selected object relative to the bbox of the macro*)
    obj := macro;
    WHILE obj # NIL DO
      DEC (obj.bbox.west,  bbox.west);
      DEC (obj.bbox.east,  bbox.west);
      DEC (obj.bbox.north, bbox.north);
      DEC (obj.bbox.south, bbox.north);
      obj := obj.next
    END;

    (*make the macro object and insert it in the mainlist*)
    IF macro # NIL THEN
      defn := SilMacro.NewDefn ();
      WHILE (macro # NIL) DO
        obj := macro;
        macro := macro.next;
        obj.next := NIL;
        SilMacro.AddObj (defn, obj);
      END;
      Insert (t, NEW (SilMacro.T).init(bbox, defn));
      IF t.drawBoxes THEN InvalidateRect (t, bbox); END;
    END;
  END Group;
  
PROCEDURE UnGroup (t: T) =
  (*expand a selected macro into its compnents*)
  VAR
    obj,newobj, macro: SilObject.T;
    bl, bt: INTEGER;
  BEGIN
    obj := t.first;
    WHILE obj # NIL DO
      IF obj.state < SilObject.Visible THEN
        DEC (obj.state);
      ELSIF (obj.state = SilObject.Selected) AND ISTYPE (obj, SilMacro.T) THEN
        obj.state := SilObject.Visible - 1;  (*delete the instance*)
        bl := obj.bbox.west;
        bt := obj.bbox.north;
        macro := SilMacro.Contents (obj);
        WHILE macro # NIL DO
          newobj            := macro.clone ();
          newobj.state      := SilObject.Selected;
          newobj.bbox.west  := macro.bbox.west + bl;
          newobj.bbox.east  := macro.bbox.east + bl;
          newobj.bbox.north := macro.bbox.north + bt;
          newobj.bbox.south := macro.bbox.south + bt;
          Insert (t, newobj);
          macro := macro.next;
        END;
        t.isDirty := TRUE;
        IF t.drawBoxes THEN InvalidateRect (t, obj.bbox); END;
      END;
      obj := obj.next;
    END;
  END UnGroup;
  
PROCEDURE Insert (t: T;  p: SilObject.T) =
  BEGIN
    IF t.first = NIL
      THEN t.first := p
      ELSE t.last.next := p;
    END;
    t.last := p;
    t.isDirty := TRUE;
  END Insert;

PROCEDURE PushMark (t: T;  READONLY p: Point.T) =
  BEGIN
    IF t.numMarks = 4 THEN (*full stack*)
      InvalidateMark (t, t.markStack[3]);
      t.numMarks := 3
    END;
    FOR i := t.numMarks TO 1 BY -1 DO
      t.markStack[i] := t.markStack[i-1];
    END;
    t.markStack[0] := p;
    INC (t.numMarks);
  END PushMark;
  
PROCEDURE PopMark (t: T;  VAR(*OUT*) p: Point.T) =
  BEGIN
    p := t.markStack[0];
    IF t.numMarks > 0 THEN
      DEC (t.numMarks);
      InvalidateMark (t, p)
    END;
    FOR i := 0 TO t.numMarks-1 DO
      t.markStack[i] := t.markStack[i+1];
    END;
  END PopMark;
  
PROCEDURE DrawMark (t: T;  READONLY p: Point.T) =
  VAR oldROp2: INTEGER;  
  BEGIN
    oldROp2 := WinGDI.SetROP2 (t.dc, WinGDI.R2_NOT);
    DrawLineXY (t,  p.h - 5, p.v,  p.h - 1, p.v);
    DrawLineXY (t,  p.h + 1, p.v,  p.h + 5, p.v);
    DrawLineXY (t,  p.h, p.v - 5, p.h,  p.v - 1);
    DrawLineXY (t,  p.h, p.v + 1, p.h,  p.v + 5);
    EVAL WinGDI.SetROP2 (t.dc, oldROp2);
  END DrawMark;
  
PROCEDURE ClearMarkStack (t: T) =
  VAR p: Point.T;
  BEGIN
    WHILE t.numMarks > 0 DO
      PopMark (t, p);
      InvalidateMark (t, p)
    END;
  END ClearMarkStack;
  
PROCEDURE InvalidateMark (t: T;  READONLY p: Point.T) =
  VAR r: Rect.T;
  BEGIN
    r.north := p.v - 6;
    r.south := r.north + 12;
    r.west  := p.h - 6;
    r.east  := r.west + 12;
    InvalidateRect (t, r);
  END InvalidateMark;

VAR
  read_procs := ARRAY CHAR OF ReadProc { NIL, .. };

PROCEDURE RegisterReader (min, max: CHAR;  r: ReadProc) =
  BEGIN
    FOR ch := min TO max DO
      <*ASSERT read_procs[ch] = NIL*>
      read_procs[ch] := r;
    END;
  END RegisterReader;
  
PROCEDURE FileOpen (t: T) =
  CONST
    DefExt  = "sil";
    Filter  = "Sil Files\000*.sil\000All Files (*.*)\000*.*\000\000";
  VAR
    openFN    : CommDlg.OPENFILENAME;
    fn        : ARRAY [0..100] OF CHAR; 
    cur_macro : SilMacro.Defn;
    rd        : SilRd.T;
    AddFile   : BOOLEAN;
    proc      : ReadProc;
    ref       : REFANY;
  
  BEGIN
    IF NOT CanClose (t) THEN
      (* => drawing is dirty and the user wants to save it first *)
      RETURN;
    END;

    ClearMarkStack (t);

    AddFile := (WinUser.GetKeyState (WinUser.VK_SHIFT) < 0) AND (t.first #NIL);
    IF NOT AddFile THEN Cleanup (t); END;

    fn[0] := '\000';

    RTMisc.Zero (ADR (openFN), BYTESIZE(openFN));
    WITH x = openFN DO
      x.hInstance      := t.hInstance;
      x.hwndOwner      := t.hWindow;
      x.lpstrDefExt    := M3toC.TtoS (DefExt);
      x.lpstrFile      := ADR (fn[0]);
      x.lpstrFilter    := M3toC.TtoS (Filter);
      x.lpstrFileTitle := NIL;
      x.Flags          := CommDlg.OFN_FILEMUSTEXIST;
      x.lStructSize    := BYTESIZE (openFN);
      x.nFilterIndex   := 1;
      x.nMaxFile       := BYTESIZE (fn);
    END;

    IF CommDlg.GetOpenFileName (ADR (openFN)) # 0 THEN
      (*read the file*)
      rd := SilRd.Open (M3toC.StoT (ADR (fn[0])));

      WHILE SilRd.ParseNextLine (rd) DO
        proc := read_procs [rd.cmd];
        IF proc = NIL THEN
          SilError.Put (NIL, "unexpected character '",
                        Text.FromChar(rd.cmd), "' in input file");
        ELSE
          ref := proc (rd);
          TYPECASE ref OF
          | NULL =>
              (* ignore *)
          | SilObject.T (obj) =>
              IF (rd.in_macro)
                THEN SilMacro.AddObj (cur_macro, obj);
                ELSE Insert (t, obj);
              END;
          | SilMacro.Defn (m) =>
              cur_macro := m;
          ELSE
              <*ASSERT FALSE*>
          END;
        END;
      END; (*WHILE*)

      SilRd.Close (rd);
    END; (*IF openfile*)

    IF (t.theFont = NIL) THEN t.theFont := SilFont.FromMenu (0); END;
    IF NOT AddFile THEN t.fileName := M3toC.CopyStoT (ADR (fn[0])); END;
    t.isDirty := FALSE;
    BuildFontMenu (t);
    UpdateTitle (t, 0, 0);
    InvalidateWindow (t);
  END FileOpen;
  
PROCEDURE WriteFile (t: T;  name: TEXT) =
  VAR
    wr  := SilWr.Open (name);
    obj : SilObject.T;
  BEGIN
    ClearMarkStack (t);

    (* dump the objects *)
    obj := t.first;
    WHILE obj # NIL DO
      IF obj.state >= SilObject.Visible THEN obj.write (wr); END;
      obj := obj.next
    END;

    SilWr.Close(wr);

    t.isDirty := FALSE;
    UpdateTitle (t, 0, 0);
  END WriteFile;
  
PROCEDURE FileSave (t: T) =
  BEGIN
    IF (t.fileName # NIL) AND Text.Length (t.fileName) # 0
      THEN WriteFile (t, t.fileName);
      ELSE FileSaveAs (t); 
    END;
  END FileSave;
  
PROCEDURE FileSaveAs (t: T) =
  CONST
    DefExt = "sil";
    Filter = "Sil Files\000*.sil\000All Files (*.*)\000*.*\000\000";
  VAR
    openFN       : CommDlg.OPENFILENAME;
    fullFileName : ARRAY [0..255] OF CHAR;
    fileTitle    : ARRAY [0..255] OF CHAR;
  BEGIN
    fullFileName[0] := '\000';
    IF (t.fileName = NIL) THEN
      fileTitle[0] := '\000';
    ELSE
      Text.SetChars (fileTitle, t.fileName);
      fileTitle [MIN (LAST(fileTitle), Text.Length(t.fileName))] := '\000';
    END;

    RTMisc.Zero (ADR (openFN), BYTESIZE (openFN));
    WITH x = openFN DO
      x.hInstance      := t.hInstance;
      x.hwndOwner      := t.hWindow;
      x.lpstrDefExt    := M3toC.TtoS (DefExt);
      x.lpstrFile      := ADR (fullFileName);
      x.nMaxFile       := BYTESIZE (fullFileName);
      x.lpstrFilter    := M3toC.TtoS (Filter);
      x.lpstrFileTitle := ADR (fileTitle);
      x.nMaxFileTitle  := BYTESIZE (fileTitle);
      x.Flags          := 0;
      x.lStructSize    := BYTESIZE (x);
      x.nFilterIndex   := 1;
    END;
    IF CommDlg.GetSaveFileName (ADR (openFN)) # 0 THEN
      t.fileName := M3toC.CopyStoT (ADR (fileTitle[0]));
      WriteFile (t, M3toC.CopyStoT (ADR (fullFileName[0])));
    END;
  END FileSaveAs;
  

PROCEDURE BuildFontMenu (t: T) =
  BEGIN
    EVAL WinUser.DeleteMenu (t.hMenu, FontMenuPos, WinUser.MF_BYPOSITION);
    EVAL WinUser.InsertMenu (t.hMenu, FontMenuPos,
                       WinUser.MF_POPUP+WinUser.MF_BYPOSITION,
                       LOOPHOLE (SilFont.BuildMenu (cm_Fonts), WinDef.UINT),
                       M3toC.TtoS ("F&ont"));
    EVAL WinUser.DrawMenuBar (t.hWindow);
  END BuildFontMenu;

PROCEDURE BuildFont (t: T;  READONLY x: WinGDI.LOGFONT): SilFont.T =
  VAR f := SilFont.New (M3toC.CopyStoT (ADR (x.lfFaceName)),
                        x.lfHeight, x.lfWeight, x.lfItalic # 0);
  BEGIN
    BuildFontMenu (t);
    RETURN f;
  END BuildFont;

PROCEDURE SelectFont (t: T;  fontIndex: INTEGER) =
  VAR
    obj   : SilObject.T := t.first;
    found : BOOLEAN     := FALSE;
    fid   : SilFont.T   := SilFont.FromMenu (fontIndex);
  BEGIN
    t.dc := WinUser.GetDC (t.hWindow);

    (*change the font for a selected string. If none, change the current font*)
    WHILE obj # NIL DO
      IF obj.setFont (fid, FALSE, t) THEN found := TRUE; END;
      obj := obj.next;
    END;
    IF NOT found
      THEN t.theFont := fid
      ELSE InvalidateWindow (t);
    END;

    EVAL WinUser.ReleaseDC (t.hWindow, t.dc);
  END SelectFont;

PROCEDURE SelectFonts (t: T) =
  VAR
    chooseRec: CommDlg.CHOOSEFONT;
    fontRec  : WinGDI.LOGFONT;
  BEGIN
    WITH x = fontRec DO
      x.lfHeight        := 11;
      x.lfWidth         := 0;
      x.lfEscapement    := 0;
      x.lfOrientation   := 0;
      x.lfWeight        := WinGDI.FW_REGULAR;
      x.lfItalic        := 0;
      x.lfUnderline     := 0;
      x.lfStrikeOut     := 0;
      x.lfCharSet       := WinGDI.ANSI_CHARSET;
      x.lfOutPrecision  := WinGDI.OUT_DEFAULT_PRECIS;
      x.lfClipPrecision := WinGDI.CLIP_DEFAULT_PRECIS;
      x.lfQuality       := WinGDI.PROOF_QUALITY;
      x.lfPitchAndFamily:= WinGDI.VARIABLE_PITCH + WinGDI.FF_ROMAN;
      EVAL Cstring.strncpy (ADR (x.lfFaceName[0]), M3toC.TtoS ("Arial"),
                            NUMBER (x.lfFaceName));
    END;

    RTMisc.Zero (ADR (chooseRec), BYTESIZE (chooseRec));
    WITH x = chooseRec DO
      x.lStructSize:= BYTESIZE (chooseRec);
      x.hwndOwner  := t.hWindow;
      x.lpLogFont  := ADR (fontRec);
      x.nSizeMin   := 6;
      x.nSizeMax   := 48;
      x.Flags      := CommDlg.CF_ANSIONLY
                       + CommDlg.CF_TTONLY
                       + CommDlg.CF_SCREENFONTS
                       + CommDlg.CF_INITTOLOGFONTSTRUCT
                       + CommDlg.CF_LIMITSIZE;
    END;
    IF CommDlg.ChooseFont (ADR (chooseRec)) # 0 THEN
      t.theFont := BuildFont (t, fontRec);
    END;
  END SelectFonts;

BEGIN
END SilWindow.
