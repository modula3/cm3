(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Main program *)

(* Last modified on Tue Oct  8 14:25:43 PDT 1996 by birrell   *)
(*      modified on Mon Jun 24 08:37:49 PDT 1996 by mcjones   *)
(*      modified on Fri Sep  8 15:45:25 PDT 1995 by weihl     *)
(*      modified on Thu Aug 31 17:10:34 PDT 1995 by perl      *)

MODULE Lectern EXPORTS Main;

(* Main imports. PLEASE KEEP IN ALPHABETICAL ORDER! *)
IMPORT
  AnyEvent, Axis,
  Cursor,
  Env,
  FileRd, FileWr, Find, FlexVBT, FloatMode, Fmt, Font, FormsVBT, FS, FVTypes,
  ImageRd, Images, ImageVBT,
  KeyboardKey,
  Latin1Key, LecternBundle, LecternDoc, LecternOCR, LecternServer, Lex, Links,
    ListVBT,
  MultiSplit, MyFileBrowserVBT,
  NumericVBT,
  OSError,
  PaintOp, Params, Pathname, Pipe, Point, Process,
  Random, Rd, ReactivityVBT, Rect, RefSeq, Region, Rsrc,
  Scan, ScrnPixmap, Shadow, ShadowedVBT, Split, SplitterVBT, StableVBT, Stdio,
  TempFiles, Text, TextPort, TextRd, TextSeq, TextWr, Thread,
    ThumbnailVBT, Time, Trestle, TrestleComm,
  VBT, VBTClass, ViewAreaVBT,
  Wr,
  XTrestle,
  ZSplit;

<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>
<*FATAL Wr.Failure *>

(********* Version ********)
CONST Version = "Version 1.2.4 of 8th October 1996";


(* *)
(* NOTE: all procedures in this program should have locking-level comments *)
(* *)


(* *)
(* Types, constants and such *)
(* *)

CONST
  ImageGamma = 0.625;
  ThumbnailGamma = 0.625;
  InitImageTitle = "Lectern: initializing viewer";
  ImageSuffix = "";
  ThumbnailSuffix = " (thumbnails)";
  ServerSuffix = " (server)";
  EmptyImageTitle = "<Empty>" & ImageSuffix;
  EmptyThumbnailTitle = "<Empty>" & ThumbnailSuffix;
  AppName = "Lectern";
  DefaultHighlight = TRUE;   (* highlighting during MoveTo *)
  DefaultPaintChunk = 0;     (* painting granularity for image *)
  IncrementalPaint = 100000; (* painting granularity for incremental paint *)
  DefaultAnimation = 0.0;    (* seconds per pixel for animated moveTo's *)
  SmoothAnimation = 0.0005;  (* animation speed for smooth scrolling *)
  Margin = 3;                (* keep image from edge of visible area *)
  Overlap = 9;               (* overlap sectors in NextSector/PrevSector *)
  ThumbnailWindow = FALSE;   (* Use separate window for thumbnails *)
  ReadNextPage = FALSE;      (* read page+1 *)
  ReadPrevPage = FALSE;      (* read page-1 *)
  PreReadPage = FALSE;       (* Use ImageRd's pre-reading option *)


TYPE LectFilterVBT = StableVBT.T OBJECT
    (* The top level VBT in the Image and Thumbnail windows. *)
    (* Mostly transparent, but intercepts keystrokes and selection *)
    lect: Lectern;
    waitCursor := Cursor.DontCare;
    childFocus: VBT.T := NIL;
    isPassive: BOOLEAN := FALSE;
    prevKeyDown := VBT.NoKey;
    typeAhead: RefSeq.T := NIL;
  METHODS
    takeFocus(ts: VBT.TimeStamp) := KeyFilterTakeFocus;
      (* LL.sup < v *)
    passive(bePassive: BOOLEAN) := KeyFilterPassive;
      (* LL = VBT.mu *)
    flush() := KeyFilterFlush;
      (* LL = VBT.mu *)
  OVERRIDES
    misc := KeyFilterMisc;
    reshape := KeyFilterReshape;
    mouse := KeyFilterMouse;
    key := KeyFilterKey;
    acquire := KeyFilterAcquire;
  END;

TYPE LecternFVClass = FormsVBT.T OBJECT
    lect: Lectern;
    firstPopup := TRUE; (* For ShowZVisible *)
  OVERRIDES
    mouse := LecternFVMouse;
    realize := LecternFVRealize;
  END;

TYPE MyLinks = Links.T OBJECT
    lect: Lectern;      (* The viewer whose outline can be modified *)
  OVERRIDES
    update := UpdateLinks;
  END;

TYPE PageNumber = INTEGER;

TYPE Op = {
    Null,               (* does nothing *)
  (* Initialization *)
    Init,               (* Creates VBT's and reads document from .initLink *)
  (* External (these are queued) *)
    Request,            (* Services .clientRequest and .clientError *)
    Reshape,            (* Responds to reshaping image VBT *)
  (* Viewing Controls window *)
    FullPage,
    HorThenVer,
    VerThenHor,
    HorOnly,
    VerOnly,
    ViewAreaHit,        (* Adjusts viewArea rectangles *)
    AutoViewArea,       (* Switches to automatic view area calculation *)
    ManualViewArea,     (* Switches to amnual view area adjustment *)
  (* Thumbnail window and link tiles *)
    BookmarkHit,        (* Jumps to the bookmark *)
    DiaryHit,           (* Jumps to the diary entry *)
    OutlineHit,         (* Jumps to the outline entry *)
    ThumbnailHit,       (* Moves to that page *)
  (* Menu window *)
    About,              (* Pops up "About" dialog *)
    Open,               (* Pops up dialog for opening new document *)
    Close,              (* Closes this viewer *)
    Detach,             (* Makes this viewer no longer the server *)
    SaveDoc,            (* Save modified document *)
    Print,              (* Pops up dialog for printing *)
    ShowHelp,           (* Makes help window visible *)
    Quit,               (* Terminates application *)
    NextSector,         (* Moves to next sector *)
    PrevSector,         (* Moves to previous sector *)
    NextPage,           (* Move to next page *)
    PrevPage,           (* Moves to previous page *)
    NextPage5,          (* Move 5 pages forward *)
    PrevPage5,          (* Move 5 pages back *)
    NextPage20,
    PrevPage20,
    FirstPage,          (* Moves to first page in document *)
    LastPage,           (* Moves to last page in document *)
    ContentsPage,       (* Moves to contents page *)
    IndexPage,          (* Moves to index page *)
    GotoPage,           (* Pops up "Goto Page" dialog *)
    GotoPageConfirmed,  (* double-click in image window *)
    Home,               (* Goes to selection *)
    ShowFind,           (* Shows find dialog window, adjusting pos or sel *)
    ShowHideViewing,    (* Shows or hides viewing controls *)
    ShowHideMenu,       (* Shows or hides menu anchor *)
    OpenCloseMenu,      (* Pull down or close menu (down-arrow button) *)
    ShowHideToolbar,    (* Shows or hides the toolbar *)
    ShowImage,          (* Shows image window *)
    ShowHideLinks,      (* Shows or hides links column *)
    Zoom,               (* Zooms image window *)
    Smaller,            (* Uses smaller images *)
    Larger,             (* Uses larger images *)
    (* AutoSelect button invokes Op.AutoViewArea *)
    Undo,
    Redo,
    NewLink,
    EditLink,
    JumpLink,
  (* Popup window *)
    PrintToPrinter,     (* Event in "To Printer" choice button *)
    PrintToFile,        (* Event in "To File" choice button *)
    FileUseOriginal,    (* Event in "Original PS" choice button *)
    FileUseImages,      (* Event in "Images" choice button *)
    Stop,               (* Alerts current worker thread *)
    Yes,                (* General-purpose positive modal response *)
    No,                 (* General-purpose negative modal response *)
    OpenNew,            (* Alternative response in "open file" dialog *)
    GoForward,          (* "Fwd" in "goto" dialog *)
    GoBackward,         (* "Back" in "goto dialog *)
  (* Links tile column *)
    BookmarkZoom,       (* Zooms bookmarks tile *)
    DiaryZoom,          (* Zooms diary tile *)
    OutlineZoom,        (* Zooms outline Tile *)
    ThumbnailZoom,      (* Zooms thumbnail tile *)
    BookmarkNew,        (* New bookmark *)
    DiaryNew,           (* New diary entry *)
    OutlineNew,         (* New outline entry *)
    BookmarkEdit,       (* Edit bookmarks *)
    DiaryEdit,          (* Edit diary *)
    OutlineEdit,        (* Edit outline *)
  (* Find window *)
    FindFirst,
    FindNext,
    FindPrevious,
    FindNextConfirmed   (* Use source selection as search target; find next *)
  };

CONST
  FindDlgOps = SET OF Op {
    (* Op's that leave the find dialog visible *)
    Op.FindFirst,
    Op.FindNext,
    Op.FindPrevious
  };

  ResponseOps = SET OF Op {
    (* Op's that are responses that complete a modal wait *)
    Op.Yes,
    Op.OpenNew,
    Op.GoForward,
    Op.GoBackward,
    Op.No
  };

  AsyncOps = SET OF Op {
    (* Op's that can be performed asynchronously with worker threads *)
    Op.Stop,
    Op.PrintToPrinter,
    Op.PrintToFile,
    Op.FileUseOriginal,
    Op.FileUseImages
  };

  ImmediateOps = SET OF Op {
    (* Op's that can be handled synchronously, if no work in progress *)
    Op.Null,
    Op.About,
    Op.Detach,
    Op.ShowHelp,
    Op.ShowHideViewing,
    Op.ShowHideMenu,
    Op.OpenCloseMenu,
    Op.ShowHideToolbar,
    Op.ShowImage,
    Op.ShowHideLinks,
    Op.BookmarkZoom,
    Op.DiaryZoom,
    Op.OutlineZoom,
    Op.ThumbnailZoom
  };

TYPE Partition = { FullPage, HorThenVer, VerThenHor, HorOnly, VerOnly };

TYPE UndoRec = RECORD
    next: Undo;
    link: Links.Link;
  END;

TYPE Undo = REF UndoRec;

TYPE Tile = { Bookmark, Diary, Outline, Thumbnail };
  (* Tiled sub-windows *)

CONST AllTilesNormal = ARRAY Tile OF BOOLEAN{ FALSE, .. };

CONST
  TileName = ARRAY Tile OF TEXT{
    "BookmarkTile", "DiaryTile", "OutlineTile", "ThumbnailTile"};
  TileShape = TileName;
  TileBrowser = ARRAY Tile OF TEXT{
    "BookmarkBrowser", "DiaryBrowser", "OutlineBrowser", "ThumbnailBrowser"};

TYPE Lectern = MUTEX OBJECT
  (* This represents an instance of Lectern viewing a single document.
     It contains a variety of VBT's, the document itself, and minor bits
     of state. *)
  (* The following field is constant after creation *)
    initLink: Links.Link;                (* Argument for Op.Init *)
  (* The following fields are read-only after Op.Init completes *)
    rsrcPath: Rsrc.Path;                 (* constant *)
    waitCursor := Cursor.DontCare;       (* for passive state in each VBT *)
    bg: PaintOp.T;                       (* for image and thumbnail VBT's *)
    shadow: Shadow.T := NIL;             (* for file browser from NumericVBT *)
    shadow2: Shadow.T := NIL;            (* for file browser from NumericVBT *)
    font: Font.T := Font.BuiltIn;        (* for file browser from NumericVBT *)
    clientMiscCode: VBT.MiscCodeType;    (* for LecternClient requests *)
    iw: LectFilterVBT := NIL;            (* window for main image VBT *)
    iz: ZSplit.T := NIL;                 (* ZSplit in .iw *)
    iv: LectImageVBT := NIL;             (* the main image VBT *)
    popupFV: LecternFVClass := NIL;      (* the pop-up form *)
    fb: MyFileBrowserVBT.T := NIL;       (* File Browser widget *)
    progressVBT: ProgressVBT := NIL;     (* Progress widget *)
    menuFV: LecternFVClass := NIL;       (* the menu form *)
    toolbarFV: LecternFVClass := NIL;    (* the toolbar form *)
    viewingFV: LecternFVClass := NIL;    (* the viewing controls form *)
    viewArea: ViewAreaVBT.T := NIL;      (* ViewArea widget *)
    thumbnailW: LectFilterVBT := NIL;    (* the thumbnail window *)
    thumbnailFilter: ReactivityVBT.T;    (* filter for thumbnailW *)
    mainSplit: SplitterVBT.T := NIL;     (* Splitter for inserting .tileFV *)
    tileVBT: ARRAY Tile OF VBT.T;        (* the tiled sub-windows *)
    tileSplitter: SplitterVBT.T := NIL;  (* Splitter for the tiles *)
    tileFV: FormsVBT.T := NIL;           (* form for the tiled sub-windows *)
    bookmarkVBT: ListVBT.T := NIL;       (* the bookmark widget *)
    diaryVBT: ListVBT.T := NIL;          (* the diary widget *)
    outlineVBT: ListVBT.T := NIL;        (* the outline widget *)
    thumbnailVBT: ThumbnailVBT.T := NIL; (* the thumbnail widget *)
    findFV: LecternFVClass := NIL;       (* Find dialog *)
    links: MyLinks := NIL;               (* Links dialog *)
    menuFilter: ReactivityVBT.T;         (* filter to make menus passive *)
    toolbarFilter: ReactivityVBT.T;      (* filter to make toolbar passive *)
    viewingFilter: ReactivityVBT.T;      (* filter to make viewing passive *)
    tileFilter: ReactivityVBT.T;         (* filter to make tiles passive *)
    printFile: TEXT;                     (* name of temp file for printing *)
  (* The following fields are protected by VBT.mu *)
    closed: BOOLEAN := FALSE;            (* viewer is closed and moribund *)
    nextV, prevV: Lectern := NIL;        (* ring of existing viewers *)
    actionWindow: VBT.T := NIL;          (* window of last user interaction *)
    dropShadow := TRUE;                  (* use drop shadow if image fits *)
    animationSpeed := DefaultAnimation;  (* msecs per pixel for MoveTo *)
    highlightMotion := DefaultHighlight; (* highlight during MoveTo *)
    paintChunk := DefaultPaintChunk;     (* painting granularity for image *)
    allowPrintImages := FALSE;           (* allow display of print images *)
    worker: Thread.T := NIL;             (* thread forked by ApplyOp *)
    showingError := FALSE;               (* whether error dlg is showing *)
    isPassive: BOOLEAN := TRUE;          (* no new operations allowed *)
    response := Op.Null;                 (* response to modal wait *)
    responseTime: VBT.TimeStamp;         (* event time of response *)
    responseEvent: AnyEvent.T := NIL;    (* event for the response *)
    responseCV: Thread.Condition;        (* wait here for a response *)
    hideMenu := FALSE;                   (* hide menu anchor *)
    zoomed := FALSE;                     (* .iw is in "zoomed" state *)
    unzoomedDomain: Rect.T;              (* last unzoomed domain *)
    unzoomedNW: Point.T;                 (* last unzoomed window position *)
    lasso := Rect.T{west := 0, east := 1,
                    north := 0, south := 1}; (* viewarea *)
    lassoDomain := Rect.T{west := 0, east := 1, north := 0, south := 1};
    autoViewArea := TRUE;                (* automatic viewArea calculation *)
    server: ServerClosure := NIL;        (* server for LecternClient *)
    clientRequest: REF ARRAY OF TEXT := NIL; (* pending request from server *)
    clientError: TEXT := NIL;            (* pending error from server *)
    reshapeWanted := FALSE;              (* pending Op.Reshape *)
    closeWanted := FALSE;                (* pending Op.Close *)
    spare1, spare2, spare3: ImageRd.T := NIL; (* idle images to save storage *)
    tileZoomed := AllTilesNormal;        (* links tiling arrangement*)
    bookmark, diary, outline: Links.LinkList := NIL;
  (* The following fields are writeable under VBT.mu + SELF, and readable
     under either lock. *)
    path: TEXT := NIL;                   (* Currently open document, or NIL *)
    rd: Rd.T;                            (* Currently open document, if any *)
    dir: LecternDoc.Dir;                 (* Currently open document, if any *)
    ocr: LecternOCR.T;                   (* object for accessing OCR data *)
    find: Find.T;                        (* object for searches and queries *)
    hasPrintImages := FALSE;             (* Document has print images *)
    hasOriginal := FALSE;                (* Document has original PS *)
    modified := FALSE;                   (* whether doc has changed *)
    imageTitle: TEXT := NIL;             (* title for image window *)
    imageTitleBase: TEXT := NIL;         (* ditto without any server suffix *)
    thumbnailTitle: TEXT := NIL;         (* title for thumbnail window *)
    pm: ImageRd.T := NIL;                (* image for .page, or NIL *)
    prev: ImageRd.T := NIL;              (* image for .page-1, or NIL *)
    next: ImageRd.T := NIL;              (* image for .page+1, or NIL *)
    part := Partition.VerThenHor;        (* current partitioning scheme *)
    page: PageNumber := -1;              (* current page *)
    class: LecternDoc.Class;             (* current image class *)
    undo, redo: Undo := NIL;             (* undo/redo linked lists *)
  METHODS
    beep() := Beep;
      (* LL = VBT.mu *)
      (* Flashes the screen, and flushes type-ahead *)
    error(msg: TEXT) := Error;
      (* LL = VBT.mu *)
      (* Pops up error dialog, and flushes type-ahead *)
    passive(bePassive: BOOLEAN) := Passive;
      (* LL = VBT.mu *)
      (* Makes everything except popup passive; tolerates initial state
         where VBT's haven't yet been created. *)
    doModalLocked(VAR time: VBT.TimeStamp): Op := DoModalLocked;
      (* LL = VBT.mu *)
      (* Blocks until .applyOp wakes it up, which happens when .applyOp
         gets called with an Op in ResponseOps.  Returns the op and time *)
    doModal(VAR time: VBT.TimeStamp): Op := DoModal;
      (* LL = lect *)
      (* Calls doModalLocked under VBT.mu *)
    doModalPopup(name: TEXT;
                 focus: TEXT := NIL;
                 replaceMode := FALSE;
                 VAR time: VBT.TimeStamp): Op := DoModalPopup;
      (* LL = lect *)
      (* Pops up given named popup, and uses .doModal to get a response,
         then pops down the dialog and returns the response Op and time. *)
    confirm(msg, yes, no: TEXT; VAR time: VBT.TimeStamp): BOOLEAN := Confirm;
      (* LL = lect *)
      (* Uses .doModalPopup with ConfirmDlg to block until we get a
         boolean response (and event time) from the user. *)
    freeImage(VAR image: ImageRd.T) := FreeImage;
      (* LL = VBT.mu *)
      (* Iff image is non-NIL, saves image as SELF.spare* and sets it to nil *)
    createViewer(link: Links.Link;
                 time: VBT.TimeStamp): Lectern := CreateViewer;
      (* LL = VBT.mu *)
      (* Create a new viewer to show "link" *)
    initInstance(link: Links.Link) := InitInstance;
      (* LL = SELF *)
      (* Called for Op.Init, only; and only from within ForkedOp *)
      (* Creates VBT's and calls .readDoc *)
    normalize(delta: Point.T): Point.T := Normalize;
      (* LL = VBT.mu+SELF *)
      (* Returns point closest to "delta" consistent with that being
         a position in the current parititioning scheme. *)
    usePart(part: Partition) := UsePart;
      (* LL = VBT.mu+SELF *)
      (* Switches UI to given partitioning scheme; doesn't move image. *)
    linkToHere(): Links.Link := LinkToHere;
      (* LL = VBT.mu+SELF *)
      (* Calls Links.LinkToHere with current fields of SELF *)
    gotoPage(page: PageNumber; class: LecternDoc.Class; undoLink: Links.Link;
             noisy := TRUE): BOOLEAN := GotoPage;
      (* LL = VBT.mu+SELF *)
      (* Acquires and installs the image for given page and class, taking
         advantage of existing data and updating auxiliary data structures.
         Writes an undo record if this call changes the page or class, and
         NIL's the redo list. Fails silently if not "noisy". Note
         that the locking level allows a subsequent repositioning without
         releasing VBT.mu, thus avoiding extra repaints.  Returns TRUE
         iff it succeeded. *)
    gotoPageAuto(page: PageNumber;
                 class: LecternDoc.Class;
                 part: Partition) := GotoPageAuto;
      (* LL = SELF *)
      (* Goto the given page, class and partitioning scheme. If this
         is a different page, position to the start of the page; otherwise
         normalize the current position to be valid with the new partitioning
         scheme. *)
    gotoSelection(undoLink: Links.Link)
                  RAISES { Thread.Alerted } := GotoSelection;
      (* LL = VBT.mu+SELF *)
      (* Ensures selection is visible, perhaps moving pages. Moves to beginning
         if there is no selection. *)
    gotoPageStart(page: PageNumber; noisy := TRUE) := GotoPageStart;
      (* LL = SELF *)
      (* Displays first sector of given page. Fails silently if not "noisy" *)
    gotoPageEnd(page: PageNumber; noisy := TRUE) := GotoPageEnd;
      (* LL = SELF *)
      (* Displays last sector of given page. Fails silently if not "noisy" *)
    nextSector() := NextSector;
      (* LL = SELF *)
      (* Displays next sector, perhaps moving to next page *)
    prevSector() := PrevSector;
      (* LL = SELF *)
      (* Displays previous sector, perhaps moving to previous page *)
    computeViewArea() := ComputeViewArea;
      (* LL = VBT.mu *)
      (* Compute interesting area of current page *)
    setAutoViewArea(auto: BOOLEAN) := SetAutoViewArea;
      (* LL = VBT.mu *)
      (* Switch to or from automatic view area calculation, updating buttons *)
    print(VAR time: VBT.TimeStamp) := Print;
      (* LL = SELF *)
      (* Prints or saves images or PostScript, depending on options in dlg *)
    closeDoc(VAR time: VBT.TimeStamp): BOOLEAN := CloseDoc;
      (* Called whenever about to close a document.  If document was
         modified, asks user to confirm discard.  Returns TRUE iff it's OK
         to proceed with the close.  Before returning TRUE, appends a
         bookmark link describing the current page of the current document. *)
    readDoc(link: Links.Link;
            time: VBT.TimeStamp;
            from, for: INTEGER := -1) := ReadDoc;
      (* LL = SELF *)
      (* Reads the document and displays initial page.  If from>=0, selects
         the word range [from..from+for) and displays page containing from. *)
    request(request: REF ARRAY OF TEXT; error: TEXT;
            time: VBT.TimeStamp) := Request;
      (* Services a LecternClient request or error message *)
    saveDoc(VAR time: VBT.TimeStamp) := SaveDoc;
      (* Save modified document, and re-open using new file. *)
      (* LL = SELF *)
    close(VAR time: VBT.TimeStamp) := Close;
      (* Close viewer *)
      (* LL = SELF *)
    applyOp(op: Op; ts: VBT.TimeStamp; event: AnyEvent.T := NIL) := ApplyOp;
      (* LL = VBT.mu *)
      (* Performs operation, perhaps by forking a thread.  The UI for
         this viewer is made passive for the entire duration of the
         operation if it is forked. *)
  END;


(* *)
(* Minor VBT subroutines *)
(* *)

TYPE CenterReshape = ZSplit.ReshapeControl OBJECT
  OVERRIDES
    apply := MaintainCenter;
  END;

PROCEDURE EnsureVisible(c, p: Rect.T): Rect.T =
  (* LL = any *)
  (* Ensure that "c" is reasonably visible in "p".  More precisely: return
     a rectangle "r" congruent to "c", and closest to "c", such that r.north
     is in [p.north..p.south), and such that the middle of "c" is as nearly
     as possible within "p" *)
  BEGIN
    WITH r1 = Rect.Center(c, Rect.Project(p, Rect.Middle(c))) DO
      RETURN Rect.Add(r1, Point.T{h := 0,
                                  v := MAX(0, p.north-r1.north)});
    END;
  END EnsureVisible;

PROCEDURE MaintainCenter(<*UNUSED*> self: CenterReshape;
                         <*UNUSED*> ch: VBT.T;
                         READONLY old, new, prev: Rect.T): Rect.T =
  (* LL.sup = VBT.mu.ch *)
  (* Maintain delta between centers, subject to keeping visible *)
  BEGIN
    RETURN EnsureVisible(Rect.Center(prev,
                                     Point.Add(Rect.Middle(prev),
                                               Point.Sub(Rect.Middle(new),
                                                         Rect.Middle(old)))),
                         new);
  END MaintainCenter;

PROCEDURE TransformRect(r, from, to: Rect.T): Rect.T =
    (* LL = any *)
    (* Let T be the linear coordinate transformation such that T(from) = to.
       Returns T(r). *)
    (* Not the most efficient implementation, but simple to understand *)
    (* T is ( x' = x * hScale + hDelta, y' = y * vScale + vDelta ) *)
  VAR
    hScale := FLOAT(Rect.HorSize(to)) / FLOAT(Rect.HorSize(from));
    vScale := FLOAT(Rect.VerSize(to)) / FLOAT(Rect.VerSize(from));
    hDelta := FLOAT(to.west) - FLOAT(from.west) * hScale;
    vDelta := FLOAT(to.north) - FLOAT(from.north) * vScale;
  BEGIN
    RETURN Rect.FromEdges(w := ROUND(FLOAT(r.west) * hScale + hDelta),
                          e := ROUND(FLOAT(r.east) * hScale + hDelta),
                          n := ROUND(FLOAT(r.north) * vScale + vDelta),
                          s := ROUND(FLOAT(r.south) * vScale + vDelta));
  END TransformRect;

PROCEDURE ShowWindow(w, near: VBT.T; title: TEXT): BOOLEAN =
    (* LL = VBT.mu *)
    (* Ensure "w" is visible; if installing, place it near "near". *)
  VAR soRec := Trestle.ScreenOf(w, Point.Origin);
  BEGIN
    TRY
      IF soRec.trsl = NIL THEN
        Trestle.Attach(w);
        Trestle.Decorate(v := w,
                         applName := AppName,
                         windowTitle := title,
                         iconTitle := title);
      END;
      IF soRec.id = Trestle.NoScreen THEN
        Trestle.MoveNear(w, near);
      ELSE
        Trestle.MoveNear(w, NIL);
      END;
    EXCEPT TrestleComm.Failure =>
      RETURN FALSE;
    END;
    RETURN TRUE
  END ShowWindow;

PROCEDURE Retitle(w: VBT.T; title: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    TRY
      Trestle.Decorate(v := w, windowTitle := title, iconTitle := title);
    EXCEPT
    | TrestleComm.Failure =>
    END;
  END Retitle;

PROCEDURE HideWindow(w: VBT.T) =
    (* LL = VBT.mu *)
  VAR soRec := Trestle.ScreenOf(w, Point.Origin);
  BEGIN
    IF soRec.trsl # NIL THEN Trestle.Delete(w) END;
  END HideWindow;

VAR zoomWindowManagerDeltaKludge: Point.T := Point.Origin;
  (* Amount by which Trestle.Overlap mis-positions us. *)

PROCEDURE ZoomWindow(w: VBT.T; VAR nw: Point.T): BOOLEAN =
    (* LL = VBT.mu *)
  VAR
    domainNW := Rect.NorthWest(VBT.Domain(w));
    soRec := Trestle.ScreenOf(w, domainNW);
  BEGIN
    TRY
      nw := soRec.q;
      StableVBT.SetShape(VBT.Parent(w),
                         Rect.HorSize(soRec.dom),
                         Rect.VerSize(soRec.dom));
      Trestle.Overlap(w, soRec.id, Point.Sub(Rect.NorthWest(soRec.dom),
                                             zoomWindowManagerDeltaKludge));
    EXCEPT TrestleComm.Failure =>
      RETURN FALSE;
    END;
    RETURN TRUE
  END ZoomWindow;

PROCEDURE ImageFromRsrc(name: TEXT; path: Rsrc.Path; op: PaintOp.T): ImageRd.T
                       RAISES { Thread.Alerted } =
    (* LL = any *)
  <* FATAL Rsrc.NotFound, Rd.Failure *>
  VAR rd := Rsrc.Open(name, path);
  BEGIN
    RETURN NEW(ImageRd.T).init(rd, 0, Rd.Length(rd), op)
  END ImageFromRsrc;

PROCEDURE CursorFromRsrc(name1, name2: TEXT;
                         hotspot: Point.T;
                         path: Rsrc.Path): Cursor.T
                         RAISES { Thread.Alerted } =
  <* FATAL Rsrc.NotFound, Rd.Failure, Images.Error *>
  VAR
    rd1 := Rsrc.Open(name1, path);
    rd2 := Rsrc.Open(name2, path);
    contents1: Images.RawContents :=
                       NEW(ImageRd.T).init(rd1, 0, Rd.Length(rd1)).contents();
    contents2: Images.RawContents :=
                       NEW(ImageRd.T).init(rd2, 0, Rd.Length(rd1)).contents();
    raw := Cursor.Raw{
       plane1 := contents1.raw,
       plane2 := contents2.raw,
       hotspot := hotspot,
       color1 := Cursor.RGB{r := 0.0, g := 0.0, b := 0.0,
                            gray := 0.0,
                            bw := Cursor.BW.UseFg},
       color2 := Cursor.RGB{r := 1.0, g := 1.0, b := 1.0,
                            gray := 1.0,
                            bw := Cursor.BW.UseBg},
       color3 := Cursor.RGB{r := 1.0, g := 1.0, b := 1.0,
                            gray := 1.0,
                            bw := Cursor.BW.UseBg}};
  BEGIN
    RETURN Cursor.FromRaw(raw)
  END CursorFromRsrc;

PROCEDURE MakeActive(fv: FormsVBT.T; name: TEXT; c := Cursor.TextPointer) =
    (* LL = VBT.mu *)
    (* Make item active with given cursor *)
  BEGIN
    FormsVBT.MakeActive(fv, name);
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), c);
  END MakeActive;

<*UNUSED*>
PROCEDURE MakePassive(fv: FormsVBT.T; name: TEXT; c := Cursor.NotReady) =
    (* LL = VBT.mu *)
    (* make item passive with given cursor *)
  BEGIN
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), c);
    FormsVBT.MakePassive(fv, name);
  END MakePassive;

PROCEDURE MakeDormant(fv: FormsVBT.T; name: TEXT; c := Cursor.DontCare) =
    (* LL = VBT.mu *)
    (* make itme dormant with given cursor *)
  BEGIN
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), c);
    FormsVBT.MakeDormant(fv, name);
  END MakeDormant;

PROCEDURE SetTileColumnShape(lect: Lectern; range: FlexVBT.SizeRange) =
  VAR
    flex: FlexVBT.T;
  BEGIN
    IF lect.tileFV # NIL THEN
      flex := FormsVBT.GetVBT(lect.tileFV, "LinkTileShape");
      FlexVBT.SetRange(flex, Axis.T.Hor, range);
    END;
  END SetTileColumnShape;

PROCEDURE SetTileShape(lect: Lectern; tile: Tile; range: FlexVBT.SizeRange) =
  VAR
    flex: FlexVBT.T;
  BEGIN
    IF lect.tileFV # NIL THEN
      flex := FormsVBT.GetVBT(lect.tileFV, TileShape[tile]);
      FlexVBT.SetRange(flex, Axis.T.Ver, range);
    END;
  END SetTileShape;


(* *)
(* Running Sub-processes *)
(* *)

PROCEDURE Tokenise(line: TEXT): REF ARRAY OF TEXT RAISES { Thread.Alerted } =
  (* Break "line" into words *)
  <*FATAL Rd.EndOfFile, Rd.Failure*>
  VAR
    rd := TextRd.New(line);
    prevC, c: CHAR;
    argv: REF ARRAY OF TEXT := NIL;
    wr: Wr.T;
    numTokens := 0;
  BEGIN
    prevC := ' ';
    WHILE NOT Rd.EOF(rd) DO
      c := Rd.GetChar(rd);
      IF c # ' ' AND prevC = ' ' THEN INC(numTokens) END;
      prevC := c;
    END;
    argv := NEW(REF ARRAY OF TEXT, numTokens);
    Rd.Seek(rd, 0);
    c := Rd.GetChar(rd);
    FOR i := 0 TO numTokens-1 DO
      WHILE c = ' ' DO c := Rd.GetChar(rd) END;
      wr := TextWr.New();
      LOOP
        Wr.PutChar(wr, c);
        IF Rd.EOF(rd) THEN EXIT END;
        c := Rd.GetChar(rd);
        IF c = ' ' THEN EXIT END;
      END;
      argv[i] := TextWr.ToText(wr);
    END;
    Rd.Close(rd);
    RETURN argv
  END Tokenise;

PROCEDURE RunSub(READONLY argv: ARRAY OF TEXT): TEXT
    RAISES {OSError.E, Rd.Failure, Thread.Alerted} =
    (* Run a command as a sub-process, returning its output as a text *)
  VAR
    hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
    wr: Wr.T;
    rd: Rd.T;
    child: Process.T;
    output: TEXT;
    rc: INTEGER;
  BEGIN
    Pipe.Open(hr := hrChild, hw := hwSelf);
    wr := NEW(FileWr.T).init(hwSelf);
    Pipe.Open(hr := hrSelf, hw := hwChild);
    rd := NEW(FileRd.T).init(hrSelf);
    child := Process.Create(argv[0],
                            SUBARRAY(argv, 1, NUMBER(argv)-1),
                            NIL, NIL,
                            hrChild, hwChild, hwChild);
    hrChild.close();
    hwChild.close();
    Wr.Close(wr);
    output := Rd.GetText(rd, LAST(CARDINAL));
    Rd.Close(rd);
    rc := Process.Wait(child);
    IF rc # 0 THEN
      RETURN "Exit status " & Fmt.Int(rc);
    ELSE
      RETURN output;
    END;
  END RunSub;


(* *)
(* Progress Widget *)
(* *)

TYPE ProgressVBT = VBT.Leaf OBJECT
    state: REAL := 0.0; (* LL = VBT.mu *)
  METHODS
    set(new: REAL) := ProgressSet; (* LL = VBT.mu *)
  OVERRIDES
    repaint := ProgressRepaint;
  END;

PROCEDURE ProgressRepaint(v: ProgressVBT; <*UNUSED*> READONLY rgn: Region.T) =
    (* LL.sup = mu.SELF *)
  VAR
    free := VBT.Domain(v);
    width := ROUND(FLOAT(Rect.HorSize(free)) * v.state);
    used := free;
  BEGIN
    used.east := used.west + width;
    free.west := used.east;
    VBT.PaintTint(v, used, PaintOp.Fg);
    VBT.PaintTint(v, free, PaintOp.Bg);
  END ProgressRepaint;

PROCEDURE ProgressSet(v: ProgressVBT; new: REAL) =
    (* LL = VBT.mu *)
  BEGIN
    v.state := new;
    VBT.Mark(v);
  END ProgressSet;


(* *)
(* Server thread for LecternClient *)
(* *)

TYPE
  ServerClosure = Thread.Closure OBJECT
      (* All synchronized by VBT.mu, for simplicity *)
    lect: Lectern;
  OVERRIDES
    apply := RunServer;
  END;

PROCEDURE RunServer(self: ServerClosure): REFANY =
  VAR
    newRequest: REF ARRAY OF TEXT;
    newError: TEXT;
  BEGIN
    WHILE TRUE DO
      TRY
        newRequest := NIL; newError := NIL;
        newRequest := LecternServer.AwaitRequest();
      EXCEPT LecternServer.Error(t) =>
        newError := t;
      END;
      (* Caution: self.lect can change whenever VBT.mu is unlocked, if our
         current viewer does Op.Detach. *)
      LOCK VBT.mu DO
        (* Even if there's an Op.Request in progress, we update the fields
           of self.lect, since a newer request is always better. *)
        self.lect.clientRequest := newRequest;
        self.lect.clientError := newError;
        (* We need to go through clientMiscCode to get an event time.  However,
           it's remotely possible the self.lect.iw isn't initialized yet
           because self.lect is still executing Op.Init.  In that case
           the code at the end of ForkedOp will notice the pending request and
           cause the clientMiscCode event. *)
        IF self.lect.worker = NIL THEN
          TRY
            VBT.Forge(self.lect.iw, self.lect.clientMiscCode);
          EXCEPT VBT.Error =>
            (* Uninstalled.  I can't imagine what we could do here. *)
          END;
        END;
      END;
      IF newError # NIL THEN EXIT END;
    END;
    RETURN NIL
  END RunServer;


(* *)
(* KeyFilter methods *)
(* *)

PROCEDURE KeyFilterPassive(v: LectFilterVBT; bePassive: BOOLEAN) =
    (* LL = VBT.mu *)
  BEGIN
    v.isPassive := bePassive;
    WHILE NOT v.isPassive AND v.typeAhead # NIL AND v.typeAhead.size() > 0 DO
      WITH cd = NARROW(v.typeAhead.remlo(), AnyEvent.Key).key DO
        IF v.childFocus # NIL THEN
          VBTClass.Key(v.childFocus, cd);
        ELSE
          v.key(cd);
        END;
      END;
    END;
  END KeyFilterPassive;

PROCEDURE KeyFilterFlush(v: LectFilterVBT) =
    (* LL = VBT.mu *)
  BEGIN
    v.typeAhead := NIL;
  END KeyFilterFlush;

PROCEDURE KeyFilterReshape(v: LectFilterVBT; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF v = v.lect.iw THEN
      SetTileColumnShape(v.lect, FlexVBT.FixedRange);
    END;
    StableVBT.T.reshape(v, cd);
  END KeyFilterReshape;

PROCEDURE KeyFilterMisc(v: LectFilterVBT; READONLY cd: VBT.MiscRec) =
    (* LL = VBT.mu *)
  BEGIN
    IF (cd.type = VBT.TakeSelection) AND (cd.selection = VBT.KBFocus) THEN
      IF v.childFocus = NIL THEN
        v.takeFocus(cd.time);
      ELSE
        VBTClass.Misc(v.childFocus, cd);
      END
    ELSIF cd.type = VBT.Lost AND cd.selection = VBT.KBFocus THEN
      (* It's for us - don't pass it down to our children *)
    ELSIF cd.type = v.lect.clientMiscCode THEN
        IF v.lect.worker = NIL THEN v.lect.applyOp(Op.Request, cd.time) END;
        (* Otherwise, the request will be dealt with at the end of ForkedOp,
           by creating another clientMiscCode event.   The code in ForkedOp
           (at Op.Request and at the end) allows for the slim possibility that
            our request has been serviced by the current worker thread. *)
    ELSE
      IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
        IF (NOT v.lect.closed) AND v = v.lect.iw THEN
          v.lect.closeWanted := TRUE;
          IF v.lect.worker = NIL THEN v.lect.applyOp(Op.Close, cd.time) END;
        END;
      END;
      StableVBT.T.misc(v, cd);
    END;
  END KeyFilterMisc;

PROCEDURE KeyFilterMouse(v: LectFilterVBT; READONLY cd: VBT.MouseRec) =
    (* LL = VBT.mu *)
  BEGIN
    IF v = v.lect.iw THEN
      SetTileColumnShape(v.lect, FlexVBT.DefaultRange);
    END;
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF v.childFocus = NIL THEN v.takeFocus(cd.time) END;
    END;
    StableVBT.T.mouse(v, cd);
  END KeyFilterMouse;

PROCEDURE KeyFilterKey(v: LectFilterVBT; READONLY cd: VBT.KeyRec) =
    (* LL = VBT.mu *)
  VAR
    sameKey := (cd.whatChanged = v.prevKeyDown);
  BEGIN
    IF NOT cd.wentDown THEN
      v.prevKeyDown := VBT.NoKey; (* no longer auto-repeating *)
    ELSE
      v.prevKeyDown := cd.whatChanged;
      v.lect.actionWindow := v;
      IF v.isPassive THEN
        IF cd.whatChanged = KeyboardKey.Escape THEN
          v.lect.beep(); (* also flushes *)
        ELSIF sameKey THEN
          (* auto-repeat type-ahead: discard *)
        ELSE
          IF v.typeAhead = NIL THEN v.typeAhead := NEW(RefSeq.T).init(10) END;
          v.typeAhead.addhi(AnyEvent.FromKey(cd));
        END;
      ELSE
        CASE cd.whatChanged OF
          | KeyboardKey.Shift_L, KeyboardKey.Shift_R,
            KeyboardKey.Control_L,  KeyboardKey.Control_R,
            KeyboardKey.Caps_Lock, KeyboardKey.Shift_Lock,
            KeyboardKey.Meta_L, KeyboardKey.Meta_R,
            KeyboardKey.Alt_L, KeyboardKey.Alt_R,
            KeyboardKey.Super_L, KeyboardKey.Super_R,
            KeyboardKey.Hyper_L, KeyboardKey.Hyper_R  => (* ignore *)
          | KeyboardKey.Return, KeyboardKey.KP_Enter =>
              IF v.lect.worker # NIL THEN
                v.lect.applyOp(Op.Yes, cd.time);
              ELSIF v.lect.findFV # NIL AND ZSplit.IsMapped(v.lect.findFV) THEN
                v.lect.applyOp(Op.FindNext, cd.time);
              ELSE
                v.lect.applyOp(Op.Null, cd.time);
              END;
          | KeyboardKey.Escape =>
              IF v.lect.worker # NIL THEN
                v.lect.applyOp(Op.No, cd.time);
              ELSE
                v.lect.applyOp(Op.Null, cd.time);
              END;
          | Latin1Key.question => v.lect.applyOp(Op.ShowHelp, cd.time);
          | KeyboardKey.Next, Latin1Key.plus, Latin1Key.equal =>
              v.lect.applyOp(Op.NextPage, cd.time);
          | KeyboardKey.Prior,  Latin1Key.minus, Latin1Key.underscore =>
              v.lect.applyOp(Op.PrevPage, cd.time);
          | Latin1Key.less, Latin1Key.comma =>
              v.lect.applyOp(Op.Smaller, cd.time);
          | KeyboardKey.End => v.lect.applyOp(Op.LastPage, cd.time);
          | Latin1Key.greater, Latin1Key.period =>
              v.lect.applyOp(Op.Larger, cd.time);
          | Latin1Key.space,
            KeyboardKey.Right, KeyboardKey.Down =>
              v.lect.applyOp(Op.NextSector, cd.time);
          | KeyboardKey.BackSpace, KeyboardKey.Delete,
            KeyboardKey.Left, KeyboardKey.Up =>
              v.lect.applyOp(Op.PrevSector, cd.time);
          | Latin1Key.zero .. Latin1Key.nine =>
              WITH c = VAL(ORD('0')+cd.whatChanged-Latin1Key.zero, CHAR) DO
                FormsVBT.PutText(v.lect.popupFV, "GotoCount",
                                 Text.FromChar(c));
              END;
              v.lect.applyOp(Op.GotoPage, cd.time);
          | Latin1Key.A, Latin1Key.a =>
              v.lect.applyOp(Op.AutoViewArea, cd.time);
          | Latin1Key.B, Latin1Key.b =>
              v.lect.applyOp(Op.FirstPage, cd.time);
          | Latin1Key.C, Latin1Key.c =>
              v.lect.applyOp(Op.ContentsPage, cd.time);
          | Latin1Key.D, Latin1Key.d =>
              v.lect.applyOp(Op.Detach, cd.time);
          | Latin1Key.E, Latin1Key.e =>
              v.lect.applyOp(Op.EditLink, cd.time);
          | Latin1Key.F, Latin1Key.f =>
              v.lect.applyOp(Op.ShowFind, cd.time);
          | KeyboardKey.Home,
            Latin1Key.H, Latin1Key.h =>
              IF VBT.Modifier.Shift IN cd.modifiers AND
                 VBT.Modifier.Control IN cd.modifiers THEN
                (* Undocumented feature to flip moveto highlight *)
                v.lect.highlightMotion := NOT v.lect.highlightMotion;
              ELSE
                v.lect.applyOp(Op.Home, cd.time);
              END;
          | Latin1Key.I, Latin1Key.i =>
              IF VBT.Modifier.Shift IN cd.modifiers AND
                 VBT.Modifier.Control IN cd.modifiers THEN
                (* Undocumented feature to flip incremental repaint *)
                v.lect.paintChunk := IncrementalPaint - v.lect.paintChunk;
                IF v.lect.pm # NIL THEN
                  v.lect.iv.put(v.lect.pm, v.lect.bg,
                                v.lect.dropShadow,
                                v.lect.paintChunk);
                END;
              ELSE
                v.lect.applyOp(Op.IndexPage, cd.time);
              END;
          | Latin1Key.J, Latin1Key.j =>
              IF VBT.Modifier.Shift IN cd.modifiers AND
                 VBT.Modifier.Control IN cd.modifiers THEN
                (* Undocumented feature to flip jump scrolling *)
                IF v.lect.animationSpeed = 0.0 THEN
                  v.lect.animationSpeed := SmoothAnimation;
                ELSE
                  v.lect.animationSpeed := 0.0;
                END;
              ELSE
                v.lect.applyOp(Op.JumpLink, cd.time);
              END;
          | Latin1Key.K, Latin1Key.k =>
              v.lect.applyOp(Op.ShowHideMenu, cd.time);
          | Latin1Key.L, Latin1Key.l =>
              v.lect.applyOp(Op.ShowHideLinks, cd.time);
          | Latin1Key.M, Latin1Key.m =>
              v.lect.applyOp(Op.OpenCloseMenu, cd.time);
          | Latin1Key.N, Latin1Key.n =>
              v.lect.applyOp(Op.NewLink, cd.time);
          | Latin1Key.O, Latin1Key.o => v.lect.applyOp(Op.Open, cd.time);
          | Latin1Key.P, Latin1Key.p =>
              IF VBT.Modifier.Shift IN cd.modifiers AND
                 VBT.Modifier.Control IN cd.modifiers THEN
                (* Undocumented feature to flip display of print images *)
                v.lect.allowPrintImages := NOT v.lect.allowPrintImages;
              ELSE
                v.lect.applyOp(Op.Print, cd.time);
              END;
          | Latin1Key.Q, Latin1Key.q => v.lect.applyOp(Op.Quit, cd.time);
          | Latin1Key.R, Latin1Key.r => v.lect.applyOp(Op.Redo, cd.time);
          | Latin1Key.S, Latin1Key.s =>
              IF VBT.Modifier.Shift IN cd.modifiers AND
                 VBT.Modifier.Control IN cd.modifiers THEN
                (* Undocumented feature to turn on drop shadows *)
                v.lect.thumbnailVBT.shadow := NOT v.lect.thumbnailVBT.shadow;
                VBT.ForceRepaint(v.lect.thumbnailVBT, Region.Full);
                v.lect.dropShadow := NOT v.lect.dropShadow;
                IF v.lect.pm # NIL THEN
                  v.lect.iv.put(v.lect.pm, v.lect.bg,
                                v.lect.dropShadow,
                                v.lect.paintChunk);
                END;
              ELSE
                v.lect.applyOp(Op.SaveDoc, cd.time);
              END;
          | Latin1Key.T, Latin1Key.t =>
              v.lect.applyOp(Op.ShowHideToolbar, cd.time);
          | Latin1Key.U, Latin1Key.u => v.lect.applyOp(Op.Undo, cd.time);
          | Latin1Key.V, Latin1Key.v =>
              v.lect.applyOp(Op.ShowHideViewing, cd.time);
          | Latin1Key.W, Latin1Key.w => v.lect.applyOp(Op.Close, cd.time);
          | Latin1Key.Z, Latin1Key.z => v.lect.applyOp(Op.Zoom, cd.time);
        ELSE
          v.lect.beep();
        END; (*CASE*)
      END;
    END;
  END KeyFilterKey;

PROCEDURE KeyFilterAcquire(v: LectFilterVBT; ch: VBT.T; w: VBT.T;
                   s: VBT.Selection; ts: VBT.TimeStamp) RAISES{ VBT.Error } =
    (* LL.sup = ch *)
  BEGIN
    StableVBT.T.acquire(v, ch, w, s, ts);
    IF s = VBT.KBFocus THEN LOCK v DO v.childFocus := w END END;
  END KeyFilterAcquire;

PROCEDURE KeyFilterTakeFocus(v: LectFilterVBT; ts: VBT.TimeStamp) =
    (* LL.sup < v *)
  BEGIN
    LOCK v DO
      v.prevKeyDown := VBT.NoKey;
      v.childFocus := NIL;
      TRY VBTClass.Acquire(v, VBT.KBFocus, ts) EXCEPT VBT.Error => END;
    END;
  END KeyFilterTakeFocus;


(* *)
(* Filtered type-ins of various flavors: sub-classes and filter methods *)
(* *)

TYPE FilteredTypeIn = FVTypes.FVTypeIn OBJECT
    fv: LecternFVClass; (* get .lect from here, because it can change
                           if we're re-installed in another viewer *)
    name: TEXT;
  OVERRIDES
    key := FilteredTypeInKey;
  END;

TYPE FilteredHelper = MyFileBrowserVBT.Helper OBJECT
    fv: LecternFVClass;
    name: TEXT;
  OVERRIDES
    key := FilteredHelperKey;
  END;

TYPE FilteredNumeric = NumericVBT.Typein OBJECT
    fv: LecternFVClass;
    name: TEXT;
  OVERRIDES
    key := FilteredNumericKey;
  END;

PROCEDURE FilteredTypeInKey(v: FilteredTypeIn; READONLY cd: VBT.KeyRec) =
    (* LL = VBT.mu *)
    (* Interpret various keys specially, for particular type-ins. *)
  VAR
    lect := v.fv.lect;
  BEGIN
    IF cd.wentDown THEN
      IF Text.Equal(v.name, "FindTxt") THEN
        IF cd.whatChanged = KeyboardKey.Escape THEN
          (* ESC in non-modal: Op.Null will close non-modal dialogs *)
          lect.applyOp(Op.Null, cd.time); RETURN;
        END;
      ELSE
        IF cd.whatChanged = KeyboardKey.Escape THEN
          (* ESC in modal: equivalent to "Cancel" *)
          lect.applyOp(Op.No, cd.time); RETURN;
        END;
        IF Text.Equal(v.name, "GotoCount") THEN
          (* Additional keyboard shortcuts in "goto" dialog *)
          CASE cd.whatChanged OF
          | Latin1Key.plus, Latin1Key.equal =>
              lect.applyOp(Op.GoForward, cd.time); RETURN;
          | Latin1Key.minus, Latin1Key.underscore =>
              lect.applyOp(Op.GoBackward, cd.time); RETURN;
          ELSE
          END;
        END;
      END;
    END;
    IF Text.Equal(v.name, "LinkName") OR Text.Equal(v.name, "LinkRename") THEN
      lect.links.key(v, cd);
    ELSE
      (* If we didn't handle it, pass it on *)
      FVTypes.FVTypeIn.key(v, cd);
    END;
  END FilteredTypeInKey;

PROCEDURE FilteredHelperKey(v: FilteredHelper; READONLY cd: VBT.KeyRec) =
    (* LL = VBT.mu *)
    (* Interpret ESC specially for file dialog helper. *)
  VAR
    lect := v.fv.lect;
  BEGIN
    IF cd.wentDown THEN
      IF cd.whatChanged = KeyboardKey.Escape THEN
        (* ESC in modal: equivalent to "Cancel" *)
        lect.applyOp(Op.No, cd.time); RETURN;
     END;
    END;
    (* If we didn't handle it, pass it on *)
   MyFileBrowserVBT.Helper.key(v, cd);
  END FilteredHelperKey;

PROCEDURE FilteredNumericKey(v: FilteredNumeric; READONLY cd: VBT.KeyRec) =
    (* LL = VBT.mu *)
    (* Interpret ESC specially for file dialog helper. *)
  VAR
    lect := v.fv.lect;
  BEGIN
    IF cd.wentDown THEN
      IF cd.whatChanged = KeyboardKey.Escape THEN
        (* ESC in modal: equivalent to "Cancel" *)
        lect.applyOp(Op.No, cd.time); RETURN;
     END;
    END;
    (* If we didn't handle it, pass it on *)
   NumericVBT.Typein.key(v, cd);
  END FilteredNumericKey;


(* *)
(* ViewArea: sub-class and hit method, and auto-manual switch *)
(* *)

TYPE MyViewAreaVBT = ViewAreaVBT.T OBJECT
    lect: Lectern;
  OVERRIDES
    hit := ViewAreaHit;
  END;

PROCEDURE ViewAreaHit(v: MyViewAreaVBT; time: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  VAR
    lasso :=v.getSelected();
  BEGIN
    v.lect.actionWindow := v.lect.iw;
    IF NOT Rect.IsEmpty(lasso) THEN
      v.lect.lasso := lasso;
      v.lect.lassoDomain := v.get().domain(NIL);
      v.lect.applyOp(Op.ViewAreaHit, time);
    END;
  END ViewAreaHit;


(* *)
(* Image: sub-class for mouse clicks, selections and shape *)
(* *)

TYPE LectImageVBT = ImageVBT.T OBJECT
    lect: Lectern;
  OVERRIDES
    mouse := ImageMouse;
    position := ImagePosition;
    shape := ImageShape;
    reshape := ImageReshape;
    paint := ImagePaint;
    read := ImageRead;
    write := ImageWrite;
  END;

PROCEDURE ImageMouse(v: LectImageVBT; READONLY cd: VBT.MouseRec) =
    (* LL = VBT.mu *)
  BEGIN
    IF v.lect.ocr = NIL THEN
      ImageVBT.T.mouse(v, cd);
    ELSE
      v.lect.ocr.mouse(cd, v.lect.page, v.lect.class);
      IF cd.clickType = VBT.ClickType.LastUp AND cd.clickCount = 3 THEN
        IF cd.whatChanged = VBT.Modifier.MouseL THEN
          v.lect.applyOp(Op.FindNextConfirmed, cd.time);
        ELSE
          TRY
            WITH value = VBT.Read(v.lect.iw, VBT.Source, cd.time) DO
              FormsVBT.PutText(v.lect.popupFV, "GotoCount", value.toRef());
            END;
            v.lect.applyOp(Op.GotoPageConfirmed, cd.time);
          EXCEPT VBT.Error =>
            v.lect.beep();
          END;
        END;
      END;
    END;
  END ImageMouse;

PROCEDURE ImagePosition(v: LectImageVBT; READONLY cd: VBT.PositionRec) =
    (* LL = VBT.mu *)
  BEGIN
    IF v.lect.ocr = NIL THEN
      ImageVBT.T.position(v, cd);
    ELSE
      v.lect.ocr.position(cd, v.lect.page, v.lect.class);
    END;
  END ImagePosition;

PROCEDURE ImageShape(v: LectImageVBT; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
    (* LL = VBT.mu.v *)
  VAR sr := ImageVBT.T.shape(v, ax, n);
  BEGIN
    IF ax = Axis.T.Hor THEN sr.pref := 640 ELSE sr.pref := 480 END;
    sr.lo := 0;
    sr.hi := VBT.DefaultShape.hi;
    RETURN sr
  END ImageShape;

PROCEDURE ImageReshape(v: LectImageVBT; READONLY cd: VBT.ReshapeRec) =
    (* LL = VBT.mu.v *)
  VAR dummy: Point.T;
  BEGIN
    IF v.lect.zoomed AND NOT Rect.IsEmpty(cd.new) THEN
      WITH
        domainNW = Rect.NorthWest(VBT.Domain(v.lect.iw)),
        soRec = Trestle.ScreenOf(v.lect.iw, domainNW),
        error = Point.Sub(soRec.q, Rect.NorthWest(soRec.dom)) DO
        IF error # Point.Origin AND
                              zoomWindowManagerDeltaKludge = Point.Origin THEN
          zoomWindowManagerDeltaKludge := error;
          EVAL ZoomWindow(v.lect.iw, dummy);
        END;
      END;
    END;
    ImageVBT.T.reshape(v, cd);
    v.lect.reshapeWanted := TRUE;
    IF v.lect.worker = NIL THEN v.lect.applyOp(Op.Reshape, 0) END;
    (* Otherwise, current worker will handle the request when it finishes
       whatever it's doing.  There's a potential race if the current
       worker is just starting and is itself doing Op.Reshape, and this
       is handled by the worker. *)
  END ImageReshape;

PROCEDURE ImagePaint(v: LectImageVBT; READONLY rgn: Region.T)
                    RAISES { Thread.Alerted, Images.Error } =
    (* LL.sup = mu.SELF *)
  BEGIN
    IF v.lect.ocr = NIL THEN
      ImageVBT.T.paint(v, rgn);
    ELSE
      v.lect.ocr.paint(rgn, v.lect.page, v.lect.class);
    END;
  END ImagePaint;

PROCEDURE ImageRead(v: LectImageVBT; s: VBT.Selection; tc: CARDINAL): VBT.Value
                    RAISES { VBT.Error } =
  BEGIN
    IF v.lect.ocr = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection);
    ELSE
      WITH t = v.lect.ocr.read(s) DO
        IF tc # TYPECODE(t) THEN
          RAISE VBT.Error(VBT.ErrorCode.WrongType);
        ELSE
          RETURN VBT.FromRef(t)
        END;
      END;
    END;
  END ImageRead;

PROCEDURE ImageWrite(v: LectImageVBT;
                     <*UNUSED*> s: VBT.Selection;
                     <*UNUSED*> value: VBT.Value;
                     <*UNUSED*> tc: CARDINAL) RAISES { VBT.Error } =
  BEGIN
    IF v.lect.ocr = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection);
    ELSE
      RAISE VBT.Error(VBT.ErrorCode.Unwritable);
    END;
  END ImageWrite;


(* *)
(* ThumbnailVBT: sub-class to provide images and handle hits, and shape *)
(* *)

TYPE MyThumbnailVBT = ThumbnailVBT.T OBJECT
    lect: Lectern;
  OVERRIDES
    hit := ThumbnailVBTHit;
  END;

PROCEDURE ThumbnailVBTHit(v: MyThumbnailVBT; n: INTEGER; time: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    IF ThumbnailWindow THEN
      v.lect.actionWindow := v.lect.thumbnailW;
    ELSE
      v.lect.actionWindow := v.lect.iw;
    END;
    IF n >= 0 AND v.lect.dir.pages # NIL AND n <= LAST(v.lect.dir.pages^) THEN
      v.lect.applyOp(Op.ThumbnailHit, time);
    END;
  END ThumbnailVBTHit;

PROCEDURE CreateThumbnailWindow(lect: Lectern) =
    (* LL = VBT.mu *)
    (* Creates thumbnail window. Doesn't install it. *)
  BEGIN
    lect.thumbnailVBT := NEW(MyThumbnailVBT,
                             lect := lect,
                             shadow := TRUE).init(
                                lect.shadow2,
                                Point.T{68, 68},
                                Point.T{5, 5});
    IF ThumbnailWindow THEN
      lect.thumbnailFilter := NEW(ReactivityVBT.T).init(lect.thumbnailVBT);
      lect.thumbnailW := NEW(LectFilterVBT,
                             lect := lect,
                             waitCursor := lect.waitCursor).init(
                                                    lect.thumbnailFilter);
    END;
  END CreateThumbnailWindow;


(* *)
(* Forms *)
(* *)

TYPE FVClosure = FormsVBT.Closure OBJECT
  op: Op;
  OVERRIDES
    apply := FVApply;
  END;

PROCEDURE FVApply(self: FVClosure;
                  fv: FormsVBT.T;
                  name: TEXT;
                  time: VBT.TimeStamp) =
  (* Event handler for form events handled by .applyOp *)
  (* LL = VBT.mu *)
  VAR
    lect := NARROW(fv, LecternFVClass).lect; (* can change from time to time *)
  BEGIN
    lect.actionWindow := lect.iw;
    WITH
      event = FormsVBT.GetTheEvent(fv),
      v = FormsVBT.GetVBT(fv, name) DO
      IF ISTYPE(v, FVTypes.FVNumeric) AND
         ISTYPE(event, AnyEvent.Mouse) THEN
        RETURN; (* up/down buttons *)
      ELSIF Text.Equal(name, "Browser") THEN
        IF lect.links.browserHit() THEN
          lect.applyOp(self.op, time, event);
        END;
      ELSE
        lect.applyOp(self.op, time, event);
      END;
    END;
  END FVApply;

PROCEDURE LecternFVMouse(v: VBT.T; READONLY cd: VBT.MouseRec) =
    (* Mouse filter for floating forms, to bring to top *)
    (* Benign if v isn't a child of a ZSplit *)
    (* LL.sup = VBT.mu *)
  BEGIN
    IF ISTYPE(VBT.Parent(v), ZSplit.T) THEN
      IF cd.clickType = VBT.ClickType.FirstDown THEN
        ZSplit.Lift(v, ZSplit.Altitude.Top);
      END;
    END;
    FormsVBT.T.mouse(v, cd);
  END LecternFVMouse;

TYPE LecternNumeric = FVTypes.FVNumeric OBJECT
    (* Filter to capture the shadow and font during initialization, to
       given to our FileHelper. *)
    lect: Lectern;
  OVERRIDES
    init := LecternNumericInit;
  END;

PROCEDURE LecternNumericInit(v: LecternNumeric;
                             min := FIRST(INTEGER);
                             max := LAST(INTEGER);
                             allowEmpty := FALSE;
                             naked := FALSE;
                             font := Font.BuiltIn;
                             shadow: Shadow.T := NIL): NumericVBT.T =
  BEGIN
    v.lect.shadow := shadow;
    v.lect.font := font;
    RETURN FVTypes.FVNumeric.init(v,
                                  min, max, allowEmpty, naked, font, shadow);
  END LecternNumericInit;

TYPE LecternFrame = FVTypes.FVFrame OBJECT
    (* Filter to capture the shadow during initialization, to
       given to our FileDirMenu. *)
    lect: Lectern;
  OVERRIDES
    init := LecternFrameInit;
  END;

PROCEDURE LecternFrameInit(v: LecternFrame;
                           ch: VBT.T;
                           shadow: Shadow.T := NIL;
                           style := Shadow.Style.Flat): ShadowedVBT.T =
  BEGIN
    v.lect.shadow2 := shadow;
    RETURN FVTypes.FVFrame.init(v, ch, shadow, style);
  END LecternFrameInit;

PROCEDURE LecternFVRealize(fv: LecternFVClass; type, name: TEXT): VBT.T
                           RAISES { FormsVBT.Error } =
  BEGIN
    IF Text.Equal(type, "TypeIn") THEN
      RETURN NEW(FilteredTypeIn, fv := fv, name := name);
    ELSIF Text.Equal(type, "Frame") THEN
      RETURN NEW(LecternFrame, lect := fv.lect);
    ELSIF Text.Equal(type, "Numeric") THEN
      RETURN NEW(LecternNumeric,
                 lect := fv.lect,
                 typein := NEW(FilteredNumeric,
                               fv := fv, name := name));
    ELSE
      RETURN FormsVBT.T.realize(fv, type, name);
    END;
  END LecternFVRealize;

TYPE LecternFileBrowserVBT = MyFileBrowserVBT.T OBJECT
    fv: LecternFVClass;
  OVERRIDES
    activateFile := LecternFileBrowserActivate;
  END;

PROCEDURE LecternFileBrowserActivate(v: LecternFileBrowserVBT;
                                     <*UNUSED*> fileName: TEXT;
                                     event: AnyEvent.T) =
  VAR
    time: VBT.TimeStamp;
    lect := v.fv.lect; (* can change if we're moved to another viewer *)
  BEGIN
    TYPECASE event OF
      | AnyEvent.Key(k) => time := k.key.time;
      | AnyEvent.Mouse(m) => time := m.mouse.time;
    ELSE
      time := 0;
    END;
    lect.applyOp(Op.Yes, time, event);
  END LecternFileBrowserActivate;

PROCEDURE CreateForms(lect: Lectern) RAISES { Thread.Alerted } =
    (* LL = VBT.mu *)
  <*FATAL Rd.Failure*>
  <*FATAL Rsrc.NotFound*>
  VAR
    form: LecternFVClass;
  PROCEDURE Create(name: TEXT) RAISES { Thread.Alerted } =
    BEGIN
      form := NEW(LecternFVClass,
                  lect := lect).initFromRsrc(name, lect.rsrcPath, TRUE);
    END Create;
  PROCEDURE Attach(name: TEXT; op: Op) =
    BEGIN
      FormsVBT.Attach(form, name, NEW(FVClosure, op := op));
    END Attach;
  BEGIN
    (* First, create all the forms.  Then assemble them. *)
    (* Menu window *)
    Create("menu.fv");
    form.firstPopup := FALSE;
    lect.menuFilter := FormsVBT.GetVBT(form, "MenuFilter");
    Attach("ShowMenu", Op.OpenCloseMenu);
    Attach("HideMenu", Op.OpenCloseMenu);
    Attach("About", Op.About);
    Attach("Help", Op.ShowHelp);
    Attach("Open", Op.Open);
    Attach("Close", Op.Close);
    Attach("Detach", Op.Detach);
    Attach("SaveDoc", Op.SaveDoc);
    Attach("Print", Op.Print);
    Attach("Quit", Op.Quit);
    Attach("NewLink", Op.NewLink);
    Attach("EditLink", Op.EditLink);
    Attach("JumpLink", Op.JumpLink);
    Attach("NextSector", Op.NextSector);
    Attach("PrevSector", Op.PrevSector);
    Attach("NextPage", Op.NextPage);
    Attach("PrevPage", Op.PrevPage);
    Attach("FirstPage", Op.FirstPage);
    Attach("ContentsPage", Op.ContentsPage);
    Attach("IndexPage", Op.IndexPage);
    Attach("GotoPage", Op.GotoPage);
    Attach("Home", Op.Home);
    Attach("Find", Op.ShowFind);
    Attach("ShowHideMenu", Op.ShowHideMenu);
    Attach("OpenCloseMenu", Op.OpenCloseMenu);
    Attach("ShowHideToolbar", Op.ShowHideToolbar);
    Attach("ShowHideViewing", Op.ShowHideViewing);
    Attach("ShowHideLinks", Op.ShowHideLinks);
    Attach("Zoom", Op.Zoom);
    Attach("Smaller", Op.Smaller);
    Attach("Larger", Op.Larger);
    Attach("AutoSelect", Op.AutoViewArea);
    Attach("Undo", Op.Undo);
    Attach("Redo", Op.Redo);
    lect.menuFV := form;
    (* Toolbar window *)
    Create("toolbar.fv");
    form.firstPopup := FALSE;
    lect.toolbarFilter := FormsVBT.GetVBT(form, "TBFilter");
    Attach("TBClose", Op.ShowHideToolbar);
    Attach("TBBack", Op.PrevSector);
    Attach("TBFwd", Op.NextSector);
    Attach("TBBack20", Op.PrevPage20);
    Attach("TBBack5", Op.PrevPage5);
    Attach("TBBack1", Op.PrevPage);
    Attach("TBFwd1", Op.NextPage);
    Attach("TBFwd5", Op.NextPage5);
    Attach("TBFwd20", Op.NextPage20);
    Attach("TBUndo", Op.Undo);
    Attach("TBRedo", Op.Redo);
    Attach("TBLarger", Op.Larger);
    Attach("TBSmaller", Op.Smaller);
    Attach("TBZoom", Op.Zoom);
    Attach("TBView", Op.ShowHideViewing);
    Attach("TBLinks", Op.ShowHideLinks);
    Attach("TBOpen", Op.Open);
    lect.toolbarFV := form;
    (* Viewing Controls window *)
    Create("viewing.fv");
    form.firstPopup := TRUE;
    lect.viewingFilter := FormsVBT.GetVBT(form, "ViewingFilter");
    lect.viewArea := NEW(MyViewAreaVBT, lect := lect).init(Images.Empty,
                                                           PaintOp.Bg,
                                                           Rect.Full);
    VBT.SetCursor(lect.viewArea,
                  CursorFromRsrc("target.1.pbm",
                                 "target.2.pbm",
                                 Point.T{7,7},
                                 lect.rsrcPath));
    FormsVBT.PutGeneric(form, "ViewArea", lect.viewArea);
    Attach("ViewingBtn", Op.ShowHideViewing);
    Attach("FullPage", Op.FullPage);
    Attach("HorThenVer", Op.HorThenVer);
    Attach("VerThenHor", Op.VerThenHor);
    Attach("HorOnly", Op.HorOnly);
    Attach("VerOnly", Op.VerOnly);
    Attach("AutoViewArea", Op.AutoViewArea);
    Attach("ManualViewArea", Op.ManualViewArea);
    lect.viewingFV := form;
    (* Popup window *)
    Create("popup.fv");
    lect.fb := NEW(LecternFileBrowserVBT, fv := form).init(lect.font,
                                                           lect.shadow);
    FormsVBT.PutGeneric(form, "File", lect.fb);
    WITH
      fileFrom = FormsVBT.GetVBT(form, "FileFrom"),
      dm = NEW(MyFileBrowserVBT.DirMenu).init(lect.font, lect.shadow2),
      fh = NEW(FilteredHelper,
               tabNext := fileFrom,
               fv := form,
               name := "FileHelper").init(expandOnDemand := TRUE,
                                          font := lect.font,
                                          colorScheme := lect.shadow,
                                          turnMargin := 0.0) DO
      FormsVBT.PutGeneric(form, "DirMenu", dm);
      FormsVBT.PutGeneric(form, "FileHelper", fh);
      <*FATAL MyFileBrowserVBT.Error*>
      BEGIN
        MyFileBrowserVBT.SetHelper(lect.fb, fh);
        MyFileBrowserVBT.SetDirMenu(lect.fb, dm);
      END;
    END;
    lect.progressVBT := NEW(ProgressVBT);
    FormsVBT.PutGeneric(form, "Progress", lect.progressVBT);
    FormsVBT.PutText(form, "Version", Version);
    Attach("AboutClose", Op.Null);
    Attach("GotoClose", Op.No);
    Attach("GotoCount", Op.Yes);
    Attach("GotoPage", Op.Yes);
    Attach("GoForward", Op.GoForward);
    Attach("GoBackward", Op.GoBackward);
    Attach("GotoCancel", Op.No);
    Attach("FileClose", Op.No);
    Attach("PrintToPrinter", Op.PrintToPrinter);
    Attach("PrintToFile", Op.PrintToFile);
    Attach("Original", Op.FileUseOriginal);
    Attach("Images", Op.FileUseImages);
    Attach("FileFrom", Op.Yes);
    Attach("FileTo", Op.Yes);
    Attach("OpenHere", Op.Yes);
    Attach("OpenNew", Op.OpenNew);
    Attach("SaveConfirm", Op.Yes);
    Attach("PrintConfirm", Op.Yes);
    Attach("FileCancel", Op.No);
    Attach("PrintCommand", Op.Yes);
    Attach("ErrorClose", Op.Null);
    Attach("Stop", Op.Stop);
    Attach("Yes", Op.Yes);
    Attach("No", Op.No);
    lect.popupFV := form;
    (* Find window *)
    Create("find.fv");
    Attach("FindTxt", Op.FindNext);
    Attach("FindClose", Op.Null);
    Attach("FindFirst", Op.FindFirst);
    Attach("FindNext", Op.FindNext);
    Attach("FindPrevious", Op.FindPrevious);
    Attach("FindStop", Op.Stop);
    lect.findFV := form;
    (* Thumbnail widget and/or window; uses .shadow, initialized by popupFV *)
    CreateThumbnailWindow(lect);
    (* Tile sub-window; uses .thumbnailVBT, initialized bu CreateThum.. *)
    Create("linkTile.fv");
    lect.tileFilter := FormsVBT.GetVBT(form, "LinkTileFilter");
    Attach("BookmarkZoom", Op.BookmarkZoom);
    Attach("DiaryZoom", Op.DiaryZoom);
    Attach("OutlineZoom", Op.OutlineZoom);
    Attach("ThumbnailZoom", Op.ThumbnailZoom);
    Attach("BookmarkNew", Op.BookmarkNew);
    Attach("DiaryNew", Op.DiaryNew);
    Attach("OutlineNew", Op.OutlineNew);
    Attach("BookmarkEdit", Op.BookmarkEdit);
    Attach("DiaryEdit", Op.DiaryEdit);
    Attach("OutlineEdit", Op.OutlineEdit);
    Attach(TileBrowser[Tile.Bookmark], Op.BookmarkHit);
    Attach(TileBrowser[Tile.Diary], Op.DiaryHit);
    Attach(TileBrowser[Tile.Outline], Op.OutlineHit);
    FOR t := FIRST(Tile) TO LAST(Tile) DO
      lect.tileVBT[t] := FormsVBT.GetVBT(form, TileName[t]);
    END;
    lect.bookmarkVBT := FormsVBT.GetVBT(form, TileBrowser[Tile.Bookmark]);
    lect.diaryVBT := FormsVBT.GetVBT(form, TileBrowser[Tile.Diary]);
    lect.outlineVBT := FormsVBT.GetVBT(form, TileBrowser[Tile.Outline]);
    IF NOT ThumbnailWindow THEN
      FormsVBT.PutGeneric(form, TileBrowser[Tile.Thumbnail],
                          lect.thumbnailVBT);
    END;
    lect.tileZoomed := AllTilesNormal;
    lect.tileSplitter := FormsVBT.GetVBT(form, "LinkTileSplitter");
    StableVBT.SetShape(lect.tileSplitter, 180, 0);
    lect.tileFV := form;
    (* Links dialog.  NOTE: requires the link tile sub-window *)
    IF lect.links = NIL THEN
      (* Links dialog *)
      Create("links.fv");
      Attach("Btn", Op.No);
      Attach("Browser", Op.Yes);
      Attach("LinkName", Op.Yes);
      Attach("LinkRename", Op.Yes);
      Attach("Define", Op.Yes);
      Attach("Close1", Op.No);
      Attach("Close2", Op.Yes);
      Attach("Jump", Op.Yes);
      Attach("Close3", Op.No);
      <* FATAL Rsrc.NotFound *>
      BEGIN
        lect.links := NEW(MyLinks, lect := lect).init(form,
                             Rsrc.Open("bookmark.pbm", lect.rsrcPath),
                             Rsrc.Open("diary.pbm", lect.rsrcPath));
      END;
    ELSE
      (* ensure that the diary and bookmark links are loaded *)
      UpdateLinksBrowser(lect.diaryVBT, lect.diary);
      UpdateLinksBrowser(lect.bookmarkVBT, lect.bookmark);
    END;
    (* Create and install the image window *)
    <*FATAL MultiSplit.NotAChild, Split.NotAChild*>
    BEGIN
      WITH
        logoVBT = NARROW(Split.Succ(lect.iw, NIL), ImageVBT.T),
        logo = logoVBT.get() DO
        lect.iv := NEW(LectImageVBT, lect := lect).init(logo, lect.bg);
        lect.mainSplit := NEW(SplitterVBT.T).init(hv := Axis.T.Hor,
                                                  op := lect.shadow2.bgFg);
        MultiSplit.Insert(lect.mainSplit, NIL, lect.iv);
        lect.iz := NEW(ZSplit.T).init(lect.mainSplit);
        Split.Replace(lect.iw, logoVBT, lect.iz);
      END;
    END;
    (* Install floating windows *)
    InsertZ(lect, lect.menuFV, 0, 0, NOT lect.hideMenu);
    InsertZ(lect, lect.toolbarFV, 0, 36, FALSE);
    InsertZ(lect, lect.popupFV, 0, 0, FALSE);
    InsertZ(lect, lect.viewingFV, 0, 0, FALSE);
    InsertZ(lect, lect.findFV, 0, 0, FALSE);
  END CreateForms;


(* *)
(* Popups and modal dialogs *)
(* *)

PROCEDURE InsertZ(lect: Lectern; ch: VBT.T; h, v: INTEGER; mapped: BOOLEAN) =
    (* Insert ch as child of lect.iz *)
  BEGIN
    ZSplit.InsertAt(lect.iz, ch,
                    Point.Add(Rect.NorthWest(ZSplit.GetParentDomain(lect.iz)),
                              Point.T{h := h, v := v}),
                    ZSplit.Altitude.Top,
                    mapped);
  END InsertZ;

PROCEDURE ShowZ(lect: Lectern; child: VBT.T) =
    (* LL = VBT.mu *)
  BEGIN
    ZSplit.Lift(child, ZSplit.Altitude.Top);
    ZSplit.Map(child);
    IF ShowWindow(lect.iw, lect.actionWindow, lect.imageTitle) THEN
    ELSE
      (* We have no way of communicating with the user.  Sad. *)
      TRY
        Wr.PutText(Stdio.stderr, "Lectern: can't contact display server\n");
        Wr.Flush(Stdio.stderr);
        Process.Exit(1);
      EXCEPT
        Thread.Alerted =>
      END;
    END;
  END ShowZ;

PROCEDURE ShowZVisible(lect: Lectern; child: LecternFVClass) =
    (* As ShowZ, but also ensure that child is reasonably visible *)
    (* If child.firstPopup, centers child *)
    (* LL = VBT.mu *)
  VAR
    prev: Rect.T;
  BEGIN
    WITH parent = VBT.Parent(child) DO
      IF parent # lect.iz THEN
        <*FATAL Split.NotAChild*>
        BEGIN
          IF parent # NIL THEN Split.Delete(parent, child) END;
        END;
        InsertZ(lect, child, 0, 0, FALSE);
        child.firstPopup := TRUE;
        child.lect := lect;
      END;
    END;
    prev := ZSplit.GetDomain(child);
    WITH z = ZSplit.GetParentDomain(lect.iz) DO
      IF NOT Rect.IsEmpty(z) THEN
        IF child.firstPopup THEN prev := z; child.firstPopup := FALSE; END;
        ZSplit.Move(child,
                    EnsureVisible(Rect.Center(ZSplit.GetDomain(child),
                                              Rect.Middle(prev)),
                                  z));
      END;
    END;
    ShowZ(lect, child);
  END ShowZVisible;

PROCEDURE HideZ(<*UNUSED*>lect: Lectern; child: VBT.T) =
    (* LL = VBT.mu *)
  BEGIN
    IF child # NIL AND ZSplit.IsMapped(child) THEN ZSplit.Unmap(child) END;
  END HideZ;

PROCEDURE SetPopupFocus(lect: Lectern; focus: TEXT;
                        replaceMode: BOOLEAN;
                        time: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  (* Give "focus" the focus, and fix the "FileTo" tabnext field *)
  VAR
    focusV: VBT.T;
  BEGIN
    IF focus # NIL THEN
      IF Text.Equal(focus, "FileHelper") THEN
        focusV := FormsVBT.GetGeneric(lect.popupFV, focus);
      ELSE
        focusV := FormsVBT.GetVBT(lect.popupFV, focus);
      END;
      IF ISTYPE(focusV, NumericVBT.T) THEN
        focusV := NARROW(focusV, NumericVBT.T).typein;
      END;
      WITH toV =  FormsVBT.GetVBT(lect.popupFV, "FileTo") DO
        NARROW(toV, NumericVBT.T).typein.tabNext := focusV;
      END;
      IF TextPort.TryFocus(focusV, time) AND replaceMode THEN
        TextPort.Select(v := focusV, time := time, replaceMode := TRUE);
      END;
    END;
  END SetPopupFocus;

PROCEDURE DoPopup(lect: Lectern; name: TEXT;
                  focus: TEXT := NIL;
                  replaceMode := FALSE;
                  time: VBT.TimeStamp := 0) =
  (* LL = VBT.mu *)
  (* Make one flavor of popup visible; handles all errors. If "focus" is
     non-NIL, try to give it the keyboard focus. *)
  VAR
    index: INTEGER;
  BEGIN
    <* ASSERT lect.popupFV # NIL *>
    lect.showingError := FALSE;
    IF Text.Equal(name, "AboutDlg") THEN index := 0;
    ELSIF Text.Equal(name, "GotoDlg") THEN index := 1;
    ELSIF Text.Equal(name, "FileDlg") THEN index := 2;
    ELSIF Text.Equal(name, "ErrorDlg") THEN index := 3;
    ELSIF Text.Equal(name, "ProgressDlg") THEN index := 4;
    ELSIF Text.Equal(name, "ConfirmDlg") THEN index := 5;
    ELSE <*ASSERT FALSE*>
    END;
    FormsVBT.PutInteger(lect.popupFV, "DlgTSplit", index);
    lect.popupFV.firstPopup := TRUE; (* Force centering *)
    ShowZVisible(lect, lect.popupFV);
    SetPopupFocus(lect, focus, replaceMode, time);
  END DoPopup;

PROCEDURE DoPopDown(lect: Lectern; time: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    IF lect.popupFV # NIL THEN HideZ(lect, lect.popupFV) END;
    IF lect.iw # NIL THEN
      IF lect.iw.childFocus # NIL THEN lect.iw.takeFocus(time) END;
    END;
    lect.showingError := FALSE;
  END DoPopDown;

PROCEDURE DoModalLocked(lect: Lectern; VAR time: VBT.TimeStamp): Op =
  (* Wait for a response, in a modal dialog style *)
  (* LL = VBT.mu *)
  VAR response: Op;
  BEGIN
    lect.passive(FALSE);
    WHILE lect.response = Op.Null DO Thread.Wait(VBT.mu, lect.responseCV) END;
    response := lect.response; lect.response := Op.Null;
    time := lect.responseTime;
    RETURN response
  END DoModalLocked;

PROCEDURE DoModal(lect: Lectern; VAR time: VBT.TimeStamp): Op =
  (* LL = lect *)
  BEGIN
    LOCK VBT.mu DO RETURN lect.doModalLocked(time) END;
  END DoModal;

PROCEDURE DoModalPopup(lect: Lectern; name: TEXT;
                       focus: TEXT := NIL;
                       replaceMode := FALSE;
                       VAR time: VBT.TimeStamp): Op =
  (* Pop-up, wait for modal response, then pop down *)
  (* LL = lect *)
  VAR
    responseTime: VBT.TimeStamp;
  BEGIN
    LOCK VBT.mu DO
      DoPopup(lect, name, focus, replaceMode, time);
      WITH op = lect.doModalLocked(responseTime) DO
        DoPopDown(lect, responseTime);
        RETURN op;
      END;
    END;
  END DoModalPopup;

PROCEDURE Confirm(lect: Lectern; message, yes, no: TEXT;
                  VAR time: VBT.TimeStamp): BOOLEAN =
  (* LL = lect *)
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(lect.popupFV, "ConfirmText", message);
      FormsVBT.PutText(lect.popupFV, "YesText", yes);
      FormsVBT.PutText(lect.popupFV, "NoText", no);
    END;
    RETURN lect.doModalPopup("ConfirmDlg", "ConfirmText",
                             FALSE, time) = Op.Yes
  END Confirm;

PROCEDURE GetFileName(lect: Lectern): TEXT =
  (* LL = VBT.mu *)
  BEGIN
    TRY
      RETURN MyFileBrowserVBT.GetFile(lect.fb);
    EXCEPT MyFileBrowserVBT.Error(e) => RETURN e.path
    END;
  END GetFileName;

PROCEDURE PutFileName(lect: Lectern; name: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    TRY MyFileBrowserVBT.Set(lect.fb, name);
    EXCEPT MyFileBrowserVBT.Error => (* Well, I won't bother then *)
    END;
  END PutFileName;

PROCEDURE DoFileDlg(lect: Lectern;
                    label, printDest, fileDest, ps, epsf, confirm: INTEGER;
                    focus: TEXT;
                    time: VBT.TimeStamp): Op =
  (* LL = lect *)
  (* Prepare file browser for a particular use *)
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutInteger(lect.popupFV, "FileDlgLabel", label);
      FormsVBT.PutInteger(lect.popupFV, "PrintDest", printDest);
      FormsVBT.PutInteger(lect.popupFV, "FileDest", fileDest);
      FormsVBT.PutInteger(lect.popupFV, "FilePostScript", ps);
      FormsVBT.PutInteger(lect.popupFV, "FileEPSF", epsf);
      FormsVBT.PutInteger(lect.popupFV, "FileConfirmLabel", confirm);
    END;
    LOOP
      WITH response = lect.doModalPopup("FileDlg", focus, TRUE, time) DO
        IF response # Op.No THEN
          LOCK VBT.mu DO
            WITH name = GetFileName(lect) DO
              TRY
                IF FS.Status(name).type = FS.DirectoryFileType THEN
                  PutFileName(lect, name);
                  (* and loop *)
                ELSE
                  RETURN response;
                END;
              EXCEPT
                OSError.E => RETURN response (* we'll report the error later *)
              END;
            END;
          END;
        ELSE
          RETURN response;
        END;
      END;
    END;
  END DoFileDlg;

PROCEDURE SetAndGotoSelection(lect: Lectern; time: VBT.TimeStamp)
                             RAISES { Thread.Alerted } =
  (* LL = SELF *)
  (* Ensure selection is on current page and scroll to it *)
  VAR
    selStart, selEnd: LecternOCR.SelPos;
  BEGIN
    LOCK VBT.mu DO
      lect.ocr.getSelection(selStart, selEnd);
      IF selStart = LecternOCR.NoSelPos OR selStart.page # lect.page THEN
        selStart.page := lect.page;
        selStart.word := 0;
        lect.ocr.setSelection(selStart, selStart, time,
                              lect.page, lect.class);
      END;
      lect.gotoSelection(Links.NoLink);
    END;
  END SetAndGotoSelection;

PROCEDURE ShowFind(lect: Lectern; time: VBT.TimeStamp)
                   RAISES { Thread.Alerted } =
  (* LL = SELF *)
  (* Ensure the "find" dialog is visible, and give it the focus, and show
     selection *)
  BEGIN
    IF lect.path # NIL THEN
      LOCK VBT.mu DO
        <* ASSERT lect.findFV # NIL *>
        FormsVBT.PutText(lect.findFV, "Status", "Find ...");
        ShowZVisible(lect, lect.findFV);
        FormsVBT.TakeFocus(lect.findFV, "FindTxt", time, TRUE);
      END;
      SetAndGotoSelection(lect, time);
    END;
  END ShowFind;

(* *)
(* Lectern methods and creation *)
(* *)

PROCEDURE Beep(lect: Lectern) =
    (* LL = VBT.mu *)
  BEGIN
    IF lect.iv # NIL THEN lect.iv.flash() END;
    IF lect.iw # NIL THEN lect.iw.flush() END;
  END Beep;

PROCEDURE Error(lect: Lectern; msg: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    FormsVBT.PutText(lect.popupFV, "ErrorText", msg);
    DoPopup(lect, "ErrorDlg");
    lect.showingError := TRUE;
  END Error;

PROCEDURE Passive(lect: Lectern; bePassive: BOOLEAN) =
    (* LL = VBT.mu *)
    (* Make everything passive.  For Op.Init, must tolerate VBT's not having
       been created yet. *)
    VAR
      state: ReactivityVBT.State;
      cursor: Cursor.T;
  PROCEDURE AdjustReactivity(fv: FormsVBT.T; name: TEXT) =
    BEGIN
      IF bePassive THEN
        FormsVBT.MakePassive(fv, name);
      ELSE
        FormsVBT.MakeActive(fv, name);
      END;
    END AdjustReactivity;
  BEGIN
    IF bePassive THEN
      state := ReactivityVBT.State.Passive;
      cursor := lect.waitCursor;
    ELSE
      state := ReactivityVBT.State.Active;
      cursor := Cursor.DontCare;
    END;
    lect.isPassive := bePassive;
    IF lect.iw # NIL THEN lect.iw.passive(bePassive) END;
    IF lect.iv # NIL THEN VBT.SetCursor(lect.iv, cursor) END;
    IF lect.thumbnailW # NIL THEN
      lect.thumbnailW.passive(bePassive);
      ReactivityVBT.Set(lect.thumbnailFilter, state, cursor);
    END;
    IF lect.menuFV # NIL THEN
      ReactivityVBT.Set(lect.menuFilter, state, cursor);
    END;
    IF lect.toolbarFV # NIL THEN
      (* Setting "passive" sends a "gone" position to the trill buttons,
         causing extra events (and flicker):
      ReactivityVBT.Set(lect.toolbarFilter, state, cursor);
         Instead, just make it look passive: *)
      VBT.SetCursor(lect.toolbarFilter, cursor);
    END;
    IF lect.viewingFV # NIL THEN
      ReactivityVBT.Set(lect.viewingFilter, state, cursor);
    END;
    IF lect.findFV # NIL THEN
      VBT.SetCursor(lect.findFV, cursor);
      AdjustReactivity(lect.findFV, "FindClose");
      AdjustReactivity(lect.findFV, "FindTxt");
      AdjustReactivity(lect.findFV, "FindFirst");
      AdjustReactivity(lect.findFV, "FindNext");
      AdjustReactivity(lect.findFV, "FindPrevious");
    END;
    IF lect.tileFV # NIL THEN
      ReactivityVBT.Set(lect.tileFilter, state, cursor);
    END;
    Thread.Pause(0.025D0);
  END Passive;

PROCEDURE FreeImage(lect: Lectern; VAR image: ImageRd.T) =
    (* LL = VBT.mu *)
  BEGIN
    IF image # NIL THEN
      IF lect.spare1 = NIL THEN
        lect.spare1 := image;
      ELSIF lect.spare2 = NIL THEN
        lect.spare2 := image;
      ELSIF lect.spare3 = NIL THEN
        lect.spare3 := image
      ELSE
        <*ASSERT FALSE*> (* too many images *)
      END;
      (* cancel read-ahead *) EVAL image.init(NIL, 0, 0);
      image := NIL;
    END;
  END FreeImage;

VAR (*CONST*) (* The Digital Logo color, approximately *)
  decFg := PaintOp.FromRGB(0.256, 0.0, 0.029,
                           PaintOp.Mode.Accurate,
                           0.0,
                           PaintOp.BW.UseFg);

PROCEDURE DefaultImage(lect: Lectern): Images.T =
    (* Returns the default image for a viewer that has no document *)
  BEGIN
    TRY
      RETURN ImageFromRsrc("logo", lect.rsrcPath,
                           PaintOp.Pair(lect.bg, decFg))
    EXCEPT Thread.Alerted =>
      RETURN Images.Empty
    END;
  END DefaultImage;

PROCEDURE CreateViewer(lect: Lectern; link: Links.Link;
                      time: VBT.TimeStamp): Lectern =
    (* LL = VBT.mu *)
    (* Create a new viewer, link it into the ring, and fork its
       initialization *)
  VAR
    child := NEW(Lectern);
  BEGIN
    child.initLink := link;
    child.class := lect.class;
    child.nextV := lect.nextV;
    lect.nextV := child;
    child.prevV := lect;
    child.nextV.prevV := child;
    child.closed := TRUE;
    child.links := lect.links;
    child.diary := lect.diary;
    child.bookmark := lect.bookmark;
    child.applyOp(Op.Init, time);
    RETURN child;
  END CreateViewer;

PROCEDURE InitInstance(lect: Lectern; link: Links.Link) =
    (* LL = lect *)
  <*FATAL Thread.Alerted*>
  BEGIN
    lect.responseCV := NEW(Thread.Condition);
    lect.printFile := "/tmp/lect." &
                        Fmt.Int(NEW(Random.Default).init().integer(0, 999999));
    lect.rsrcPath := Rsrc.BuildPath("$LecternPATH", LecternBundle.Get());
    lect.bg := PaintOp.FromRGB(0.67, 0.67, 0.67);
    lect.waitCursor := CursorFromRsrc("wait.1.pbm",
                                      "wait.2.pbm",
                                      Point.T{7,7},
                                      lect.rsrcPath);
    lect.clientMiscCode := VBT.GetMiscCodeType("LecternClient");
    WITH
      logo = DefaultImage(lect),
      logoVBT = NEW(LectImageVBT, lect := lect).init(logo, lect.bg) DO
      lect.iw := NEW(LectFilterVBT,
                     lect := lect,
                     waitCursor := lect.waitCursor).init(logoVBT);
    END;
    lect.imageTitleBase := InitImageTitle;
    lect.imageTitle := lect.imageTitleBase;
    TRY
      TRY
        LOCK VBT.mu DO
          (* Install window at earliest opportunity, to entertain user *)
          lect.passive(TRUE);
          IF link # Links.NoLink OR lect.nextV = lect THEN
            (* Install window if we've been asked to display a document,
               or if we're the only (initial) viewer. *)
            XTrestle.Install(v := lect.iw,
                           applName := AppName,
                           windowTitle := lect.imageTitle,
                           iconTitle := lect.imageTitle,
                           iconWindow := NIL);
            lect.closed := FALSE;
          END;
        END;
        Thread.Pause(0.25D0); (* let image window paint *)
      FINALLY
        (* Be sure this executes even on error, so we have valid viewer *)
        LOCK VBT.mu DO
          CreateForms(lect);
          lect.passive(TRUE);
        END;
      END;
    EXCEPT
    | TrestleComm.Failure =>
        LOCK VBT.mu DO
          lect.error("Can't contact display server");
        END;
    | XTrestle.Error =>
        LOCK VBT.mu DO
          lect.error("Error in \"-geometry\" or \"-display\" parameter");
        END;
    END;
    IF link # Links.NoLink THEN
      lect.readDoc(link, 0); (* Even on error: it's still a viewer *)
    END;
    LOCK VBT.mu DO
      IF lect.path = NIL THEN
        (* No path specified, or it failed. *)
        TRY
          PutFileName(lect, Canonicalize(Pathname.Current));
        EXCEPT OSError.E, BadPath => (* ignore; we did our best *)
        END; 
        lect.imageTitleBase := EmptyImageTitle;
        IF lect.server = NIL THEN
          lect.imageTitle := lect.imageTitleBase
        ELSE
          lect.imageTitle := lect.imageTitleBase & ServerSuffix;
        END;
        lect.thumbnailTitle := EmptyThumbnailTitle;
        lect.dir := LecternDoc.Dir{
            outline := LecternDoc.Component{},
            original := LecternDoc.Component{},
            pages := NEW(LecternDoc.DirPages, 0),
            attributes := NEW(LecternDoc.Attributes, 0),
            gammas := NEW(LecternDoc.Gammas, 0)
          };
        lect.usePart(Partition.VerThenHor);
        Retitle(lect.iw, lect.imageTitle);
      END;
    END;
  END InitInstance;

PROCEDURE Normalize(lect: Lectern; delta: Point.T): Point.T =
    (* LL = VBT.mu+SELF *)
  VAR
    imDomain, viewArea, visibleDomain, visibleViewArea: Rect.T;
    error: Point.T;
  PROCEDURE AdjustHorizontal() =
    (* Ensure that we cover all the horizontal view area *)
    BEGIN
      (* If there's viewArea to the east, don't waste space at west *)
      IF visibleViewArea.east > visibleDomain.east THEN
        error.h := MAX(0, MIN(error.h,
                              visibleViewArea.west - visibleDomain.west));
      END;
      (* If there's viewArea to the west, don't waste space at east *)
      IF visibleViewArea.west < visibleDomain.west THEN
        error.h := MIN(0, MAX(error.h,
                              visibleViewArea.east - visibleDomain.east));
      END;
    END AdjustHorizontal;
  PROCEDURE AdjustVertical() =
    (* Ensure that we cover all the vertical view area *)
    BEGIN
      (* If there's viewArea to the south, don't waste space at north *)
      IF visibleViewArea.south > visibleDomain.south THEN
        error.v := MAX(0, MIN(error.v,
                              visibleViewArea.north - visibleDomain.north));
      END;
      (* If there's viewArea to the north, don't waste space at south *)
      IF visibleViewArea.north < visibleDomain.north THEN
        error.v := MIN(0, MAX(error.v,
                              visibleViewArea.south - visibleDomain.south));
      END;
    END AdjustVertical;
  BEGIN
    IF lect.pm = NIL THEN (* no current page *) RETURN Point.Origin END;
    imDomain := lect.pm.domain(lect.iv);
    viewArea := TransformRect(lect.lasso, lect.lassoDomain, imDomain);
    visibleDomain := Rect.Inset(VBT.Domain(lect.iv), Margin);
    (* If the viewArea is smaller than the
       window, we want to allocate the excess space so as to tend
       to center the image in the window (in the limit where the image
       is also smaller than the window).  We achieve this by translating
       the view area towards the center of the image, by an amount that
       ensures the entire untranslated viewArea will remain visible. *)
    WITH
      excessV = MAX(0, Rect.VerSize(visibleDomain) - Rect.VerSize(viewArea)),
      excessH = MAX(0, Rect.HorSize(visibleDomain) - Rect.HorSize(viewArea)),
      centerDiff = Point.Sub(Rect.Middle(imDomain), Rect.Middle(viewArea)) DO
      viewArea := Rect.Add(viewArea,
                           Point.T{h := MAX(MIN(centerDiff.h, excessH DIV 2),
                                            -excessH DIV 2),
                                   v := MAX(MIN(centerDiff.v, excessV DIV 2),
                                            -excessV DIV 2)});
    END;
    (* Translate the viewArea to the coordinates of the VBT (visibleDomain) *)
    visibleViewArea := Rect.Add(viewArea,
                                Point.Add(delta,
                                          Point.Sub(Rect.Middle(visibleDomain),
                                                    Rect.Middle(imDomain))));
    (* Compute distance from viewArea's center to VBT's center.  If we leave
       this value in "error", it will cause viewArea to be centered in the
       window in both axes. *)
    error := Point.Sub(Rect.Middle(visibleViewArea),
                       Rect.Middle(visibleDomain));
    CASE lect.part OF
    | Partition.FullPage =>
        (* initial value of "error" is appropriate *)
    | Partition.HorThenVer, Partition.VerThenHor =>
        AdjustHorizontal(); AdjustVertical();
    | Partition.HorOnly =>
        AdjustHorizontal();
    | Partition.VerOnly =>
        AdjustVertical();
    END;
    RETURN Point.Sub(delta, error)
  END Normalize;

CONST (* deltas that, when normalized, will yield first and last sectors *)
  FirstSectorDelta = Point.T{ h := 100000000, v := 100000000 };
  LastSectorDelta = Point.T{ h := -100000000, v := -100000000 };

PROCEDURE UsePart(lect: Lectern; part: Partition) =
    (* LL = VBT.mu+SELF *)
  VAR
    partBtn: TEXT;
  BEGIN
    lect.part := part;
    CASE lect.part OF
    | Partition.FullPage => partBtn := "FullPage";
    | Partition.HorThenVer => partBtn := "HorThenVer";
    | Partition.VerThenHor => partBtn := "VerThenHor";
    | Partition.HorOnly => partBtn := "HorOnly";
    | Partition.VerOnly => partBtn := "VerOnly";
    ELSE <*ASSERT FALSE*>
    END;
    IF lect.viewingFV # NIL THEN
      IF NOT FormsVBT.IsSelected(lect.viewingFV, partBtn) THEN
        FormsVBT.MakeSelected(lect.viewingFV, partBtn);
      END;
    END;
  END UsePart;

PROCEDURE LinkToHere(lect: Lectern): Links.Link =
    (* LL = VBT.mu+SELF *)
  BEGIN
    RETURN Links.LinkToHere(lect.path, lect.dir, lect.ocr, lect.page);
  END LinkToHere;

PROCEDURE OpenPage(lect: Lectern; page: PageNumber;
                   class: LecternDoc.Class;
                   reportNoPage, reportOther: BOOLEAN): ImageRd.T =
    (* LL = VBT.mu+SELF *)
    (* Internal subroutine of GotoPage, textually outside for clarity *)
  VAR
    newPM: ImageRd.T;
    relPage := page + lect.dir.origin;
    st: VBT.ScreenType;
  PROCEDURE Report(more: TEXT) =
    VAR msg: TEXT;
    BEGIN
      IF lect.path # NIL THEN
        msg := "\"" & lect.path & "\" has no image " &
               Fmt.Int(page+lect.dir.origin+1);
        IF page > 0 THEN
          msg := msg & " (page " & Fmt.Int(page) & ")";
        END;
        msg := msg & more;
        lect.error(msg);
      END;
    END Report;
  BEGIN
    IF lect.path = NIL OR relPage < 0 OR relPage > LAST(lect.dir.pages^) THEN
      IF reportNoPage THEN
        (* Explicit bad page position from user, so post error dialog *)
        Report("");
      ELSIF reportOther THEN
        (* Use can take error messages, so give mild indication of failure *)
        lect.beep();
      END;
      RETURN NIL
    ELSE
      WITH entry = lect.dir.pages[relPage][class] DO
        IF entry.start = 0 THEN
          IF reportOther THEN Report(" at this scale") END;
          RETURN NIL
        ELSE
          IF lect.spare3 # NIL THEN (* avoid allocating ScrnPixmap.Raw's *)
            newPM := lect.spare3; lect.spare3 := NIL;
          ELSIF lect.spare2 # NIL THEN
            newPM := lect.spare2; lect.spare2 := NIL;
          ELSIF lect.spare1 # NIL THEN
            newPM := lect.spare1; lect.spare1 := NIL;
          ELSE
            newPM := NEW(ImageRd.T);
          END;
          IF PreReadPage THEN
            st := VBT.ScreenTypeOf(lect.iv)
          ELSE
            st := NIL;
          END;
          RETURN newPM.init(lect.rd,
                            entry.start,
                            entry.length,
                            PaintOp.Copy,
                            st,
                            ImageGamma)
        END;
      END;
    END;
  END OpenPage;

PROCEDURE GotoPage(lect: Lectern; page: PageNumber;
                   class: LecternDoc.Class;
                   undoLink: Links.Link;
                   noisy := TRUE): BOOLEAN =
    (* LL = VBT.mu+SELF *)
  VAR
    newPM: ImageRd.T;
  BEGIN
    IF page = lect.page AND lect.pm # NIL AND class = lect.class THEN
      (* use existing page *)
      RETURN TRUE;
    ELSIF page = lect.page-1 AND lect.prev # NIL AND class = lect.class THEN
      lect.freeImage(lect.next);
      newPM := lect.prev;
      lect.prev := NIL;
    ELSIF page = lect.page+1 AND lect.next # NIL AND class = lect.class THEN
      lect.freeImage(lect.prev);
      newPM := lect.next;
      lect.next := NIL;
    ELSE
      lect.freeImage(lect.next);
      lect.freeImage(lect.prev);
      newPM := OpenPage(lect, page, class, noisy, TRUE);
      IF newPM = NIL THEN RETURN FALSE END;
    END;
    IF page = lect.page+1 AND class = lect.class THEN
      lect.prev := lect.pm;
      lect.pm := NIL;
    ELSIF page = lect.page-1 AND class = lect.class THEN
      lect.next := lect.pm;
      lect.pm := NIL;
    ELSE
      lect.freeImage(lect.pm);
    END;
    IF ReadNextPage AND lect.next = NIL THEN
      lect.next := OpenPage(lect, page+1, class, FALSE, FALSE);
    END;
    IF ReadPrevPage AND lect.prev = NIL THEN
      lect.prev := OpenPage(lect, page-1, class, FALSE, FALSE);
    END;
    <* ASSERT newPM # NIL AND lect.pm = NIL *>
    IF undoLink # Links.NoLink THEN
      lect.undo := NEW(Undo, next := lect.undo, link := undoLink);
      lect.redo := NIL;
    END;
    lect.pm := newPM;
    lect.page := page;
    lect.class := class;
    lect.iv.put(newPM, lect.bg, lect.dropShadow, lect.paintChunk);
    IF lect.menuFV # NIL THEN
      WITH i = Fmt.Int(page+lect.dir.origin+1) DO
        IF page > 0 THEN
          WITH p = Fmt.Int(page) DO
            FormsVBT.PutText(lect.menuFV, "This", "p." & p);
            FormsVBT.PutText(lect.menuFV, "FullPageNumber", "Page: " & p);
          END;
        ELSE
          FormsVBT.PutText(lect.menuFV, "This", "i." & i);
          FormsVBT.PutText(lect.menuFV, "FullPageNumber", "");
        END;
        FormsVBT.PutText(lect.menuFV, "FullImageNumber", "Image: " & i);
      END;
      VBT.Sync(lect.menuFV, FALSE); (* get it on-screen before painting pm *)
    END;
    WITH relPage = page + lect.dir.origin DO
      IF lect.thumbnailVBT # NIL THEN
        lect.thumbnailVBT.scrollToShow(relPage);
        lect.thumbnailVBT.set(relPage);
        IF lect.viewArea # NIL THEN
          lect.viewArea.put(lect.thumbnailVBT.acquire(relPage), lect.bg);
        END;
      END;
    END;
    IF lect.autoViewArea THEN lect.computeViewArea() END;
    RETURN TRUE;
  END GotoPage;

PROCEDURE GotoPageAuto(lect: Lectern;
                       page: PageNumber;
                       class: LecternDoc.Class;
                       part: Partition) =
    (* LL = SELF *)
  VAR prevPage := lect.page; prevPart := lect.part;
  BEGIN
    LOCK VBT.mu DO
      IF lect.gotoPage(page, class, lect.linkToHere(), TRUE) THEN
        lect.usePart(part);
        IF lect.page = prevPage THEN
          IF lect.part = prevPart THEN
            lect.iv.moveTo(lect.normalize(lect.iv.getDelta()),
                           lect.animationSpeed,
                           lect.highlightMotion);
          ELSE
            lect.iv.moveTo(lect.normalize(FirstSectorDelta),
                           lect.animationSpeed,
                           lect.highlightMotion);
          END;
        ELSE
          lect.iv.moveTo(lect.normalize(FirstSectorDelta));
        END;
      END;
    END;
  END GotoPageAuto;

PROCEDURE GotoSelection(lect: Lectern; undoLink: Links.Link)
                        RAISES { Thread.Alerted } =
    (* LL = VBT.mu+SELF *)
  VAR
    selStart, selEnd: LecternOCR.SelPos;
    page: INTEGER;
    r: Rect.T;
    d1, d2: Point.T;
  BEGIN
    lect.ocr.getSelection(selStart, selEnd);
    IF selStart = LecternOCR.NoSelPos THEN
      page := 0-lect.dir.origin;
    ELSE
      page := selStart.page;
    END;
    IF lect.gotoPage(page, lect.class, undoLink, TRUE) THEN
      IF selStart = selEnd THEN
        r := Rect.Empty;
      ELSE
        r := lect.ocr.getRect(selStart, lect.class);
      END;
      IF Rect.IsEmpty(r) THEN
        lect.iv.moveTo(lect.normalize(FirstSectorDelta));
      ELSE    
        (* Subtract enough from current delta to make SE visible, then
           add enough to make NW visible.  NW dominates. *)
        WITH se = Rect.SouthEast(r) DO
          d1 := Point.Sub(se, Rect.Project(VBT.Domain(lect.iv), se));
        END;
        WITH nw = Point.Sub(Rect.NorthWest(r), d1) DO
          d2 := Point.Sub(Rect.Project(VBT.Domain(lect.iv), nw), nw);
        END;
        lect.iv.moveTo(Point.Add(Point.Sub(lect.iv.getDelta(), d1), d2),
                       lect.animationSpeed,
                       lect.highlightMotion);
      END;
    END;
  END GotoSelection;

PROCEDURE GotoPageStart(lect: Lectern; page: PageNumber; noisy := TRUE) =
  (* LL = SELF *)
  BEGIN
    LOCK VBT.mu DO
      IF lect.gotoPage(page, lect.class, lect.linkToHere(), noisy) THEN
        lect.iv.moveTo(lect.normalize(FirstSectorDelta));
      END;
    END;
  END GotoPageStart;

PROCEDURE GotoPageEnd(lect: Lectern; page: PageNumber; noisy := TRUE) =
  (* LL = SELF *)
  BEGIN
    LOCK VBT.mu DO
      IF lect.gotoPage(page, lect.class, lect.linkToHere(), noisy) THEN
        lect.iv.moveTo(lect.normalize(LastSectorDelta));
      END;
    END;
  END GotoPageEnd;

PROCEDURE NextSector(lect: Lectern) =
  (* LL = lect *)
  VAR
    domain := VBT.Domain(lect.iv);
    oldDelta := lect.iv.getDelta();
    normalized: Point.T;
    PROCEDURE MoveHorizontal(v: INTEGER): BOOLEAN =
        (* Set "normalized" to next horizontal delta; TRUE if different *)
      BEGIN
      WITH adjustment = Point.T{h := Rect.HorSize(domain)-Overlap, v := v} DO
        normalized := lect.normalize(Point.Sub(oldDelta, adjustment));
      END;
      RETURN normalized.h < oldDelta.h
      END MoveHorizontal;
    PROCEDURE MoveVertical(h: INTEGER): BOOLEAN =
        (* Set "normalized" to next vertical delta; TRUE if different *)
      BEGIN
      WITH adjustment = Point.T{h := h, v := Rect.VerSize(domain)-Overlap} DO
        normalized := lect.normalize(Point.Sub(oldDelta, adjustment));
      END;
      RETURN normalized.v < oldDelta.v
      END MoveVertical;
    PROCEDURE MoveIt(sector: BOOLEAN) =
        (* Move to normalized delta or next page *)
      BEGIN
        IF sector THEN
          LOCK VBT.mu DO
            lect.iv.moveTo(normalized,
                           lect.animationSpeed,
                           lect.highlightMotion);
          END;
        ELSE
          lect.gotoPageStart(lect.page+1, FALSE);
        END;
      END MoveIt;
  BEGIN
    CASE lect.part OF
    | Partition.FullPage =>
        MoveIt(FALSE);
    | Partition.HorThenVer =>
        MoveIt(MoveHorizontal(0) OR MoveVertical(-10000000));
    | Partition.VerThenHor =>
        MoveIt(MoveVertical(0) OR MoveHorizontal(-10000000));
    | Partition.HorOnly =>
        MoveIt(MoveHorizontal(0));
    | Partition.VerOnly =>
        MoveIt(MoveVertical(0));
    END;
  END NextSector;

PROCEDURE PrevSector(lect: Lectern) =
  (* LL = lect *)
  VAR
    domain := VBT.Domain(lect.iv);
    oldDelta := lect.iv.getDelta();
    normalized: Point.T;
    PROCEDURE MoveHorizontal(v: INTEGER): BOOLEAN =
        (* Set "normalized" to prev horizontal delta; TRUE if different *)
      BEGIN
      WITH adjustment = Point.T{h := Overlap-Rect.HorSize(domain), v := v} DO
        normalized := lect.normalize(Point.Sub(oldDelta, adjustment));
      END;
      RETURN normalized.h > oldDelta.h
      END MoveHorizontal;
    PROCEDURE MoveVertical(h: INTEGER): BOOLEAN =
        (* Set "normalized" to prev vertical delta; TRUE if different *)
      BEGIN
      WITH adjustment = Point.T{h := h, v := Overlap-Rect.VerSize(domain)} DO
        normalized := lect.normalize(Point.Sub(oldDelta, adjustment));
      END;
      RETURN normalized.v > oldDelta.v
      END MoveVertical;
    PROCEDURE MoveIt(sector: BOOLEAN) =
        (* Move to normalized delta or prev page *)
      BEGIN
        IF sector THEN
          LOCK VBT.mu DO
            lect.iv.moveTo(normalized,
                           lect.animationSpeed,
                           lect.highlightMotion);
          END;
        ELSE
          lect.gotoPageEnd(lect.page-1, FALSE);
        END;
      END MoveIt;
  BEGIN
    CASE lect.part OF
    | Partition.FullPage =>
        MoveIt(FALSE);
    | Partition.HorThenVer =>
        MoveIt(MoveHorizontal(0) OR MoveVertical(10000000));
    | Partition.VerThenHor =>
        MoveIt(MoveVertical(0) OR MoveHorizontal(10000000));
    | Partition.HorOnly =>
        MoveIt(MoveHorizontal(0));
    | Partition.VerOnly =>
        MoveIt(MoveVertical(0));
    END;
  END PrevSector;

PROCEDURE ImageBody(contents: Images.RawContents): Rect.T =
    (* Returns the smallest white rectangle containing the central area of the
       image. *)
  VAR
    map := contents.map;
    raw := contents.raw;
    bounds := raw.bounds;
    levels := NEW(REF ARRAY OF Images.Gray, NUMBER(map^));
  <*INLINE*> PROCEDURE Grayed(h, v: INTEGER): Images.Gray =
      (* Returns a pixel of the image converted to gray *)
      (* Uses the standard brightness algorithm *)
    BEGIN
      RETURN levels[raw.get(Point.T{h := h, v := v})];
    END Grayed;
(* The procedure "Blurred" is the straightforward encoding of the blurring
   algorithm, but it's too slow.  It's replaced by code in DarkRow and
   DarkColumn, but it remains here for documentation.
  <*INLINE*> PROCEDURE Blurred(thisIndexH, thisIndexV: INTEGER): Images.Gray =
      (* Returns a pixel of the image converted to gray then blurred *)
      (* Convolves the image with a low-pass filter approximated by the
         matrix:
                  1 2 1
                  2 4 2
                  1 2 1
         *)
      (* Ignores edge problems.  *)
    BEGIN
      WITH
        prevIndexH = thisIndexH - 1,
        prevIndexV = thisIndexV - 1,
        nextIndexH = thisIndexH + 1,
        nextIndexV = thisIndexV + 1 DO
        RETURN ( Grayed(prevIndexH, prevIndexV) +
                 Grayed(thisIndexH, prevIndexV) * 2 +
                 Grayed(nextIndexH, prevIndexV) +
                 Grayed(prevIndexH, thisIndexV) * 2 +
                 Grayed(thisIndexH, thisIndexV) * 4 +
                 Grayed(nextIndexH, thisIndexV) * 2 +
                 Grayed(prevIndexH, nextIndexV) +
                 Grayed(thisIndexH, nextIndexV) * 2 +
                 Grayed(nextIndexH, nextIndexV) +
                 8 (* rounding *) ) DIV 16 (* normalize to 1 *);
      END;
    END Blurred;
*)
    <*INLINE*> PROCEDURE DarkRow(minH, maxH, v: INTEGER): BOOLEAN =
      (* Returns TRUE if [minH..maxH] of row v have a dark pixel *)
      VAR prev, this, next: INTEGER := 255 + 255*2 + 255;
      BEGIN
        FOR h := minH TO maxH DO
(*        IF Blurred(h, v) < VeryPale THEN RETURN TRUE END;*)
          WITH nextIndexH = h+1 DO
            next := Grayed(nextIndexH, v-1) +
                    Grayed(nextIndexH, v) * 2 +
                    Grayed(nextIndexH, v+1);
          END;
          IF (prev + this*2 + next + 8) DIV 16 < VeryPale THEN
            RETURN TRUE;
          END;
          prev := this; this := next;
        END;
        RETURN FALSE;
      END DarkRow;
   <*INLINE*>  PROCEDURE DarkColumn(h, minV, maxV: INTEGER): BOOLEAN =
      (* Returns TRUE if [minV..maxV] of column h have a dark pixel *)
      VAR prev, this, next: INTEGER := 255 + 255*2 + 255;
      BEGIN
        FOR v := minV TO maxV DO
(*          IF Blurred(h, v) < VeryPale THEN RETURN TRUE END;*)
          WITH nextIndexV = v+1 DO
            next := Grayed(h-1, nextIndexV) +
                    Grayed(h, nextIndexV) * 2 +
                    Grayed(h+1, nextIndexV);
          END;
          IF (prev + this*2 + next + 8) DIV 16 < VeryPale THEN
            RETURN TRUE;
          END;
          prev := this; this := next;
        END;
        RETURN FALSE;
      END DarkColumn;
  VAR
    res: Rect.T;
    progressH, progressV, checkH, checkV: BOOLEAN;
  CONST
    VeryPale = 242; (* roughly 95% *)
  BEGIN
    FOR i := 0 TO LAST(map^) DO levels[i] := Images.GrayFromRGB(map[i]) END;
    IF Rect.IsEmpty(bounds) THEN RETURN Rect.Empty END;
    (* Initialize to a rectangle of moderate size, centered *)
    res := Rect.Center(Rect.FromSize((2 * Rect.HorSize(bounds)) DIV 3,
                                     (2 * Rect.VerSize(bounds)) DIV 3),
                       Rect.Middle(bounds));
    IF Rect.IsEmpty(res) THEN RETURN bounds END;
    (* Expand outwards, stopping at white-ish edges *)
    (* Nested loops typically touch less pixels *)
    checkV := TRUE; (* initially, need to check V coords after checking H *)
    LOOP
      LOOP
        progressH := FALSE; (* whether H coords are changing *)
        IF res.east < bounds.east-1 THEN
          IF DarkColumn(res.east-1, res.north, res.south-1) THEN
            INC(res.east); progressH := TRUE;
          END;
        END;
        IF res.west > bounds.west THEN
          IF DarkColumn(res.west, res.north, res.south-1) THEN
            DEC(res.west); progressH := TRUE;
          END;
        END;
        IF progressH THEN checkV := TRUE ELSE EXIT END;
      END;
      checkH := FALSE; (* whether we will need to re-check H coords *)
      IF NOT checkV THEN EXIT END;
      LOOP
        progressV := FALSE; (* whether V coords are changing *)
        IF res.south < bounds.south-1 THEN
          IF DarkRow(res.west, res.east-1, res.south-1) THEN
            INC(res.south); progressV := TRUE;
          END;
        END;
        IF res.north > bounds.north THEN
          IF DarkRow(res.west, res.east-1, res.north) THEN
            DEC(res.north); progressV := TRUE;
          END;
        END;
        IF progressV THEN checkH := TRUE; ELSE EXIT END;
      END;
      checkV := FALSE; (* whether we will need to re-check V coords *)
      IF NOT checkH THEN EXIT END;
    END;
    RETURN Rect.Inset(res, 1); (* Return the inner non-pale area *)
  END ImageBody;

PROCEDURE ComputeViewArea(lect: Lectern) =
    (* LL = VBT.mu *)
    (* Automatically select view area, based on thumbnail *)
  BEGIN
    <* ASSERT lect.pm # NIL *>
    WITH image = lect.thumbnailVBT.acquire(lect.page+lect.dir.origin) DO
      IF image # NIL THEN
        TRY
          lect.lasso := ImageBody(image.contents());
          lect.lassoDomain := image.domain(NIL);
          IF Rect.IsEmpty(lect.lasso) OR Rect.IsEmpty(lect.lassoDomain) THEN
            lect.lasso := Rect.FromEdges(0, 1, 0, 1);
            lect.lassoDomain := lect.lasso;
          END;
          lect.viewArea.setSelected(lect.lasso);
        EXCEPT Images.Error, Thread.Alerted => (* ignore *)
        END; 
      END;
    END;
  END ComputeViewArea;

PROCEDURE SetAutoViewArea(lect: Lectern; auto: BOOLEAN) =
    (* LL = VBT.mu *)
  BEGIN
    IF auto AND lect.pm # NIL THEN lect.computeViewArea() END;
    lect.autoViewArea := auto;
    IF auto THEN
      FormsVBT.MakeSelected(lect.viewingFV, "AutoViewArea");
    ELSE
      FormsVBT.MakeSelected(lect.viewingFV, "ManualViewArea");
    END;
  END SetAutoViewArea;

PROCEDURE Print(lect: Lectern; VAR time: VBT.TimeStamp) =
  (* LL = lect *)
  VAR
    toPrinter: BOOLEAN;
    image := NEW(ImageRd.T);
    wr: Wr.T;
    fromPage, toPage: PageNumber;
    command: TEXT;
    fileName: TEXT;
    action: TEXT;
    dest: TEXT;
    images, epsf, binary: BOOLEAN;
  PROCEDURE Exists(name: TEXT): BOOLEAN =
    (* LL = any, but preferably < VBT.mu *)
    (* Returns TRUE if the file exists, FALSE if not or if error. *)
    BEGIN
      TRY
        FS.OpenFileReadonly(name).close();
        RETURN TRUE
      EXCEPT OSError.E =>
        RETURN FALSE
      END;
    END Exists;
  PROCEDURE ReplaceOK(name: TEXT): BOOLEAN =
    (* LL < VBT.mu *)
    (* Asks for confirmation that it's OK to replace existing "name" *)
    BEGIN
      RETURN lect.confirm("Replace existing \"" & name & "\"?",
                          "Replace", "Cancel", time);
    END ReplaceOK;
  BEGIN
    LOCK VBT.mu DO
      toPrinter := FormsVBT.IsSelected(lect.popupFV, "PrintToPrinter");
      images := FormsVBT.IsSelected(lect.popupFV, "Images");
      epsf := FormsVBT.IsSelected(lect.popupFV, "EPSFFiles") AND
                  images AND NOT toPrinter;
      binary := FormsVBT.IsSelected(lect.popupFV, "Binary") AND images;
      fromPage := MAX(FormsVBT.GetInteger(lect.popupFV, "FileFrom"),
                      0-lect.dir.origin);
      toPage := MIN(FormsVBT.GetInteger(lect.popupFV, "FileTo"),
                    LAST(lect.dir.pages^)-lect.dir.origin);
      command := FormsVBT.GetText(lect.popupFV, "PrintCommand");
      fileName := GetFileName(lect);
      IF lect.path = NIL THEN
        lect.error("This viewer has no document in it");
        RETURN
      END;
    END;
    IF NOT toPrinter THEN
      IF epsf THEN
        FOR page := fromPage TO toPage DO
          IF Exists(fileName & "." & Fmt.Int(page)) THEN
            IF ReplaceOK(fileName & "." & "*") THEN
              EXIT;
            ELSE
              RETURN;
            END;
          END;
        END;
      ELSE
        IF Exists(fileName) AND NOT ReplaceOK(fileName) THEN RETURN END;
      END;
    END;
    IF images THEN
      WITH diskSpace = (toPage-fromPage+1) * 250 (* KBytes *) DO
        IF diskSpace > 50000 THEN
          IF NOT lect.confirm("This operation will use roughly " &
                              Fmt.Int(diskSpace DIV 1000) &
                     " MBytes of disk space. Are you sure you want to do it?",
                             "Continue", "Cancel", time) THEN
            RETURN;
          END;
        END;
      END;
    END;
    LOCK VBT.mu DO
      IF images THEN
        lect.progressVBT.set(0.0);
        IF toPrinter THEN
          action := "Printing via \"" & command & "\"";
        ELSE
          action := "Saving ";
          IF epsf THEN action := action & "EPSF " END;
          action := action & "as \"" & fileName;
          IF epsf THEN action := action & ".*" END;
          action := action & "\"";
        END;
        FormsVBT.PutText(lect.popupFV, "Action", action);
        DoPopup(lect, "ProgressDlg");
      ELSIF lect.dir.original.start <= 0 THEN
        lect.error("This document has no original PostScript");
        RETURN
      END;
    END;
    TRY (*EXCEPT*)
      IF NOT epsf THEN
        IF toPrinter THEN
          dest := lect.printFile;
        ELSE
          dest := fileName;
        END;
        wr := FileWr.Open(dest);
      END;
      TRY (*FINALLY DeleteFile*)
        TRY (*FINALLY Wr.Close *)
          IF images THEN
            IF NOT epsf THEN
              Wr.PutText(wr, "%!PS-Adobe-3.0\n");
              Wr.PutText(wr, "%%Creator: (Lectern " & Version & ")\n");
              Wr.PutText(wr, "%%Pages: " & Fmt.Int(toPage-fromPage+1) & "\n");
              Wr.PutText(wr, "%%PageOrder: Ascend\n");
              Wr.PutText(wr, "%%EndComments\n");
            END;
            FOR page := fromPage TO toPage DO
              WITH entry =
                   lect.dir.pages[page+lect.dir.origin][LecternDoc.Class.Print] DO
                LOCK VBT.mu DO
                  lect.progressVBT.set((FLOAT(page-fromPage)+0.5) /
                    FLOAT(toPage-fromPage+1));
                  IF entry.start <= 0 THEN
                    lect.error("There's no print image for page " &
                      Fmt.Int(page));
                    EXIT
                  END;
                END;
                EVAL image.init(lect.rd, entry.start, entry.length);
                IF epsf THEN
                  dest := fileName & "." & Fmt.Int(page);
                  wr := FileWr.Open(dest);
                  TRY
                    image.toEPSF(wr, binary);
                    Wr.PutText(wr, "%%EOF\n");
                  FINALLY
                    Wr.Close(wr);
                  END;
                ELSE
                  Wr.PutText(wr, "\n%%Page: " & Fmt.Int(page) & " " &
                    Fmt.Int(page-fromPage+1) & "\n");
                  Wr.PutText(wr, "/lecternSave save def\n");
                  Wr.PutText(wr, "/showpage { } def\n");
                  Wr.PutText(wr, "%%BeginDocument: (EPSF of page " &
                  Fmt.Int(page) & ")\n");
                  image.toEPSF(wr, binary);
                  Wr.PutText(wr, "%%EndDocument\n");
                  Wr.PutText(wr, "lecternSave restore\n");
                  Wr.PutText(wr, "showpage\n");
                END;
              END;
            END;
            IF NOT epsf THEN
              Wr.PutText(wr, "\n%%Trailer\n");
              Wr.PutText(wr, "%%EOF\n");
            END;
          ELSE
            EVAL LecternDoc.CopyComponent(lect.rd, lect.dir.original, wr);
          END;
        FINALLY
          IF NOT epsf THEN Wr.Close(wr) END;
        END;
        IF toPrinter THEN
          WITH output = RunSub(Tokenise(command & " " & dest)^) DO
            IF NOT Text.Empty(output) THEN
              LOCK VBT.mu DO lect.error("Response was: " & output) END;
            END;
          END;
        END;
      FINALLY
        IF toPrinter THEN
          TRY
            FS.DeleteFile(dest);
          EXCEPT OSError.E => (* ignore *)
          END;
        END;
      END;
    EXCEPT
    | Rd.Failure =>
        LOCK VBT.mu DO
          lect.error("Failure while reading document");
        END;
    | Wr.Failure =>
        LOCK VBT.mu DO
          lect.error("Failure while writing to \"" & dest & "\"");
        END;
    | OSError.E =>
        LOCK VBT.mu DO
          lect.error("Couldn't open \"" & dest & "\"");
        END;
    | Images.Error(msg) =>
        LOCK VBT.mu DO
          lect.error(msg);
        END;
    | Thread.Alerted =>
    END;
    LOCK VBT.mu DO
      (* Remove progress dialog *)
      IF NOT lect.showingError THEN DoPopDown(lect, 0) END;
    END;
  END Print;

EXCEPTION BadPath(TEXT);
  (* Raised by Canonicalize *)

PROCEDURE BeginsWith(a, b: Pathname.Arcs): BOOLEAN =
  (* Returns TRUE iff b is an initial subset of a *)
  VAR
    bSize := b.size();
  BEGIN
    IF a.size() < bSize THEN RETURN FALSE END;
    FOR i := 0 TO bSize-1 DO
      IF NOT Text.Equal(a.get(i), b.get(i)) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END BeginsWith;

PROCEDURE Canonicalize(path: Pathname.T): Pathname.T
                      RAISES { BadPath, OSError.E } =
    (* Convert relative path into canonical form, by replacing a prefix of the
       absolute-ized path that matches $HOME with the value of $HOME. *)
    (* Raises OSError.E if path is inaccessible.  Raises BadPath if
       it is unacceptable, or if $HOME is unsuitable. *)
  VAR
    home := Env.Get("HOME");
    absPath, absHome: Pathname.T;
    homeArcs, absPathArcs, absHomeArcs: Pathname.Arcs;
  BEGIN
    IF Pathname.Absolute(path) THEN
      absPath := path;
    ELSE
      absPath := FS.GetAbsolutePathname(path);
    END;
    IF home = NIL THEN
      RAISE BadPath("You must define the environment variable $HOME");
    END;
    IF NOT Pathname.Absolute(home) THEN
      RAISE BadPath("$HOME must be an absolute pathname");
    END;
    TRY
      homeArcs := Pathname.Decompose(home);
      absHome := FS.GetAbsolutePathname(home);
      absHomeArcs := Pathname.Decompose(absHome);
    EXCEPT OSError.E, Pathname.Invalid =>
      RAISE BadPath("$HOME must specify an accessible directory");
    END;
    TRY
      absPathArcs := Pathname.Decompose(absPath);
    EXCEPT Pathname.Invalid =>
      RAISE BadPath("Syntax error in \"" & absPath & "\"");
    END;
    IF BeginsWith(absPathArcs, absHomeArcs) THEN
      absPathArcs := TextSeq.Cat(homeArcs,
                                 TextSeq.Sub(absPathArcs,
                                             absHomeArcs.size(), 
                                             absPathArcs.size()-
                                               absHomeArcs.size()));
    END;
    <*FATAL Pathname.Invalid*> BEGIN RETURN Pathname.Compose(absPathArcs) END;
  END Canonicalize;

PROCEDURE InstallDoc(lect: Lectern; path: TEXT;
                     rd, ocrRd: Rd.T;
                     READONLY dir: LecternDoc.Dir;
                     outline: Links.LinkList;
                     time: VBT.TimeStamp;
                     from, for: INTEGER := -1;
                     selStart, selEnd := LecternOCR.NoSelPos)
                    RAISES { Thread.Alerted } =
  (* LL = lect *)
  (* Installs an open document into an existing viewer.  There are no errors!
     This is really an internal subroutine of ReadDoc and SaveDoc.
       lect             = the viewer
       path             = path by which the document was opened
       rd, ocrRd        = independent readers on the document
       dir              = the document's directory
       outline          = the docuiment's outline
       time             = most recent event time
       from, for        = word range, counting from start of document
       selStart, selEnd = alternative form of word range
     *)
  VAR
    undoLink: Links.Link;
  BEGIN
    LOCK VBT.mu DO
      undoLink := lect.linkToHere();
      lect.path := path;
      lect.rd := rd;
      lect.dir := dir;
      UpdateLinksBrowser(lect.outlineVBT, outline);
      lect.outline := outline;
      lect.modified := FALSE;
      lect.ocr := NEW(LecternOCR.T).init(ocrRd, dir, lect.iv,
                                     CursorFromRsrc("hand.1.pbm", "hand.2.pbm",
                                                    Point.T{7,7},
                                                    lect.rsrcPath));
      lect.find := NEW(Find.T).init(lect.dir, lect.ocr, lect.findFV);
      (* Choose a name for the window decoration, not equal to other viewers *)
      VAR i := 1;
      PROCEDURE MakeName(suffix: TEXT): TEXT =
        VAR last := Pathname.Last(lect.path);
        BEGIN
          IF i = 1 THEN
            RETURN last & suffix;
          ELSE
            RETURN last & " <" & Fmt.Int(i) & ">" & suffix;
          END;
        END MakeName;
      VAR this := lect.nextV; candidate := MakeName(ImageSuffix);
      BEGIN
        WHILE this # lect DO
          IF Text.Equal(candidate, this.imageTitleBase) THEN
            INC(i);
            candidate := MakeName(ImageSuffix);
            this := lect;
          END;
          this := this.nextV;
        END;
        lect.imageTitleBase := candidate;
        IF lect.server = NIL THEN
          lect.imageTitle := lect.imageTitleBase
        ELSE
          lect.imageTitle := lect.imageTitleBase & ServerSuffix;
        END;
        lect.thumbnailTitle := MakeName(ThumbnailSuffix);
      END;
      IF lect.iw # NIL THEN
        Retitle(lect.iw, lect.imageTitle);
      END;
      IF lect.thumbnailW # NIL THEN
        Retitle(lect.thumbnailW, lect.thumbnailTitle);
      END;
      FOR i := 0 TO LAST(lect.dir.pages^) DO
        IF lect.dir.pages[i][LecternDoc.Class.Print].start > 0 THEN
          lect.hasPrintImages := TRUE;
          EXIT;
        END;
      END;
      lect.hasOriginal := (lect.dir.original.start > 0);
      lect.thumbnailVBT.setContents(lect.rd, lect.dir, ThumbnailGamma);
      IF lect.popupFV # NIL THEN
        IF lect.hasPrintImages THEN
          FormsVBT.MakeActive(lect.popupFV, "Images");
          FormsVBT.MakeSelected(lect.popupFV, "Images");
          lect.applyOp(Op.FileUseImages, 0);
        END;
        IF lect.hasOriginal THEN
          FormsVBT.MakeActive(lect.popupFV, "Original");
          FormsVBT.MakeSelected(lect.popupFV, "Original");
          lect.applyOp(Op.FileUseOriginal, 0);
        ELSE
          FormsVBT.MakeDormant(lect.popupFV, "Original");
        END;
        FormsVBT.PutInteger(lect.popupFV, "FileFrom",
                            0-lect.dir.origin);
        FormsVBT.PutInteger(lect.popupFV, "FileTo",
                            LAST(lect.dir.pages^)-lect.dir.origin);
      END;
      (* Be sure to discard pages from prior document, if any *)
      lect.freeImage(lect.prev);
      lect.freeImage(lect.pm);
      lect.freeImage(lect.next);
      (* Set up initial position *)
      lect.setAutoViewArea(TRUE);
      IF lect.gotoPage(0-lect.dir.origin, lect.class, undoLink, TRUE) THEN
        VAR sel := FALSE;
        BEGIN
          IF from >= 0 THEN
            sel := lect.ocr.selectWords(from, for, time,
                                -1-lect.dir.origin (* so nothing will paint *),
                                lect.class);
          ELSIF selStart # LecternOCR.NoSelPos THEN
            lect.ocr.setSelection(selStart, selEnd, time,
                                -1-lect.dir.origin (* so nothing will paint *),
                                lect.class);
            sel := TRUE;
          END;
          lect.usePart(Partition.VerThenHor);
          IF sel THEN
            (* Jump to the selection; but don't record another undo record *)
            lect.gotoSelection(Links.NoLink);
          ELSE
            lect.iv.moveTo(lect.normalize(FirstSectorDelta));
          END;
        END;
      END;
    END;
  END InstallDoc;

PROCEDURE CloseDoc(lect: Lectern; VAR time: VBT.TimeStamp): BOOLEAN =
  BEGIN
    IF lect.modified THEN
      IF NOT lect.confirm("Discard changes to \"" & lect.path & "\"?",
                          "Discard", "Cancel", time) THEN
        RETURN FALSE;
      END;
    END;
    IF lect.path # NIL THEN
      lect.links.appendLink(Links.Class.Bookmark, lect.path, lect.dir,
                            lect.ocr, lect.page);
    END;
    RETURN TRUE
  END CloseDoc;

PROCEDURE ReadDoc(lect: Lectern; link: Links.Link; time: VBT.TimeStamp;
                  from, for: INTEGER := -1) =
  (* LL = lect *)
  <*FATAL Thread.Alerted*>
  (* Reads the document specified by link into lect, and make selection *)
  VAR
    rd, ocrRd: Rd.T := NIL;
    dir: LecternDoc.Dir;
    outline: Links.LinkList;
    path := link.file;
    newClass: LecternDoc.Class;
  PROCEDURE DisposeRd() =
    BEGIN
      IF rd # NIL THEN
        TRY Rd.Close(rd); rd := NIL; EXCEPT Rd.Failure => END;
      END;
    END DisposeRd;
  PROCEDURE CheckClass(c: LecternDoc.Class): BOOLEAN =
    BEGIN
      IF dir.pages[0][c].start > 0 THEN
        newClass := c;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END CheckClass;
  BEGIN
    Thread.Pause(0.25D0); (* let image window paint *)
    TRY
      lect.closeWanted := FALSE; (* abandon any pending close *)
      lect.closed := FALSE; (* all paths re-install .iw *)
      path := Canonicalize(link.file);
      rd := FileRd.Open(path);
      LOCK VBT.mu DO PutFileName(lect, path) END;
      LecternDoc.ReadDir(rd, dir);
      IF NUMBER(dir.pages^) = 0 THEN
        LOCK VBT.mu DO lect.error("\"" & path & "\" has no pages") END;
        DisposeRd();
      ELSE
        (* Ensure the image class is usable *)
        IF CheckClass(lect.class) OR
           CheckClass(LecternDoc.Class.Normal) OR
           CheckClass(LecternDoc.Class.Small) OR
           CheckClass(LecternDoc.Class.Large) THEN
          lect.class := newClass;
          outline := Links.ReadOutline(rd, dir, path);
          ocrRd := FileRd.Open(path);
          IF lect.closeDoc(time) THEN
            InstallDoc(lect, path, rd, ocrRd, dir, outline,
                       time, from, for, link.selStart, link.selEnd);
          ELSE
            DisposeRd(); rd := ocrRd; DisposeRd();
          END;
        ELSE
          LOCK VBT.mu DO
            lect.error("\"" & path & "\" has no image for its first page");
          END;
          DisposeRd();
        END;
      END;
    EXCEPT
    | BadPath(msg) =>
        LOCK VBT.mu DO lect.error(msg) END;
    | OSError.E =>
        LOCK VBT.mu DO
          lect.error("Can't open \"" & path & "\"");
        END;
    | Rd.Failure, Rd.EndOfFile =>
        LOCK VBT.mu DO
          lect.error("Error reading \"" & path & "\"");
        END;
        DisposeRd();
    | LecternDoc.NotLectern =>
        LOCK VBT.mu DO
          lect.error("\"" & path & "\" isn't a lectern file");
        END;
        RETURN;
    END;
  END ReadDoc;

PROCEDURE Request(lect: Lectern; request: REF ARRAY OF TEXT; error: TEXT;
                  time: VBT.TimeStamp) =
    (* LL = lect *)
    (* Service the LecternClient error message or request *)
    PROCEDURE ToInt(n: INTEGER): INTEGER =
      BEGIN
        IF n > LAST(request^) THEN
          RETURN -1
        ELSE
          TRY
            RETURN Scan.Int(request[n])
          EXCEPT
            Lex.Error, FloatMode.Trap =>
              RETURN -1
          END;
        END;
      END ToInt;
  BEGIN
    LOCK VBT.mu DO
      EVAL ShowWindow(lect.iw, lect.iw, lect.imageTitle);
    END;
    IF error # NIL THEN
      lect.error(error);
    ELSIF request # NIL THEN
      IF NUMBER(request^) >= 1 THEN
        lect.readDoc(Links.Link{file := request[0]}, time, ToInt(1), ToInt(2));
      END;
    END;
  END Request;

PROCEDURE SaveDoc(lect: Lectern; VAR time: VBT.TimeStamp) =
    (* LL = SELF *)
    (* Write a new version of the document, presumably because the
       document's outline or hypertext changed *)
  VAR
    wr: Wr.T;
    dir := lect.dir;
    dirStart: CARDINAL;
    finalName, tempName: TEXT := NIL;
    newRd, newOcrRd: Rd.T;
  BEGIN
    <*ASSERT lect.path # NIL *>
    TRY
      IF lect.confirm("Save changes to \"" & lect.path & "\"?",
                      "Save", "Cancel", time) THEN
        finalName := lect.path;
        tempName := TempFiles.Get(Pathname.Prefix(finalName), ",lectern-");
        wr := FileWr.Open(tempName);
        TempFiles.Note(tempName);
        TRY
          LOCK VBT.mu DO
            lect.progressVBT.set(0.0);
            FormsVBT.PutText(lect.popupFV, "Action",
                             "Writing \"" & finalName & "\" ...");
            DoPopup(lect, "ProgressDlg");
          END;
          LecternDoc.WriteHeader(wr);
          dir.pages := NEW(LecternDoc.DirPages, NUMBER(lect.dir.pages^));
          FOR i := 0 TO LAST(dir.pages^) DO
            IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
            LOCK VBT.mu DO
              lect.progressVBT.set((FLOAT(i)+0.5)/FLOAT(NUMBER(dir.pages^)));
            END;
            Thread.Pause(0.010D0); (* let it repaint *)
            WITH src = lect.dir.pages[i], dest = dir.pages[i] DO
              FOR j := FIRST(LecternDoc.Class) TO LAST(LecternDoc.Class) DO
                dest[j] := LecternDoc.CopyComponent(lect.rd, src[j], wr);
              END;
            END;
          END;
          Links.PutOutline(wr, dir, lect.path, lect.outline);
          dir.original := LecternDoc.CopyComponent(lect.rd,
                                                   lect.dir.original, wr);
          dirStart := Wr.Index(wr);
          LecternDoc.WriteDir(wr, dir);
          LecternDoc.WriteTrailer(wr, dirStart);
        FINALLY
          LOCK VBT.mu DO
            (* Remove progress dialog *)
            IF NOT lect.showingError THEN DoPopDown(lect, time) END;
          END;
          TRY
            Wr.Close(wr);
          EXCEPT Wr.Failure, Thread.Alerted =>
          END;
          TempFiles.Forget(tempName);
        END;
        (* Now we're just about ready to commit.  But first, make sure
           we'll be able to read the new document *)
        TRY
          newRd := FileRd.Open(tempName);
          newOcrRd := FileRd.Open(tempName);
          TRY
            FS.Rename(tempName, finalName);
            LOCK VBT.mu DO lect.modified := FALSE END;
            tempName := NIL;
            InstallDoc(lect, finalName, newRd, newOcrRd, dir,
                       lect.outline, time);
          EXCEPT OSError.E =>
            LOCK VBT.mu DO lect.error("Can't rename temp file") END;
          END;
        EXCEPT OSError.E =>
          LOCK VBT.mu DO lect.error("Can't re-read new file") END;
        END;
      END;
    EXCEPT
      | Thread.Alerted =>
      | Rd.Failure =>
         LOCK VBT.mu DO lect.error("Can't read source file") END;
      | OSError.E =>
         LOCK VBT.mu DO lect.error("Can't open file for writing") END;
      | Wr.Failure =>
         LOCK VBT.mu DO lect.error("Can't write destination file") END;
    END;
    IF tempName # NIL THEN
      TRY FS.DeleteFile(tempName) EXCEPT OSError.E => END;
    END;
  END SaveDoc;

PROCEDURE Close(lect: Lectern; VAR time: VBT.TimeStamp) =
    (* LL = SELF *)
  BEGIN
    IF NOT lect.closeDoc(time) THEN
      LOCK VBT.mu DO lect.closeWanted := FALSE END;
      RETURN;
    END;
    LOCK VBT.mu DO
      lect.closeWanted := FALSE;
      IF lect.showingError THEN
        DoPopup(lect, "ErrorDlg"); (* just in case; also installs window *)
        RETURN;
      END;
      lect.closed := TRUE;
      IF lect.iw # NIL THEN HideWindow(lect.iw) END;
      IF lect.thumbnailW # NIL THEN HideWindow(lect.thumbnailW) END;
      (* If we're closing the only visible window, then exit *)
      VAR this := lect;
      BEGIN
        WHILE this.closed DO
          this := this.nextV;
          IF this = lect THEN Process.Exit(0); (* I.e., quit *) END;
        END;
      END;
      (* Remove the document *)
      lect.iv.put(Images.Empty, lect.bg);
      lect.viewArea.put(Images.Empty, lect.bg);
      lect.freeImage(lect.pm);
      lect.freeImage(lect.next);
      lect.freeImage(lect.prev);
      lect.spare1 := NIL;
      lect.spare2 := NIL;
      lect.spare3 := NIL;
      IF lect.path # NIL THEN
        lect.path := NIL;
        TRY
          Rd.Close(lect.rd);
        EXCEPT
        | Rd.Failure, Thread.Alerted =>
        END;
        lect.rd := NIL;
        lect.ocr.close();
      END;
      (* Leave a hook for the links dialog, in case the diary changes *)
      IF lect.links.lect = lect THEN
        lect.links.lect := lect.nextV;
      END;
      IF lect.server = NIL THEN
        (* We're not the server viewer, so we're really going away *)
        (* Remove us from the ring of viewers *)
        lect.prevV.nextV := lect.nextV;
        lect.nextV.prevV := lect.prevV;
        lect.nextV := NIL;
        lect.prevV := NIL;
      ELSE
        lect.iv.put(DefaultImage(lect), lect.bg);
      END;
    END;
  END Close;

PROCEDURE UpdateLinksBrowser(browser: ListVBT.T; list: Links.LinkList) =
    (* Load given links into the given browser *)
    (* LL = VBT.mu *)
  BEGIN
    WITH old = browser.count(), new = NUMBER(list^) DO
      IF new > old THEN
        browser.insertCells(old, new-old);
      ELSE
        browser.removeCells(new, old-new);
      END;
      FOR i := 0 TO new-1 DO browser.setValue(i, list[i].name) END;
    END;
   END UpdateLinksBrowser;

PROCEDURE UpdateLinks(t: MyLinks; class: Links.Class; list: Links.LinkList) =
    (* Callback from the Links module: update viewers as appropriate. *)
    (* LL = VBT.mu *)
  VAR
    this := t.lect;
    selfOnly: BOOLEAN;
    browser: ListVBT.T;
  BEGIN
    LOOP
      CASE class OF
      | Links.Class.Bookmark =>
          this.bookmark := list;
          browser := this.bookmarkVBT;
          selfOnly := FALSE;
      | Links.Class.Diary =>
          this.diary := list;
          browser := this.diaryVBT;
          selfOnly := FALSE;
      | Links.Class.Outline =>
          this.outline := list;
          browser := this.outlineVBT;
          selfOnly := TRUE;
          this.modified := TRUE;
      | Links.Class.Hyper =>
          RETURN
      END;
      UpdateLinksBrowser(browser, list);
      this := this.nextV;
      IF selfOnly OR this = t.lect THEN EXIT END;
    END;
  END UpdateLinks;

PROCEDURE JumpToLink(lect: Lectern; READONLY link: Links.Link;
                     VAR time: VBT.TimeStamp;
                     event: AnyEvent.T) RAISES { Thread.Alerted } =
    (* Jump to given link; in new viewer iff right mouse *)
    (* LL = SELF *)
  VAR
    newViewer := FALSE;
    undoLink: Links.Link;
  BEGIN
    IF event # NIL THEN
      TYPECASE event OF
      | AnyEvent.Mouse(m) =>
          newViewer := m.mouse.whatChanged = VBT.Modifier.MouseR OR
                       VBT.Modifier.Shift IN m.mouse.modifiers;
      ELSE
      END;
    END;
    IF link # Links.NoLink THEN
      IF newViewer THEN
        LOCK VBT.mu DO EVAL lect.createViewer(link, time) END;
      ELSIF (link.file = NIL) OR
             (lect.path # NIL AND Text.Equal(link.file, lect.path)) THEN
        LOCK VBT.mu DO
          undoLink := lect.linkToHere();
          lect.ocr.setSelection(link.selStart, link.selEnd, time,
                                lect.page, lect.class);
          lect.gotoSelection(undoLink);
        END;
      ELSE
        (* Load and jump in one go, to avoid extra repaint *)
        lect.readDoc(link, time := time);
      END;
    END;
  END JumpToLink;

PROCEDURE LinkClassFromOp(op: Op): Links.Class =
  (* LL = any *)
  BEGIN
    CASE op OF
    | Op.BookmarkNew, Op.BookmarkEdit => RETURN Links.Class.Bookmark;
    | Op.DiaryNew, Op.DiaryEdit => RETURN Links.Class.Diary;
    | Op.OutlineNew, Op.OutlineEdit => RETURN Links.Class.Outline;
    | Op.NewLink, Op.EditLink, Op.JumpLink => RETURN Links.Class.Bookmark;
    ELSE
      <* ASSERT FALSE *>
    END;
  END LinkClassFromOp;

PROCEDURE DoLinkDlg(lect: Lectern; op: Op; VAR time: VBT.TimeStamp)
                RAISES { Thread.Alerted, Links.Error } =
  (* LL = SELF *)
  VAR
    variant: Links.Variant;
    class := LinkClassFromOp(op);
    response: Op;
    responseEvent: AnyEvent.T;
    fv: LecternFVClass;
  BEGIN
    IF lect.path # NIL THEN SetAndGotoSelection(lect, time) END;
    CASE op OF
    | Op.NewLink, Op.BookmarkNew, Op.DiaryNew, Op.OutlineNew =>
        IF lect.path = NIL THEN RETURN END;
        variant := Links.Variant.Define;
    | Op.EditLink, Op.BookmarkEdit, Op.DiaryEdit, Op.OutlineEdit =>
        variant := Links.Variant.Edit;
    | Op.JumpLink =>
        variant := Links.Variant.Jump;
    ELSE
      <* ASSERT FALSE *>
    END;
    IF class = Links.Class.Outline AND lect.path = NIL THEN
      LOCK VBT.mu DO lect.beep() END;
    ELSE
      LOCK VBT.mu DO lect.links.lect := lect END; (* it's our outline *)
      fv := lect.links.popup(variant,
                             class,
                             lect.imageTitle,
                             lect.path,
                             lect.dir,
                             lect.outline,
                             lect.ocr,
                             time);
      LOCK VBT.mu DO
        fv.firstPopup := TRUE; (* Force centering *)
        ShowZVisible(lect, fv);
        WITH focusV = FormsVBT.GetVBT(fv, "LinkName") DO
          IF TextPort.TryFocus(focusV, time) THEN
            TextPort.Select(v := focusV, time := time, replaceMode := TRUE);
          END;
         END;
        response := lect.doModalLocked(time);
        responseEvent := lect.responseEvent;
        HideZ(lect, fv);
        IF lect.iw # NIL THEN
          IF lect.iw.childFocus # NIL THEN lect.iw.takeFocus(time) END;
        END;
      END;
      JumpToLink(lect, lect.links.popDown(response=Op.Yes), time,
                 responseEvent);
    END;
  END DoLinkDlg;

PROCEDURE AdjustLinkTiles(lect: Lectern; op: Op) =
  <*FATAL MultiSplit.NotAChild*>
  (* Zoom, normalize or shrink a tile *)
  (* LL = VBT.mu *)
  VAR
    tile: Tile;
    pred: VBT.T := NIL;
  BEGIN
    CASE op OF
    | Op.BookmarkZoom =>
        tile := Tile.Bookmark;
    | Op.DiaryZoom =>
        tile := Tile.Diary;
    | Op.OutlineZoom =>
        tile := Tile.Outline;
    | Op.ThumbnailZoom =>
        tile := Tile.Thumbnail;
    ELSE <*ASSERT FALSE*>
    END;
    IF lect.tileZoomed[tile] THEN
      FOR t := FIRST(Tile) TO LAST(Tile) DO
        IF t # tile THEN SetTileShape(lect, t, FlexVBT.FixedRange) END;
      END;
      FOR t := FIRST(Tile) TO LAST(Tile) DO
        IF VBT.Parent(lect.tileVBT[t]) = NIL THEN
          MultiSplit.Insert(lect.tileSplitter, pred, lect.tileVBT[t]);
        END;
        pred := lect.tileVBT[t];
      END;
      (* Force HVSplit to redisplay with the new sizes, then change them *)
      VBTClass.Redisplay(lect.tileSplitter);
      FOR t := FIRST(Tile) TO LAST(Tile) DO
        IF t # tile THEN SetTileShape(lect, t, FlexVBT.DefaultRange) END;
      END;
    ELSE
      FOR t := FIRST(Tile) TO LAST(Tile) DO
        IF t # tile AND VBT.Parent(lect.tileVBT[t]) # NIL THEN
          MultiSplit.Delete(lect.tileSplitter, lect.tileVBT[t]);
        END;
      END;
    END;
    lect.tileZoomed[tile] := NOT lect.tileZoomed[tile];
  END AdjustLinkTiles;

TYPE ForkedOpClosure = Thread.Closure OBJECT
      lect: Lectern;
      op: Op;
      time: VBT.TimeStamp;
      event: AnyEvent.T;
    OVERRIDES
      apply := ForkedOp;
    END;

PROCEDURE ForkedOp(cl: ForkedOpClosure): REFANY =
    (* LL = 0, interface is passive *)
    (* Forked by ApplyOp; not used anywhere else *)
  VAR
    op := cl.op;
    lect := cl.lect;
    start := Time.Now();
  PROCEDURE JumpToLinks(list: ListVBT.T; links: Links.LinkList) =
      (* Jump to the link selected in "list" *)
      (* LL = SELF *)
    <*FATAL Thread.Alerted*>
    VAR
      selected: INTEGER;
    BEGIN
      IF list.getFirstSelected(selected) THEN
        JumpToLink(lect, links[selected], cl.time, cl.event);
        list.selectNone();
      END;
    END JumpToLinks;
  BEGIN
   LOCK lect DO
      CASE op OF
      (* Initialization *)
        | Op.Init => lect.initInstance(lect.initLink);
      (* External *)
        | Op.Request =>
            VAR r: REF ARRAY OF TEXT; e: TEXT;
            BEGIN
              LOCK VBT.mu DO
                r := lect.clientRequest;
                e := lect.clientError;
                lect.clientRequest := NIL;
                lect.clientError := NIL;
              END;
              lect.request(r, e, cl.time);
            END;
        | Op.Reshape =>
            IF lect.reshapeWanted THEN
              lect.reshapeWanted := FALSE;
              lect.gotoPageAuto(lect.page, lect.class, lect.part);
            END;
      (* Buttons in viewing controls window *)
        | Op.FullPage =>
            lect.gotoPageAuto(lect.page, lect.class, Partition.FullPage);
        | Op.HorThenVer =>
            lect.gotoPageAuto(lect.page, lect.class, Partition.HorThenVer);
        | Op.VerThenHor =>
            lect.gotoPageAuto(lect.page, lect.class, Partition.VerThenHor);
        | Op.HorOnly =>
            lect.gotoPageAuto(lect.page, lect.class, Partition.HorOnly);
        | Op.VerOnly =>
            lect.gotoPageAuto(lect.page, lect.class, Partition.VerOnly);
      (* Actions in links browsers *)
        | Op.BookmarkHit =>
            JumpToLinks(lect.bookmarkVBT, lect.bookmark);
        | Op.DiaryHit =>
            JumpToLinks(lect.diaryVBT, lect.diary);
        | Op.OutlineHit =>
            JumpToLinks(lect.outlineVBT, lect.outline);
        | Op.ThumbnailHit =>
            VAR hit: INTEGER;
            BEGIN
              LOCK VBT.mu DO hit := lect.thumbnailVBT.get() END;
              lect.gotoPageStart(hit - lect.dir.origin);
            END;
        | Op.ViewAreaHit, Op.AutoViewArea, Op.ManualViewArea =>
            LOCK VBT.mu DO lect.setAutoViewArea(op = Op.AutoViewArea) END;
            lect.gotoPageAuto(lect.page, lect.class, lect.part);
      (* Menu items *)
        | Op.Open =>
            WITH
              response = DoFileDlg(lect, 0, 0, 0, 0, 0, 0, "FileHelper",
                                   cl.time),
              link = Links.Link{file := GetFileName(lect)} DO
              IF response = Op.OpenNew THEN
                LOCK VBT.mu DO EVAL lect.createViewer(link, cl.time) END;
              ELSIF response = Op.Yes THEN
                lect.readDoc(link, cl.time);
              END;
            END;
        | Op.SaveDoc =>
            IF lect.path # NIL THEN
              IF lect.modified THEN
                lect.saveDoc(cl.time);
              ELSE
                LOCK VBT.mu DO lect.error("You haven't made any changes" &
                                          " to this document") END;
              END;
            END;
        | Op.Close =>
            lect.close(cl.time);
        | Op.Quit =>
            VAR this := lect;
            BEGIN
              (* Close each viewer. Closing the last document implicitly
                 kills the application.  Note that if a document has been
                 modified, the user might tell us not to close, and
                 therefore prevent us quitting.  That's OK. *)
              LOCK VBT.mu DO
                LOOP
                  IF NOT this.closed THEN
                    IF this.worker = NIL THEN
                      this.applyOp(Op.Close, cl.time);
                    ELSE
                      (* Note that this case includes ourselves *)
                      this.closeWanted := TRUE;
                      (* Ensure it's visible to the user. *)
                      EVAL ShowWindow(this.iw, this.iw, this.imageTitle);
                    END;
                  END;
                  this := this.nextV;
                  IF this = lect THEN EXIT END;
                END;
              END;
            END;
        | Op.Print  =>
            IF lect.hasPrintImages OR lect.hasOriginal THEN
              LOCK VBT.mu DO
                FormsVBT.MakeSelected(lect.popupFV, "PrintToPrinter");
              END;
              IF DoFileDlg(lect, 1, 1, 1, 1, 0, 1, "PrintCommand",
                           cl.time) = Op.Yes THEN
                lect.print(cl.time);
              END;
            ELSE
              LOCK VBT.mu DO
                lect.error("No print images and no original PostScript");
              END;
            END;
        | Op.NextSector => lect.nextSector();
        | Op.PrevSector => lect.prevSector();
        | Op.NextPage => lect.gotoPageStart(lect.page+1, FALSE);
        | Op.PrevPage => lect.gotoPageStart(lect.page-1, FALSE);
        | Op.NextPage5 => lect.gotoPageStart(lect.page+5, FALSE);
        | Op.PrevPage5 => lect.gotoPageStart(lect.page-5, FALSE);
        | Op.NextPage20 => lect.gotoPageStart(lect.page+20, FALSE);
        | Op.PrevPage20 => lect.gotoPageStart(lect.page-20, FALSE);
        | Op.FirstPage => lect.gotoPageStart(0 - lect.dir.origin);
        | Op.LastPage =>
            lect.gotoPageStart(LAST(lect.dir.pages^) - lect.dir.origin);
        | Op.ContentsPage =>
            IF lect.dir.contents >= 0 THEN
              lect.gotoPageStart(lect.dir.contents - lect.dir.origin);
            ELSE
              LOCK VBT.mu DO lect.beep() END;
            END;
        | Op.IndexPage =>
            IF lect.dir.index >= 0 THEN
              lect.gotoPageStart(lect.dir.index - lect.dir.origin);
            ELSE
              LOCK VBT.mu DO lect.beep() END;
            END;
        | Op.Home =>
           TRY
             LOCK VBT.mu DO lect.gotoSelection(lect.linkToHere()) END;
           EXCEPT Thread.Alerted =>
           END;
        | Op.ShowFind =>
           TRY
             ShowFind(lect, cl.time);
           EXCEPT Thread.Alerted =>
           END;
        | Op.Zoom =>
            LOCK VBT.mu DO
              IF lect.zoomed THEN
                TRY
                  StableVBT.SetShape(VBT.Parent(lect.iw),
                                     Rect.HorSize(lect.unzoomedDomain),
                                     Rect.VerSize(lect.unzoomedDomain));
                  WITH soRec = Trestle.ScreenOf(lect.iw, Point.Origin) DO
                    Trestle.Overlap(lect.iw, soRec.id,
                                    Point.Sub(lect.unzoomedNW,
                                              zoomWindowManagerDeltaKludge));
                  END;
                  lect.zoomed := FALSE;
                EXCEPT TrestleComm.Failure =>
                END;
              ELSE
                lect.unzoomedDomain := VBT.Domain(lect.iw);
                EVAL ZoomWindow(lect.iw, lect.unzoomedNW);
                lect.zoomed := TRUE;
              END;
            END;
        | Op.Smaller =>
            VAR class: LecternDoc.Class;
            BEGIN
              IF lect.class = LecternDoc.Class.Print THEN
                class := LecternDoc.Class.Large;
              ELSIF lect.class = LecternDoc.Class.Large THEN
                class := LecternDoc.Class.Normal;
              ELSE
                class := LecternDoc.Class.Small;
              END;
              lect.gotoPageAuto(lect.page, class, lect.part);
            END;
        | Op.Larger =>
            VAR class: LecternDoc.Class;
            BEGIN
              IF lect.class = LecternDoc.Class.Small THEN
                class := LecternDoc.Class.Normal;
              ELSIF lect.class = LecternDoc.Class.Normal THEN
                class := LecternDoc.Class.Large;
              ELSE
                LOCK VBT.mu DO
                  IF lect.allowPrintImages THEN
                    class := LecternDoc.Class.Print;
                  ELSE
                    class := LecternDoc.Class.Large;
                  END;
                END;
              END;
              lect.gotoPageAuto(lect.page, class, lect.part);
            END;
        | Op.Undo =>
            VAR u := lect.undo; r := lect.redo;
            <*FATAL Thread.Alerted*>
            BEGIN
              IF u = NIL THEN
                LOCK VBT.mu DO lect.error("Nothing more to undo") END;
              ELSE
                JumpToLink(lect, u.link, cl.time, NIL);
                IF lect.undo # u THEN
                  (* We jumped, and wrote an undo record for where we were. *)
                  lect.redo := lect.undo;
                  lect.undo := u.next;
                  lect.redo.next := r;
                  u.next := NIL; (* help the poor old GC *)
                END;
              END;
            END;
        | Op.Redo =>
            VAR u := lect.undo; r := lect.redo;
            <*FATAL Thread.Alerted*>
            BEGIN
              IF r = NIL THEN
                LOCK VBT.mu DO lect.error("Nothing more to redo") END;
              ELSE
                JumpToLink(lect, r.link, cl.time, NIL);
                IF lect.undo # u THEN
                  (* That wrote an undo record for where we were *)
                  lect.redo := r.next;
                  r.next := NIL; (* help the poor old GC *)
                END;
              END;
            END;
        | Op.NewLink, Op.BookmarkNew, Op.DiaryNew, Op.OutlineNew =>
            IF lect.path # NIL THEN
              lect.links.appendLink(LinkClassFromOp(op), lect.path, lect.dir,
                                    lect.ocr, lect.page);
            END;
        | Op.EditLink, Op.JumpLink,
          Op.BookmarkEdit, Op.DiaryEdit, Op.OutlineEdit =>
            TRY
              DoLinkDlg(lect, op, cl.time);
            EXCEPT
              | Thread.Alerted =>
              | Links.Error(t) => LOCK VBT.mu DO lect.error(t) END;
            END;
        | Op.GotoPage, Op.GotoPageConfirmed =>
            TRY
              VAR
                gotoCountText: TEXT;
                gotoCount: INTEGER;
                response: Op;
              BEGIN
                IF op = Op.GotoPageConfirmed THEN
                  response := Op.Yes;
                ELSE
                  response := lect.doModalPopup("GotoDlg",
                                                "GotoCount",
                                                FALSE,
                                                cl.time);
                END;
                LOCK VBT.mu DO
                  gotoCountText := FormsVBT.GetText(lect.popupFV,
                                                    "GotoCount");
                  TextPort.Select(v := FormsVBT.GetVBT(lect.popupFV,
                                                       "GotoCount"),
                                  time := cl.time,
                                  replaceMode := TRUE);
                END;
                IF response # Op.No THEN
                  gotoCount := Scan.Int(gotoCountText);
                  IF gotoCount <= 0 THEN RAISE Lex.Error END;
                  CASE response OF
                    | Op.Yes =>
                      lect.gotoPageStart(MAX(MIN(gotoCount,
                                                 LAST(lect.dir.pages^) -
                                                 lect.dir.origin),
                                             0-lect.dir.origin));
                    | Op.GoForward =>
                      lect.gotoPageStart(MIN(lect.page+gotoCount,
                                             LAST(lect.dir.pages^) -
                                               lect.dir.origin));
                    | Op.GoBackward =>
                      lect.gotoPageStart(MAX(lect.page-gotoCount,
                                             0-lect.dir.origin));
                  ELSE
                    <* ASSERT FALSE *>
                  END;
                END;
              END;
            EXCEPT
              | Lex.Error, FloatMode.Trap =>
                  LOCK VBT.mu DO
                    lect.error("Expected a positive (non-zero) integer");
                  END;
            END;
        | Op.FindFirst, Op.FindNext, Op.FindPrevious, Op.FindNextConfirmed =>
            VAR
              selStart, selEnd: LecternOCR.SelPos;
            BEGIN
              LOCK VBT.mu DO
                IF lect.iw # NIL THEN
                  IF lect.iw.childFocus # NIL THEN
                    lect.iw.takeFocus(cl.time);
                  END;
                END;
                MakeActive(lect.findFV, "FindStop");
              END;
              TRY
                TRY
                  IF lect.path = NIL THEN
                    RAISE Find.Error("There's no document in this viewer");
                  END;
                  IF op = Op.FindNextConfirmed THEN
                    WITH value = VBT.Read(lect.iw, VBT.Source, cl.time) DO
                      LOCK VBT.mu DO
                        FormsVBT.PutText(lect.findFV, "FindTxt", value.toRef());
                      END;
                    END;
                    ShowFind(lect, cl.time);
                  END;
                  lect.find.search(op = Op.FindFirst OR op = Op.FindNext OR
                                   op = Op.FindNextConfirmed,
                                   op = Op.FindFirst,
                                   selStart, selEnd);
                  LOCK VBT.mu DO
                    IF selStart = LecternOCR.NoSelPos THEN
                      lect.beep();
                    ELSE
                      VAR undoLink := lect.linkToHere();
                      BEGIN
                        lect.ocr.setSelection(selStart, selEnd, cl.time,
                                              lect.page, lect.class);
                        lect.gotoSelection(undoLink);
                      END;
                    END;
                  END;
                EXCEPT
                | Thread.Alerted =>
                    (* continue *)
                | VBT.Error =>
                    LOCK VBT.mu DO lect.beep() END;
                | Find.Error(errmsg) =>
                    LOCK VBT.mu DO
                      lect.error(errmsg);
                      HideZ(lect, lect.findFV);
                    END;
                END;
              FINALLY
                MakeDormant(lect.findFV, "FindStop");
              END;
            END;
      ELSE
      END;
      LOCK VBT.mu DO
        lect.worker := NIL;
        lect.passive(FALSE);
        IF lect.clientRequest # NIL OR lect.clientError # NIL THEN
          TRY (* retry the miscCode, which will be given an event
                 time and will call ApplyOp, unless we're again too busy. *)
            VBT.Forge(lect.iw, lect.clientMiscCode);
          EXCEPT VBT.Error =>
            (* Uninstalled: don't be silly, this can't happen *)
          END;
        ELSIF lect.closeWanted THEN
          lect.applyOp(Op.Close, cl.time);
        ELSIF lect.reshapeWanted THEN
          lect.applyOp(Op.Reshape, cl.time);
        END;
      END;
      (* Note: a "stop" operation won't find us now *)
      EVAL Thread.TestAlert();
      <*FATAL Wr.Failure*>
      <*FATAL Thread.Alerted*>
      BEGIN
        IF FALSE THEN
          Wr.PutText(Stdio.stderr, "Elapsed time for operation: " &
            Fmt.LongReal(Time.Now()-start, Fmt.Style.Fix, 3) & "\n");
          Wr.Flush(Stdio.stderr);
        END;
      END;
    END;
    RETURN NIL
  END ForkedOp;

PROCEDURE ApplyOp(lect: Lectern; op: Op; time: VBT.TimeStamp;
                  event: AnyEvent.T := NIL) =
    (* LL = VBT.mu *)
    (* All operation invocation gestures call this procedure, which either
       performs the entire operation, or makes the UI passive and forks a
       thread to perform the operation. *)
  VAR
    forkCl: ForkedOpClosure;
    prevMenuState: INTEGER := 1;
  BEGIN
    IF op IN ResponseOps THEN
      lect.passive(TRUE);
      lect.response := op;
      lect.responseTime := time;
      lect.responseEvent := event;
      Thread.Signal(lect.responseCV);
    ELSIF op IN AsyncOps THEN
      CASE op OF
      | Op.Stop =>
          IF lect.worker # NIL THEN Thread.Alert(lect.worker) END;
      | Op.PrintToPrinter =>
          FormsVBT.PutInteger(lect.popupFV, "FileDest", 1);
          FormsVBT.PutInteger(lect.popupFV, "FileEPSF", 0);
          FormsVBT.PutInteger(lect.popupFV, "FileConfirmLabel", 1);
          SetPopupFocus(lect, "PrintCommand", TRUE, time);
      | Op.PrintToFile =>
          FormsVBT.PutInteger(lect.popupFV, "FileDest", 0);
          FormsVBT.PutInteger(lect.popupFV, "FileEPSF", 1);
          FormsVBT.PutInteger(lect.popupFV, "FileConfirmLabel", 2);
          SetPopupFocus(lect, "FileHelper", TRUE, time);
      | Op.FileUseOriginal =>
          FormsVBT.PutInteger(lect.popupFV, "FileImages", 0);
      | Op.FileUseImages =>
          FormsVBT.PutInteger(lect.popupFV, "FileImages", 1);
      ELSE
      END;
    ELSIF lect.worker # NIL THEN
      IF NOT lect.isPassive THEN lect.beep() END;
    ELSE
      IF op # Op.Reshape THEN
        (* Take down the non-modal dialogs that are no longer appropriate *)
        IF lect.showingError THEN
          DoPopDown(lect, time); (* includes "About" dialog *)
        END;
        IF NOT (op IN FindDlgOps) AND lect.findFV # NIL THEN
          HideZ(lect, lect.findFV);
          IF lect.iw # NIL THEN
            IF lect.iw.childFocus # NIL THEN lect.iw.takeFocus(time) END;
          END;
        END;
        IF lect.menuFV # NIL THEN
          IF lect.hideMenu THEN HideZ(lect, lect.menuFV) END;
          prevMenuState := FormsVBT.GetInteger(lect.menuFV, "MenuTSplit");
          (* Also used by Op.OpenCloseMenu *)
          IF prevMenuState = 1 THEN
            FormsVBT.PutInteger(lect.menuFV, "MenuBtnTSplit", 0);
            FormsVBT.PutInteger(lect.menuFV, "MenuTSplit", 0);
          END;
        END;
      END;
      IF op IN ImmediateOps THEN
        CASE op OF
        | Op.Null =>
            (* Do nothing; but we did close the non-modal dialogs. *)
        | Op.About =>
            DoPopup(lect, "AboutDlg");
            lect.showingError := TRUE; (* so that it will go away promptly *)
        | Op.Detach =>
            IF lect.server = NIL THEN
              lect.error("This viewer is already detached");
            ELSE
              WITH child = lect.createViewer(Links.NoLink, time) DO
                (* Pass the server on to it.  In fact, there is no
                   pending request because we have VBT.mu and worker=NIL,
                   but this code doesn't assume that.  We must do the
                   entire operation under VBT.mu, though, so that the server
                   thread passes an incoming request to the correct instance,
                   and so that the child doesn't proceed with its
                   initialization until we've passed the server on to it. *)
                child.server := lect.server;
                child.clientRequest := lect.clientRequest;
                child.clientError := lect.clientError;
                child.server.lect := child;
                lect.server := NIL;
                lect.clientRequest := NIL;
                lect.clientError := NIL;
                lect.imageTitle := lect.imageTitleBase;
                Retitle(lect.iw, lect.imageTitle);
              END;
            END;
        | Op.ShowHelp =>
            lect.error("The \"help\" window hasn't been implemented yet." &
                       " Type \"m\" to show the menus.");
        | Op.ShowHideMenu =>
            lect.hideMenu := NOT lect.hideMenu;
            IF lect.hideMenu THEN
              HideZ(lect, lect.menuFV);
            ELSE
              ShowZ(lect, lect.menuFV);
            END;
        | Op.OpenCloseMenu =>
            (* We already closed the menu if it was open *)
            IF prevMenuState = 0 THEN
              FormsVBT.PutInteger(lect.menuFV, "MenuBtnTSplit", 1);
              FormsVBT.PutInteger(lect.menuFV, "MenuTSplit", 1);
              ShowZ(lect, lect.menuFV); (* show it and bring it to the top *)
            END;
        | Op.ShowHideToolbar =>
            IF ZSplit.IsMapped(lect.toolbarFV) THEN
              HideZ(lect, lect.toolbarFV);
            ELSE
              ShowZVisible(lect, lect.toolbarFV);
            END;
        | Op.ShowHideViewing =>
            IF ZSplit.IsMapped(lect.viewingFV) THEN
              HideZ(lect, lect.viewingFV);
            ELSE
              ShowZVisible(lect, lect.viewingFV);
            END;
        | Op.ShowImage =>
            IF NOT ShowWindow(lect.iw, lect.thumbnailW, lect.imageTitle) THEN
              lect.error("Can't install image window");
            END;
        | Op.ShowHideLinks =>
            <*FATAL MultiSplit.NotAChild*>
            BEGIN
              IF VBT.Parent(lect.tileFV) = NIL THEN
                SetTileColumnShape(lect, FlexVBT.FixedRange);
                MultiSplit.Insert(lect.mainSplit,
                                    MultiSplit.Pred(lect.mainSplit, NIL),
                                    lect.tileFV);
              ELSE
                MultiSplit.Delete(lect.mainSplit, lect.tileFV);
              END;
            END;
        | Op.BookmarkZoom, Op.DiaryZoom, Op.OutlineZoom, Op.ThumbnailZoom =>
            AdjustLinkTiles(lect, op);
        ELSE
        END;
      ELSE
        lect.passive(TRUE);
        lect.response := Op.Null;
        forkCl := NEW(ForkedOpClosure, lect := lect, op := op,
                      time := time, event := event);
        lect.worker := Thread.Fork(forkCl);
      END;
    END;
  END ApplyOp;

PROCEDURE PauseAndLog() =
    (* Idle loop for initial thread. It's in a separate
       procedure to make things easier for the ETP tracing machinery.
       Used by the ETP machinery to flush the log every 8 hours.
       Consult Bill Weihl for details. *)
  BEGIN
    Thread.Pause(8.0D0*3600.0D0) (* pauses for 8 hours *)
  END PauseAndLog;


(* *)
(* Initialization and main program *)
(* *)

PROCEDURE Init() =
    (* LL = 0, single-threaded *)
  <*FATAL Thread.Alerted*>
  VAR param, path: TEXT := NIL; paramNum := 1;
  BEGIN
    Thread.IncDefaultStackSize(6000); (* 4000 not enough for NeXT *)
    WHILE paramNum < Params.Count DO
      param := Params.Get(paramNum);
      IF Text.Empty(param) THEN
      ELSIF Text.Equal(param, "-geometry") OR
            Text.Equal(param, "-display") THEN
        paramNum := paramNum+1;(*next param is the argument of the option*)
      ELSIF path = NIL THEN
        path := param
      ELSE
        Wr.PutText(Stdio.stderr,
             "Usage:\n    Lectern [-geometry WxH+X+Y] [-display name] file\n");
        Process.Exit(1)
      END;
      paramNum := paramNum+1;
    END;
    LOCK VBT.mu DO
      WITH child = NEW(Lectern) DO
        child.initLink := Links.Link{file := path};
        child.class := LecternDoc.Class.Normal;
        child.nextV := child;
        child.prevV := child;
        child.server := NEW(ServerClosure, lect := child);
        child.links := NIL;
        EVAL Thread.Fork(child.server);
        child.applyOp(Op.Init, 0);
      END;
    END;
    WHILE TRUE DO PauseAndLog() END;
  END Init;

BEGIN
  Init();
END Lectern.
