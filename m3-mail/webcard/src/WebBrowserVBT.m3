(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 21:59:43 PDT 1996 by mhb       *)

MODULE WebBrowserVBT;

IMPORT AnyEvent, Env, Fmt, FormsVBT, RefSeq, Rsrc, TextList,
       PostcardBundle, VBT, Web, WebVBT;

REVEAL
  T = Public BRANDED OBJECT
        w: MyWebVBT;             (* the one currently displayed *)
        (* protected by VBT.mu *)
        pages: RefSeq.T;
        currPage: INTEGER;

      OVERRIDES
        init    := Init;
        visit   := Visit;
        surf    := Surf;
        hotlink := HotLink;
      END;

TYPE
  Activity = {Ready, Loading, Reloading, ImageFetching, Aborted};

  MyWebVBT = WebVBT.T OBJECT
               activity: Activity;
               imageCt : CARDINAL;
               v       : T;
               toFetch : TEXT;
             OVERRIDES
               ready   := Ready;
               hotlink := Link;
               ismap   := IsMap;
               isindex := IsIndex;
             END;

(* called with LL=VBT.mu *)
PROCEDURE Link (w: MyWebVBT; link: TEXT; READONLY cd: VBT.MouseRec) =
  VAR
    v   := w.v;
    url := Web.AbsoluteURL(link, w.url);
  BEGIN
   v.hotlink(url, cd);
   (* LoadURL(v, url); *)
  END Link;

(* called with LL=VBT.mu *)
PROCEDURE IsMap (w: MyWebVBT; absURL: TEXT; READONLY cd: VBT.MouseRec) =
  VAR v := w.v;
  BEGIN
    LoadURL(v, absURL);
  END IsMap;

(* called with LL=VBT.mu *)
PROCEDURE IsIndex (w: MyWebVBT; absURL: TEXT) =
  VAR v := w.v;
  BEGIN
    LoadURL(v, absURL);
  END IsIndex;

(* called with LL=VBT.mu *)
PROCEDURE Ready (w: MyWebVBT; ct: CARDINAL) =
  VAR v := w.v;
  BEGIN
    IF ct = 0 THEN
      w.activity := Activity.Ready
    ELSE
      w.activity := Activity.ImageFetching;
      w.imageCt := ct;
    END;
    IF v.w = w THEN
      (* we still displaying page w *)
      FormsVBT.PutText(v, "url", w.url);
      UpdatePageBanner(v);
    END
  END Ready;

PROCEDURE URLProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR url := FormsVBT.GetText(fv, "url");
  BEGIN
    LoadURL(fv, url);
  END URLProc;

PROCEDURE HomeProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  BEGIN
    LoadURL(fv, Env.Get("WWW_HOME"))
  END HomeProc;
   
PROCEDURE SurfProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR v: T := fv;
  BEGIN
    v.surf(v.w.url, v.w.getLinks())
  END SurfProc;
   
PROCEDURE BackProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR v: T := fv;
  BEGIN
    DEC(v.currPage);
    ShowPage(v, v.pages.get(v.currPage));
    UpdatePageBanner(v);
  END BackProc;
   
PROCEDURE ForwardProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR v:T := fv;
  BEGIN
    INC(v.currPage);
    ShowPage (v, v.pages.get(v.currPage));
    UpdatePageBanner (v);
  END ForwardProc;
   
PROCEDURE ReloadProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  BEGIN
    ReloadURL(fv)
  END ReloadProc;
   
PROCEDURE StopProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR w := NARROW(FormsVBT.GetGeneric(fv, "contents"), MyWebVBT);
  BEGIN
    w.activity := Activity.Aborted;
    w.stop();
    UpdatePageBanner (fv);
  END StopProc;


PROCEDURE SelectedStyle (v:T): WebVBT.Style =
  BEGIN
    IF FormsVBT.IsSelected(v, "displayNoImages") THEN
      RETURN WebVBT.Style.NoImages
    ELSIF FormsVBT.IsSelected(v, "displayImages") THEN
      RETURN WebVBT.Style.Normal
    ELSE
      RETURN WebVBT.Style.Background
    END;
  END SelectedStyle;
   
PROCEDURE ZipperStyle (v: T): BOOLEAN =
  BEGIN
    RETURN FormsVBT.GetBoolean(v, "useZippers");
  END ZipperStyle;

PROCEDURE ReloadURL (v: T) =
  VAR w := NARROW(FormsVBT.GetGeneric(v, "contents"), MyWebVBT);
  BEGIN
    w.activity := Activity.Reloading;
    IF w.url # NIL THEN w.toFetch := w.url END;
    w.fetch(w.toFetch, style := SelectedStyle(v), zippers := ZipperStyle(v), reload := TRUE);
    UpdatePageBanner(v)
  END ReloadURL;
   
PROCEDURE LoadURL (v: T; url: TEXT) =
  VAR w: MyWebVBT;
  BEGIN
    w := NEW(MyWebVBT, v:=v, url:=url).init();
    INC(v.currPage);
    FOR i := v.currPage TO v.pages.size() - 1 DO EVAL v.pages.remhi() END;
    v.pages.addhi(w);
    w.activity := Activity.Loading;
    w.toFetch := url;
    w.fetch(w.toFetch, style := SelectedStyle(v), zippers := ZipperStyle(v));
    ShowPage(v, w);
    UpdatePageBanner(v);
  END LoadURL;

PROCEDURE ShowPage (v: T; w: MyWebVBT) =
  VAR url: TEXT;
  BEGIN
    v.w := w;
    IF w.url = NIL THEN url := w.toFetch ELSE url := w.url END;
    FormsVBT.PutText(v, "url", url);
    FormsVBT.PutGeneric(v, "contents", w);
    FormsVBT.PutText(v, "pageCounts", Fmt.Int(v.currPage+1) & "/" & Fmt.Int(v.pages.size()));
    IF v.currPage = 0 THEN FormsVBT.MakeDormant (v, "back") 
    ELSE FormsVBT.MakeActive (v, "back") END;
    IF v.currPage = v.pages.size()-1 THEN FormsVBT.MakeDormant (v, "forward") 
    ELSE FormsVBT.MakeActive (v, "forward") END;
  END ShowPage;

PROCEDURE UpdatePageBanner (v:T) =
  VAR color, bgColor, title: TEXT; w:=v.w;
  PROCEDURE GetTitle(): TEXT = 
    BEGIN
      TYPECASE w.page OF
      | WebVBT.HTMLPage(page) => RETURN page.html.title
      ELSE RETURN "<Untitled>"
      END
    END GetTitle;
  BEGIN
    CASE w.activity OF
    | Activity.Loading =>
        bgColor := "VeryLightRed";
        color := "Black";
        title := "Fetching " & w.toFetch & " ...";
        FormsVBT.MakeActive(v, "stop");
        FormsVBT.MakeDormant(v, "reload");
        FormsVBT.MakeDormant(v, "url");
    | Activity.ImageFetching =>
        bgColor := "VeryLightBlue";
        color := "Black";
        title := "Fetching images; " & Fmt.Int(w.imageCt) & " remaining...";
        FormsVBT.MakeActive(v, "stop");
        FormsVBT.MakeDormant(v, "reload");
        FormsVBT.MakeDormant(v, "url");
    | Activity.Reloading =>
        bgColor := "VeryLightRed";
        color := "Black";
        title := "Reloading " & w.toFetch & " ...";
        FormsVBT.MakeActive(v, "stop");
        FormsVBT.MakeDormant(v, "reload");
        FormsVBT.MakeDormant(v, "url");
    | Activity.Aborted =>
        IF w.page = NIL THEN
          bgColor := "DarkRed";
          color := "White";
          title := "Fetching of " & w.toFetch & " interrupted by user";
        ELSE
          bgColor := "DarkRed";
          color := "White";
          title := "Reloading \"" & GetTitle() & "\" interrupted by user";
        END;
        FormsVBT.MakeDormant(v, "stop");
        FormsVBT.MakeActive(v, "reload");
        FormsVBT.MakeActive(v, "url");
    | Activity.Ready =>
        bgColor := "White";
        color := "Black";
        title := GetTitle();
        title := w.toFetch;
        FormsVBT.MakeDormant(v, "stop");
        FormsVBT.MakeActive(v, "reload");
        FormsVBT.MakeActive(v, "url");
    END;      
    FormsVBT.PutTextProperty (v, "title", "BgColor", bgColor);
    FormsVBT.PutTextProperty (v, "title", "Color", color);
    FormsVBT.PutText(v, "title", title);
  END UpdatePageBanner;


PROCEDURE OpenProc (fv: FormsVBT.T; e: TEXT; cl: REFANY; t: VBT.TimeStamp) =
  VAR v: T:=fv; event := FormsVBT.GetTheEvent(fv);
  BEGIN
    TYPECASE event OF
    | AnyEvent.Mouse (m) =>
        IF TRUE OR VBT.Modifier.MouseM IN m.mouse.modifiers THEN 
          TYPECASE VBT.Read(v, VBT.Source, t).toRef() OF
          | NULL =>
          | TEXT (txt) => LoadURL(v, txt); RETURN;
          ELSE
          END;
        END;
     ELSE
    END;
    FormsVBT.TakeFocus(v, "openurl", t, TRUE);
    FormsVBT.PopUp(v, "OpenDlg", TRUE, t); 
  END OpenProc;
     
PROCEDURE Init (v: T): T =
  VAR
  BEGIN
    EVAL FormsVBT.T.initFromRsrc(
           v, "webbrowser.fv",
           Rsrc.BuildPath("$WebBrowserPATH", PostcardBundle.Get()));
    FormsVBT.AttachProc(v, "url", URLProc);
    FormsVBT.AttachProc(v, "autosurf", SurfProc);
    FormsVBT.AttachProc(v, "back", BackProc);
    FormsVBT.AttachProc(v, "forward", ForwardProc);
    FormsVBT.AttachProc(v, "home", HomeProc);
    FormsVBT.AttachProc(v, "reload", ReloadProc);
    FormsVBT.AttachProc(v, "stop", StopProc);
    FormsVBT.AttachProc(v, "displayStyle", ReloadProc);
    FormsVBT.AttachProc(v, "useZippers", ReloadProc);

    FormsVBT.AttachProc(v, "open", OpenProc);

    v.pages := NEW(RefSeq.T).init(); (* stack of MyWebVBT's *)
    v.currPage := -1;
    (*
     LOCK VBT.mu DO
      LoadURL(v, Env.Get("WWW_HOME"))
    END;
    *)

    RETURN v
  END Init;

PROCEDURE Visit (v: T; url: TEXT) =
  BEGIN
    LoadURL(v, url)
  END Visit;

PROCEDURE Surf (v: T; base: TEXT; links: TextList.T) =
  BEGIN
  END Surf;

PROCEDURE HotLink (v: T; link: TEXT; READONLY cd: VBT.MouseRec) =
  BEGIN
  END HotLink;

BEGIN 
END WebBrowserVBT.
