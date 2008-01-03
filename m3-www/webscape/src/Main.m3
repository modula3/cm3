(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Tue Aug 27 15:53:33 PDT 1996 by mhb       *)

MODULE Main;

IMPORT AnyEvent, Env, Fmt, FormsVBT, HTML, Rd, RefSeq, Rsrc, Stdio, Thread,
       Trestle, TrestleComm, UIBundle, VBT, Web, WebVBT, Wr;

<* FATAL FormsVBT.Error *>
<* FATAL FormsVBT.Unimplemented *>
<* FATAL Thread.Alerted *>
<* FATAL TrestleComm.Failure *>
<* FATAL VBT.Error *>
<* FATAL Wr.Failure *>


TYPE
  Form = FormsVBT.T OBJECT
           w: MyWebVBT;          (* the one currently displayed *)
         END;

  Activity = {Ready, Loading, Reloading, ImageFetching, Aborted};

  MyWebVBT = WebVBT.T OBJECT
               activity: Activity;
               imageCt : CARDINAL;
               fv      : Form;
               toFetch : TEXT; 
             OVERRIDES
               ready   := Ready;
               hotlink := Link;
               ismap   := IsMap;
               isindex := IsIndex;
             END;

VAR (* protected by VBT.mu *)
  pages := NEW(RefSeq.T).init(); (* stack of MyWebVBT's *)
  currPage := -1;
 
(* called with LL=VBT.mu *)
PROCEDURE Link (w: MyWebVBT; link: TEXT; <*UNUSED*> READONLY cd: VBT.MouseRec) =
  VAR
    fv  := w.fv;
    url := Web.AbsoluteURL(link, w.url);
  BEGIN
    LoadURL(fv, url);
  END Link;

(* called with LL=VBT.mu *)
PROCEDURE IsMap (w: MyWebVBT; absURL: TEXT; <*UNUSED*> READONLY cd: VBT.MouseRec) =
  VAR fv := w.fv;
  BEGIN
    LoadURL(fv, absURL);
  END IsMap;

(* called with LL=VBT.mu *)
PROCEDURE IsIndex (w: MyWebVBT; absURL: TEXT) =
  VAR fv := w.fv;
  BEGIN
    LoadURL(fv, absURL);
  END IsIndex;

(* called with LL=VBT.mu *)
PROCEDURE Ready (w: MyWebVBT; ct: CARDINAL) =
  VAR fv := w.fv;
  BEGIN
    IF ct = 0 THEN 
       w.activity := Activity.Ready
    ELSE 
       w.activity := Activity.ImageFetching;
       w.imageCt := ct;
    END;
    IF fv.w = w THEN
      (* we still displaying page w *)
      FormsVBT.PutText(fv, "url", w.url);
      UpdatePageBanner (fv);
    END
  END Ready;

PROCEDURE QuitProc (             fv: FormsVBT.T;
                    <* UNUSED *> e : TEXT;
                    <* UNUSED *> cl: REFANY;
                    <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(fv)
  END QuitProc;
   
PROCEDURE DebugProc (             fv: FormsVBT.T;
                     <* UNUSED *> e : TEXT;
                     <* UNUSED *> cl: REFANY;
                     <* UNUSED *> t : VBT.TimeStamp) =
  CONST
    Separator = "\n\n\n***************************************************************\n\n";
  VAR w := NARROW(fv, Form).w;
  BEGIN
    Wr.PutText(Stdio.stderr, Separator);
    (*
    ** Wr.PutText(Stdio.stderr, w.page.header);
    ** Wr.PutText(Stdio.stderr, Separator);
    *)
    Wr.PutText(Stdio.stderr, w.page.contents);
    TYPECASE (w.page) OF
    | NULL =>
    | WebVBT.HTMLPage (h) =>
        Wr.PutText(Stdio.stderr, Separator);
        HTML.Dump(h.html, Stdio.stderr);
        Wr.PutText(Stdio.stderr, Separator);
    ELSE
    END;
    Wr.PutText(Stdio.stderr, Separator);
  END DebugProc;

PROCEDURE URLProc (             fv: FormsVBT.T;
                   <* UNUSED *> e : TEXT;
                   <* UNUSED *> cl: REFANY;
                   <* UNUSED *> t : VBT.TimeStamp) =
  VAR url := FormsVBT.GetText(fv, "url");
  BEGIN
    LoadURL(fv, url);
  END URLProc;

PROCEDURE HomeURL (): TEXT =
  VAR url := Env.Get ("WWW_HOME");
  BEGIN
    IF url = NIL THEN
      url := "http://www.research.digital.com/SRC/webbrowsing/";
    END;
    RETURN url;
  END HomeURL;

PROCEDURE HomeProc (            fv: FormsVBT.T;                    
                   <* UNUSED *> e : TEXT;
                   <* UNUSED *> cl: REFANY;
                   <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    LoadURL(fv, HomeURL ());
  END HomeProc;
   
PROCEDURE BackProc (            fv: FormsVBT.T;                    
                   <* UNUSED *> e : TEXT;
                   <* UNUSED *> cl: REFANY;
                   <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    DEC(currPage);
    ShowPage (fv, pages.get(currPage));
    UpdatePageBanner (fv);
  END BackProc;
   
PROCEDURE ForwardProc (         fv: FormsVBT.T;                    
                   <* UNUSED *> e : TEXT;
                   <* UNUSED *> cl: REFANY;
                   <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    INC(currPage);
    ShowPage (fv, pages.get(currPage));
    UpdatePageBanner (fv);
  END ForwardProc;
   
PROCEDURE ReloadProc (             fv: FormsVBT.T;
                      <* UNUSED *> e : TEXT;
                      <* UNUSED *> cl: REFANY;
                      <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    ReloadURL(fv)
  END ReloadProc;
   
PROCEDURE StopProc (             fv: FormsVBT.T;
                    <* UNUSED *> e : TEXT;
                    <* UNUSED *> cl: REFANY;
                    <* UNUSED *> t : VBT.TimeStamp) =
  VAR w := NARROW(FormsVBT.GetGeneric(fv, "contents"), MyWebVBT);
  BEGIN
    w.activity := Activity.Aborted;
    w.stop();
    UpdatePageBanner(fv);
  END StopProc;


PROCEDURE SelectedStyle (fv: Form): WebVBT.Style =
  BEGIN
    IF FormsVBT.IsSelected(fv, "displayUgly") THEN
      RETURN WebVBT.Style.Ugly
    ELSIF FormsVBT.IsSelected(fv, "displayNoImages") THEN
      RETURN WebVBT.Style.NoImages
    ELSIF FormsVBT.IsSelected(fv, "displayImages") THEN
      RETURN WebVBT.Style.Normal
    ELSE
      RETURN WebVBT.Style.Background
    END;
  END SelectedStyle;
   
PROCEDURE ZipperStyle (fv: Form): BOOLEAN =
  BEGIN
    RETURN FormsVBT.GetBoolean(fv, "useZippers");
  END ZipperStyle;

PROCEDURE ReloadURL (fv: Form) =
  VAR w := NARROW(FormsVBT.GetGeneric(fv, "contents"), MyWebVBT);
  BEGIN
    w.activity := Activity.Reloading;
    IF w.url # NIL THEN w.toFetch := w.url END;
    w.fetch(w.toFetch, style := SelectedStyle(fv), zippers := ZipperStyle(fv), reload := TRUE);
    UpdatePageBanner(fv)
  END ReloadURL;
   
PROCEDURE LoadURL (fv: Form; url: TEXT) =
  VAR w: MyWebVBT;
  BEGIN
    w := NEW(MyWebVBT, fv:=fv, url:=url).init();
    INC(currPage);
    FOR i := currPage TO pages.size() - 1 DO EVAL pages.remhi() END;
    pages.addhi(w);
    w.activity := Activity.Loading;
    w.toFetch := url;
    w.fetch(w.toFetch, style := SelectedStyle(fv), zippers := ZipperStyle(fv));
    ShowPage(fv, w);
    UpdatePageBanner(fv);
  END LoadURL;

PROCEDURE ShowPage (fv: Form; w: MyWebVBT) =
  VAR url: TEXT;
  BEGIN
    fv.w := w;
    IF w.url = NIL THEN url := w.toFetch ELSE url := w.url END;
    FormsVBT.PutText(fv, "url", url);
    FormsVBT.PutGeneric(fv, "contents", w);
    FormsVBT.PutText(fv, "pageCounts", Fmt.Int(currPage+1) & "/" & Fmt.Int(pages.size()));
    IF currPage = 0 THEN FormsVBT.MakeDormant (fv, "back") 
    ELSE FormsVBT.MakeActive (fv, "back") END;
    IF currPage = pages.size()-1 THEN FormsVBT.MakeDormant (fv, "forward") 
    ELSE FormsVBT.MakeActive (fv, "forward") END;
  END ShowPage;

PROCEDURE UpdatePageBanner (fv: Form) =
  VAR color, bgColor, title: TEXT; w:=fv.w;
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
        FormsVBT.MakeActive(fv, "stop");
        FormsVBT.MakeDormant(fv, "reload");
        FormsVBT.MakeDormant(fv, "url");
    | Activity.ImageFetching =>
        bgColor := "VeryLightBlue";
        color := "Black";
        title := "Fetching images; " & Fmt.Int(w.imageCt) & " remaining...";
        FormsVBT.MakeActive(fv, "stop");
        FormsVBT.MakeDormant(fv, "reload");
        FormsVBT.MakeDormant(fv, "url");
    | Activity.Reloading =>
        bgColor := "VeryLightRed";
        color := "Black";
        title := "Reloading " & w.toFetch & " ...";
        FormsVBT.MakeActive(fv, "stop");
        FormsVBT.MakeDormant(fv, "reload");
        FormsVBT.MakeDormant(fv, "url");
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
        FormsVBT.MakeDormant(fv, "stop");
        FormsVBT.MakeActive(fv, "reload");
        FormsVBT.MakeActive(fv, "url");
    | Activity.Ready =>
        bgColor := "White";
        color := "Black";
        title := GetTitle();
        FormsVBT.MakeDormant(fv, "stop");
        FormsVBT.MakeActive(fv, "reload");
        FormsVBT.MakeActive(fv, "url");
    END;      
    FormsVBT.PutTextProperty (fv, "title", "BgColor", bgColor);
    FormsVBT.PutTextProperty (fv, "title", "Color", color);
    FormsVBT.PutText(fv, "title", title);
  END UpdatePageBanner;


PROCEDURE OpenProc (             fv: FormsVBT.T;
                    <* UNUSED *> e : TEXT;
                    <* UNUSED *> cl: REFANY;
                                 t : VBT.TimeStamp) =
  VAR event := FormsVBT.GetTheEvent(fv);
  BEGIN
    TYPECASE event OF
    | AnyEvent.Mouse (m) =>
        IF VBT.Modifier.MouseM IN m.mouse.modifiers THEN
          TYPECASE VBT.Read(fv, VBT.Source, t).toRef() OF
          | NULL =>
          | TEXT (txt) => LoadURL(fv, txt); RETURN;
          ELSE
          END;
        END;
    ELSE
    END;
    FormsVBT.TakeFocus(fv, "openurl", t, TRUE);
    FormsVBT.PopUp(fv, "OpenDlg", TRUE, t);
  END OpenProc;

PROCEDURE OpenURLProc (             fv: FormsVBT.T;
                       <* UNUSED *> e : TEXT;
                       <* UNUSED *> cl: REFANY;
                       <* UNUSED *> t : VBT.TimeStamp) =
  VAR url := FormsVBT.GetText(fv, "openurl");
  BEGIN
    LoadURL(fv, url)
  END OpenURLProc;

PROCEDURE OpenClearProc (             fv: FormsVBT.T;
                         <* UNUSED *> e : TEXT;
                         <* UNUSED *> cl: REFANY;
                         <* UNUSED *> t : VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "openurl", "")
  END OpenClearProc;
 
PROCEDURE OpenPasteProc (             fv: FormsVBT.T;
                         <* UNUSED *> e : TEXT;
                         <* UNUSED *> cl: REFANY;
                                      t : VBT.TimeStamp) =
  BEGIN
    TYPECASE VBT.Read(fv, VBT.Source, t).toRef() OF
    | NULL =>
    | TEXT (txt) =>
        FormsVBT.PutText(fv, "openurl", txt);
        FormsVBT.TakeFocus(fv, "openurl", t, FALSE);
    ELSE
    END;
  END OpenPasteProc;
 
     
PROCEDURE NewForm (): Form =
  <* FATAL Rd.Failure, Rsrc.NotFound *>
  VAR
    fv := NEW(Form).initFromRsrc(
            "ui.fv", Rsrc.BuildPath("$BrowserPATH", UIBundle.Get()));
  BEGIN
    FormsVBT.AttachProc(fv, "url", URLProc);
    FormsVBT.AttachProc(fv, "back", BackProc);
    FormsVBT.AttachProc(fv, "forward", ForwardProc);
    FormsVBT.AttachProc(fv, "home", HomeProc);
    FormsVBT.AttachProc(fv, "reload", ReloadProc);
    FormsVBT.AttachProc(fv, "stop", StopProc);
    FormsVBT.AttachProc(fv, "quit", QuitProc);
    FormsVBT.AttachProc(fv, "displayStyle", ReloadProc);
    FormsVBT.AttachProc(fv, "useZippers", ReloadProc);
    FormsVBT.AttachProc(fv, "debug", DebugProc);

    FormsVBT.AttachProc(fv, "open", OpenProc);
    FormsVBT.AttachProc(fv, "openurl", OpenURLProc);
    FormsVBT.AttachProc(fv, "openopen", OpenURLProc);
    FormsVBT.AttachProc(fv, "openclear", OpenClearProc);
    FormsVBT.AttachProc(fv, "openpaste", OpenPasteProc);

    LOCK VBT.mu DO 
      LoadURL(fv, HomeURL ()) 
    END;
    RETURN fv
  END NewForm;

BEGIN 
  WITH z = NewForm() DO
    Trestle.Install(z);
    Trestle.AwaitDelete(z);
  END    
END Main.

