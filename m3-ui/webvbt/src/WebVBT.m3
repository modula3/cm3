(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 16 15:59:05 PDT 1996 by mhb                      *)
(*      modified on Wed Jan 17 14:34:14 PST 1996 by najork                   *)
<* PRAGMA LL *>

MODULE WebVBT;

IMPORT CIText, Filter, Fmt, Font, HTML, HTMLVBT, HTMLVBTText, HTMLVBTG,
       Images, MultiClass, MultiSplit, Pixmap, PixmapVBT, Point,
       RefList, SimpleWeb, Split, TextExtras, TextList, TextEditVBT,
       TextPort, TextureVBT, TextVBT, Thread, URLCache, VBT, Web, Image, 
       Rd, TextRd, PaintOp;

REVEAL Private = Filter.T BRANDED OBJECT END;

REVEAL
  T = Public BRANDED OBJECT
        <* LL=VBT.mu *>
        t: Thread.T := NIL;
      OVERRIDES
        init     := Init;
        fetch    := Fetch;
        fromText := FromText;
        stop     := Stop;
        getLinks := GetLinks;
        search   := Search;
        ready    := Ready;
        hotlink  := Hotlink;
        isindex  := Isindex;
        ismap    := Ismap;
        form     := Form;
      END;

PROCEDURE Init (v: T): T =
  BEGIN
    RETURN Filter.T.init(v, TextureVBT.New(txt:=Pixmap.Gray))
  END Init;


PROCEDURE FromText (v             : T;
                    contents      : TEXT;
                    contentType   : Web.MIMEType := Web.MIMEType.Text;
                    contentSubType: TEXT         := "html";
                    url           : TEXT         := "text:"; 
                    style         : Style        := Style.Normal;
                    zippers       : BOOLEAN      := FALSE;
                    reload        : BOOLEAN      := FALSE;
                    server        : Web.T        := NIL;
                    scrollBar     : BOOLEAN      := TRUE) =

  VAR webpage := NEW(Web.Page);
  BEGIN
    v.stop();
    webpage.header.contentType := contentType;
    webpage.header.contentSubType := contentSubType;
    webpage.header.location := url;
    webpage.contents := contents;
    v.t :=
      Thread.Fork(NEW(FromTextClosure, v := v, webpage := webpage,
                      url := url, style := style, zippers := zippers,
                      reload := reload, server := server, 
                      scrollBar := scrollBar))
  END FromText;

TYPE
  Closure = Thread.Closure OBJECT
                      v        : T;
                      style    : Style;
                      zippers  : BOOLEAN;
                      reload   : BOOLEAN;
                      server   : Web.T;
                      scrollBar: BOOLEAN;
                    END;

TYPE
  FromTextClosure = Closure OBJECT
                      webpage : Web.Page;
                      url     : TEXT;
                    OVERRIDES
                      apply := FromTextWrapper;
                    END;

PROCEDURE FromTextWrapper (cl: FromTextClosure): REFANY =
  BEGIN
    Display(cl.webpage, cl.v, cl.url, cl.style, cl.zippers, cl.reload,
            cl.server, cl.scrollBar);
    RETURN NIL
  END FromTextWrapper;


PROCEDURE Fetch (v        : T;
                 url      : TEXT;
                 style    : Style   := Style.Normal;
                 zippers  : BOOLEAN := FALSE;
                 reload   : BOOLEAN := FALSE;
                 server   : Web.T   := NIL;
                 scrollBar: BOOLEAN := TRUE) =
  BEGIN
    v.stop();
    v.t := Thread.Fork(
             NEW(FetchClosure, v := v, url := url, style := style,
                 zippers := zippers, reload := reload, server := server, 
                 scrollBar := scrollBar))
  END Fetch;

TYPE
  FetchClosure = Closure OBJECT
                   url    : TEXT;
                 OVERRIDES
                   apply := FetchWrapper;
                 END;

PROCEDURE FetchWrapper (cl: FetchClosure): REFANY =
  VAR webpage: Web.Page; base: TEXT;
  BEGIN
    TRY
      webpage := 
          SimpleWeb.Fetch(cl.url, reload := cl.reload, server := cl.server);
      base := webpage.header.location;  
                   (* SimpleWeb.Fetch always fills in header.location *)
      Display(webpage, cl.v, base, cl.style, cl.zippers, cl.reload, cl.server, 
              cl.scrollBar);
    EXCEPT
      Thread.Alerted =>
    END;
    RETURN NIL
  END FetchWrapper;


CONST
    FontName = "-*-fixed-medium-r-semicondensed-*-*-120-*-*-*-*-iso8859-1";

PROCEDURE Display (webpage  : Web.Page;
                   v        : T;
                   base     : TEXT;
                   style    : Style;
                   zippers  : BOOLEAN;
                   reload   : BOOLEAN;
                   server   : Web.T;
                   scrollBar: BOOLEAN) =

  PROCEDURE NewTextPage (t: TEXT) RAISES {Thread.Alerted} =
    VAR page := NEW(TextPage);
    BEGIN
      page.vbt := NEW(TextEditVBT.T).init();
      WITH tp = page.vbt.tp DO
        TextPort.SetText(tp, t);
        tp.setReadOnly(TRUE);
        tp.setFont(Font.FromName(ARRAY OF TEXT{FontName}));
      END;
      NewPage(page, page.vbt)
    END NewTextPage;

  PROCEDURE NewHTMLPage (h: HTML.T) RAISES {Thread.Alerted} =
    VAR
      page              := NEW(HTMLPage);
      toLoad: RefList.T;

    BEGIN
      IF h.base = NIL THEN h.base := base END;
      page.html := h;

      URLCache.PutHTML (base, h);

      CASE style OF

      | Style.Ugly =>
          page.vbt := NEW(TextHTMLVBT, parent := v).init(page.html);
          NewPage(page, page.vbt);

      | Style.NoImages =>
          page.vbt := NEW(GraphicsHTMLVBT, parent := v).init(
                        page.html, TRUE, zippers, toLoad, scrollBar);
          NewPage(page, page.vbt);

      | Style.Normal =>
          page.vbt := NEW(GraphicsHTMLVBT, parent := v).init(
                        page.html, FALSE, zippers, toLoad, scrollBar);
          LoadResources(v, reload, server, toLoad, FALSE);
          NewPage(page, page.vbt);

      | Style.Background =>
          page.vbt := NEW(GraphicsHTMLVBT, parent := v).init(
                        page.html, FALSE, zippers, toLoad, scrollBar);
          NewPage(page, page.vbt, RefList.Length(toLoad));
          LoadResources(v, reload, server, toLoad, TRUE);

      END;
    END NewHTMLPage;

  PROCEDURE NewImagePage (pm: Pixmap.T) RAISES {Thread.Alerted} =
    VAR 
      page := NEW(ImagePage);
      (* op := PaintOp.BgFg; *)
      op := PaintOp.Copy;
    BEGIN
      page.vbt := NEW(PixmapVBT.T).init(pm, op := op);
      NewPage(page, page.vbt);
    END NewImagePage;

  PROCEDURE NewPage (page: Page; vbt: VBT.T; imageCt := 0)
    RAISES {Thread.Alerted} =
    BEGIN
      LOCK VBT.mu DO
        IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
        IF v.t # Thread.Self() THEN RETURN END;
        EVAL Filter.Replace(v, vbt);
        page.header := webpage.header;
        page.contents := webpage.contents;
        v.url := base;
        v.page := page;
        v.ready(imageCt);
      END
    END NewPage;

  BEGIN
    TRY
      WITH hdr   = webpage.header,
           stuff = webpage.contents DO
        IF hdr.contentType = Web.MIMEType.Text THEN
          IF CIText.Equal(hdr.contentSubType, "html") THEN
            NewHTMLPage(HTML.FromRd(TextRd.New(stuff)))
          ELSE
            NewTextPage(stuff);
          END;
        ELSIF hdr.contentType = Web.MIMEType.Image THEN
          TRY
            IF CIText.Equal(hdr.contentSubType, "jpeg") THEN
              NewImagePage(Images.FromJPEG(stuff));
            ELSIF CIText.Equal(hdr.contentSubType, "gif") THEN
              NewImagePage(Images.FromGIF(stuff))
            ELSIF CIText.Equal(hdr.contentSubType, "ppm") OR
                  CIText.Equal(hdr.contentSubType, "pnm") OR
                  CIText.Equal(hdr.contentSubType, "pbm") OR
                  CIText.Equal(hdr.contentSubType, "pgm") THEN
              WITH rd = TextRd.New(stuff) DO
                NewImagePage(Image.Unscaled(Image.FromRd(rd)));
              END;
            ELSE
              NewTextPage("cannot handle '" & hdr.contentSubType & "'");
            END
          EXCEPT
            Rd.Failure, Image.Error, Images.Error => 
                NewTextPage("cannot display image");
          END
        END
      END
    EXCEPT
      Thread.Alerted =>
    END
  END Display;


PROCEDURE LoadResources (v              : T;
                         reload         : BOOLEAN;
                         server         : Web.T;
                         list           : RefList.T;
                         callReadyMethod: BOOLEAN    )
  RAISES {Thread.Alerted} =
  VAR
    ct  : INTEGER;
    info: HTMLVBTG.Info;
    page: Web.Page;
  BEGIN
    ct := RefList.Length(list);
    WHILE list # NIL DO
      info := list.head;
      page :=
        SimpleWeb.Fetch(info.url, reload := reload, server := server);
      info.load(page);
      DEC(ct);
      IF callReadyMethod THEN InvokeReadyMethod(v, ct) END;
      list := list.tail;
    END;
    IF callReadyMethod THEN InvokeReadyMethod(v, 0) END;
  END LoadResources;

PROCEDURE InvokeReadyMethod (v: T; arg: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    LOCK VBT.mu DO
      IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
      IF v.t # Thread.Self() THEN RETURN END;
      v.ready(arg)
    END;
  END InvokeReadyMethod;

TYPE GraphicsHTMLVBT = HTMLVBTG.T OBJECT
    parent: T;
  OVERRIDES
    hotlink := HTMLVBTHotlink;
    ismap   := HTMLVBTIsmap;
    isindex := HTMLVBTIsindex;
  END;

TYPE TextHTMLVBT = HTMLVBTText.T OBJECT
    parent: T;
  OVERRIDES
    hotlink := HTMLVBTHotlink;
    ismap   := HTMLVBTIsmap;
    isindex := HTMLVBTIsindex;
  END;

PROCEDURE HTMLVBTHotlink (         ch : HTMLVBT.T;
                                   url: TEXT;
                          READONLY cd : VBT.MouseRec) =
  BEGIN
    TYPECASE ch OF
    | GraphicsHTMLVBT (v) => v.parent.hotlink(url, cd)
    ELSE 
    END
  END HTMLVBTHotlink;

PROCEDURE HTMLVBTIsmap (         ch : HTMLVBT.T;
                                 url: TEXT;
                        READONLY pt : Point.T;
                        READONLY cd : VBT.MouseRec) =
  BEGIN
    TYPECASE ch OF
    | GraphicsHTMLVBT (v) =>
        v.parent.ismap(url & "?" & Fmt.Int(pt.h) & "," & Fmt.Int(pt.v), cd)
    ELSE
    END
  END HTMLVBTIsmap;

PROCEDURE HTMLVBTIsindex (ch: HTMLVBT.T; typein: TEXT) =
  VAR p: T;
  BEGIN
    TYPECASE ch OF
    | GraphicsHTMLVBT (v) => p := v.parent;
    | TextHTMLVBT (v) => p := v.parent;
    ELSE <* ASSERT FALSE *>
    END;
    p.isindex(p.url & "?" & typein);
  END HTMLVBTIsindex;

PROCEDURE Stop (self: T) =
  BEGIN
    IF self.t # NIL THEN Thread.Alert(self.t) END
  END Stop;

PROCEDURE Hotlink (<* UNUSED *> self: T; 
                   <* UNUSED *> link: TEXT;
                   <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Hotlink;

PROCEDURE Isindex (<* UNUSED *> self: T; <* UNUSED *> typein: TEXT) =
  BEGIN
  END Isindex;

PROCEDURE Ismap (<* UNUSED *>          self  : T;
                 <* UNUSED *>          absURL: TEXT;
                 <* UNUSED *> READONLY cd    : VBT.MouseRec) =
  BEGIN
  END Ismap;

PROCEDURE Form (<* UNUSED *> self: T) =
  BEGIN
  END Form;

PROCEDURE Ready (<* UNUSED *> self: T; <* UNUSED *> remImages: CARDINAL) =
  BEGIN
  END Ready;

PROCEDURE GetLinks (self: T): TextList.T =
  BEGIN
    TYPECASE self.page OF
    | NULL =>
    | HTMLPage (h) => RETURN HTML.GetLinks(h.html)
    ELSE
    END;
    RETURN NIL
  END GetLinks;

PROCEDURE Search (self: T; pattern: TEXT): BOOLEAN =
  BEGIN
    TYPECASE self.page OF
    | NULL =>
    | TextPage (t) => RETURN SearchVBTTree(t.vbt, pattern)
    | HTMLPage (h) => RETURN SearchVBTTree(h.vbt, pattern)
    ELSE
    END;
    RETURN FALSE
  END Search;

PROCEDURE SearchVBTTree (v: VBT.T; pattern: TEXT): BOOLEAN =
  <* FATAL MultiSplit.NotAChild *>
  BEGIN
    TYPECASE v OF
    | TextVBT.T (textvbt) =>
        WITH text = TextVBT.Get(textvbt) DO
          RETURN TextSearch(text, pattern)
        END;
    | TextPort.T (textport) =>
        WITH text = TextPort.GetText(textport) DO
          RETURN TextSearch(text, pattern)
        END
    ELSE
      IF MultiClass.Resolve(v) # NIL OR ISTYPE(v, Split.T) THEN
        VAR ch := MultiSplit.Succ(v, NIL);
        BEGIN
          WHILE ch # NIL DO
            IF SearchVBTTree(ch, pattern) THEN
              RETURN TRUE
            ELSE
              ch := MultiSplit.Succ(v, ch)
            END
          END;
          RETURN FALSE
        END
      ELSE
        RETURN FALSE
      END
    END;
  END SearchVBTTree;
  
PROCEDURE TextSearch (text, pattern: TEXT): BOOLEAN =
  VAR index: CARDINAL := 0;
  BEGIN
    RETURN TextExtras.FindSub(text, pattern, index)
  END TextSearch;


BEGIN
END WebVBT.

