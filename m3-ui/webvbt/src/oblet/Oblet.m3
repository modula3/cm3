(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 12 08:22:33 PDT 1996 by mhb                      *)
(*      modified on Thu Jan 18 11:22:20 PST 1996 by najork                   *)

MODULE Oblet;

IMPORT BorderedVBT, Filter, HTML, HTMLVBTG, HTMLVBTGRep, Obliq,
       ObliqParser, ObLibAnim, ObLibM3, ObLibUI, ObLibWeb, ObValue,
       PixmapVBT, Pts, Rd, RefList, Split, Stdio, SynWr, Text, TextRd,
       Thread, TranslateVBT, VBT, VBTClass, Web;

TYPE
  ObletInfo = HTMLVBTG.ObletInfo;

  T = HTMLVBTG.T;

  State = HTMLVBTG.State;

  RigidTextVBT = HTMLVBTG.RigidTextVBT;

  RigidPixmapVBT = HTMLVBTG.RigidPixmapVBT;

REVEAL 
  ObletInfo = HTMLVBTG.PublicObletInfo BRANDED OBJECT 
    filter: Filter.T;
  OVERRIDES
    load := LoadOblet;
  END;

PROCEDURE DisplayOblet (v: T; vsplit: VBT.T; s: State; oblet: HTML.Oblet) =
  BEGIN
    HTMLVBTG.EnterHMode(v, vsplit);

    IF v.useAlt OR oblet.source = NIL THEN
      VAR alt: TEXT;
      BEGIN
        IF oblet.source = NIL THEN
          alt := "[oblet]"
        ELSE
          alt := "[oblet:<" & oblet.source & ">]"
        END;
        WITH vbt = NEW(RigidTextVBT).init(
                     txt := alt, hmargin := 0.0, halign := 0.5,
                     vmargin := 0.0, valign := 0.0, fnt := s.font,
                     bgFg := s.bgFg) DO
          Split.AddChild(v.hsplit, vbt)
        END
      END

    ELSE
      VAR
        pmVBT := NEW(RigidPixmapVBT).init(
                   pm := HTMLVBTG.EmptyImage, 
                   op := HTMLVBTG.RegularColors.bgFg,
                   bg := HTMLVBTG.RegularColors.bg);
        border := BorderedVBT.New(
                    pmVBT, size := Pts.ToMM(0.5), op := s.bgFg.fg);
        url: TEXT;
      BEGIN
        IF s.bgFg # HTMLVBTG.AnchorColors THEN 
          BorderedVBT.SetSize(border, 0.0) 
        END;
        Split.AddChild(v.hsplit, border);
        url := Web.AbsoluteURL(oblet.source, v.baseURL);
        v.toLoad :=
          RefList.Cons(
            NEW(ObletInfo, url := url, filter := border), v.toLoad)
      END
    END

  END DisplayOblet;


EXCEPTION BadOblet;

PROCEDURE LoadOblet (                      info  : ObletInfo; page: Web.Page)
  RAISES {Thread.Alerted} =
  VAR
    swr               := SynWr.New(Stdio.stderr);
    parser            := ObliqParser.New(swr);
    rd                := TextRd.New(page.contents);
    vbt   : VBT.T;
    env   : Obliq.Env;
  BEGIN
    TRY
      env := ParseRd(parser, info.url, rd);
      TRY Rd.Close(rd) EXCEPT Rd.Failure => END;
      WITH obj = Obliq.Lookup("oblet", env) DO
        TYPECASE Obliq.ObjectSelect(obj, "vbt") OF
        | ObLibUI.ValVBT (node) => vbt := TranslateVBT.New(node.vbt);
        ELSE
          RAISE BadOblet
        END;
        LOCK VBT.mu DO EVAL Filter.Replace(info.filter, vbt); END;
        EVAL Thread.Fork(NEW(ObletClosure, obj := obj, swr := swr));
        (* EVAL Thread.Join(Thread.Fork(NEW(ObletClosure, obj := obj))); *)
      END;
    EXCEPT
      ObValue.Error, ObValue.Exception, BadOblet =>
        LOCK VBT.mu DO
          WITH p = Filter.Child(info.filter) DO
            PixmapVBT.Put(p, HTMLVBTG.ErrorImage);
            PixmapVBT.SetColors(
              p, op := HTMLVBTG.ErrorColors.bgFg, 
              bg := HTMLVBTG.ErrorColors.bg)
          END
        END
    END
  END LoadOblet;

PROCEDURE ParseRd (p: ObliqParser.T; fullURL: TEXT; rd: Rd.T): Obliq.Env
    RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    env: Obliq.Env;
  BEGIN
    WITH baseURL = Text.Sub (fullURL, 0, Text.FindCharR (fullURL, '/') + 1),
         e0 = Obliq.EmptyEnv(),
         e1 = Obliq.NewEnv ("FullURL", Obliq.NewText (fullURL), e0),
         e2 = Obliq.NewEnv ("BaseURL", Obliq.NewText (baseURL), e1) DO
      env := e2;
    END;

    ObliqParser.ReadFrom(p, fullURL, rd, TRUE);
    TRY
      LOOP
        EVAL ObliqParser.EvalPhrase (p, ObliqParser.ParsePhrase (p), env)
      END
    EXCEPT
      ObliqParser.Eof =>         (* clean exit of loop *)
    END;
    RETURN env
  END ParseRd;


TYPE
  ObletClosure = Thread.Closure OBJECT
                   obj: Obliq.Val;
                   swr: SynWr.T;
                 OVERRIDES
                   apply := DoRunOblet
                 END;

PROCEDURE DoRunOblet (cl: ObletClosure): REFANY =
  (* The "run" method on the obliq object is called without VBT.mu locked. *)
  BEGIN
    TRY
      EVAL Obliq.ObjectInvoke(cl.obj, "run", Obliq.Vals{})
    EXCEPT
    | ObValue.Error (packet) =>
      Obliq.ReportError (cl.swr, packet);
    | ObValue.Exception (packet) =>
      Obliq.ReportException (cl.swr, packet);
    END;
    RETURN NIL
  END DoRunOblet;

BEGIN
  SynWr.Setup();
  ObliqParser.PackageSetup();
  ObLibM3.PackageSetup();
  ObLibUI.PackageSetup();
  ObLibAnim.PackageSetup();
  ObLibWeb.PackageSetup();

END Oblet.
