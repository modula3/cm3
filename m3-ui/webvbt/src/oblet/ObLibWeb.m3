(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sun Dec 31 02:19:21 PST 1995 by najork                   *)
(*       Created on Thu Oct 19 11:16:29 PDT 1995 by najork                   *)


UNSAFE MODULE ObLibWeb;

IMPORT ObLib, ObLibUI, ObValue, Obliq, SynLocation, SynWr, TextList, VBT, 
       Web, WebVBT;

VAR setupDone := FALSE;

PROCEDURE PackageSetup () =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      Setup ();
    END;
  END PackageSetup;

PROCEDURE Setup () =
  BEGIN
    SetupWeb ();
  END Setup;

(* ============ "web" package ============ *)

TYPE
  ValWebVBT = ObLibUI.ValVBT BRANDED OBJECT END;

  WebCode = {Failure, New, Fetch, Stop, GetLinks, GetTitle, Search,
             AttachReadyProc, AttachHotlinkProc, AttachIsMapProc, 
             AttachIsIndexProc, AbsoluteURL};

  WebOpCode = ObLib.OpCode OBJECT
    code: WebCode;
  END;
    
  PackageWeb = ObLib.T OBJECT
  OVERRIDES
    Eval := EvalWeb;
  END;


VAR webFailureException: ObValue.ValException;

PROCEDURE SetupWeb() =

  PROCEDURE NewWebOC (name: TEXT; arity: INTEGER; code: WebCode): WebOpCode =
    BEGIN
      RETURN NEW (WebOpCode, name := name, arity := arity, code := code);
    END NewWebOC;

  TYPE 
    OpCodes = ARRAY OF ObLib.OpCode;
  VAR 
    opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (WebCode));
    opCodes^ := 
        OpCodes{NewWebOC("failure",          -1, WebCode.Failure), 
                NewWebOC("new",               0, WebCode.New), 
                NewWebOC("fetch",             2, WebCode.Fetch), 
                NewWebOC("stop",              1, WebCode.Stop),
                NewWebOC("getLinks",          1, WebCode.GetLinks),
                NewWebOC("getTitle",          1, WebCode.GetTitle),
                NewWebOC("search",            2, WebCode.Search),
                NewWebOC("attachReadyProc",   2, WebCode.AttachReadyProc),
                NewWebOC("attachHotlinkProc", 2, WebCode.AttachHotlinkProc),
                NewWebOC("attachIsMapProc",   2, WebCode.AttachIsMapProc),
                NewWebOC("attachIsIndexProc", 2, WebCode.AttachIsIndexProc),
                NewWebOC("absoluteURL",       2, WebCode.AbsoluteURL)
               };
    ObLib.Register (NEW (PackageWeb, name := "web", opCodes := opCodes));
    webFailureException := NEW (ObValue.ValException, name := "web_failure");
  END SetupWeb;


PROCEDURE EvalWeb(                    self  : PackageWeb; 
                                      opCode: ObLib.OpCode; 
                  <*UNUSED*>          arity : ObLib.OpArity; 
                             READONLY args  : ObValue.ArgArray; 
                  <*UNUSED*>          temp  : BOOLEAN; 
                                      loc   : SynLocation.T): ObValue.Val 
  RAISES {ObValue.Error} =
  VAR 
    webvbt      : MyWebVBT;
    text1, text2: TEXT; 
  BEGIN
    CASE NARROW(opCode, WebOpCode).code OF
    | WebCode.Failure => 
        RETURN webFailureException;
    | WebCode.New => 
      WITH val = NEW (ValWebVBT, what := "<a WebVBT.T>"),
           vbt = NEW (MyWebVBT, val:=val, loc:=loc).init () DO
        val.vbt := vbt;
        RETURN val;
      END;
    | WebCode.Fetch => 
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
      ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
      webvbt.fetch (text1);
      RETURN ObValue.valOk;
    | WebCode.Stop => 
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      webvbt.stop ();
      RETURN ObValue.valOk;
    | WebCode.GetLinks =>
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      WITH links = webvbt.getLinks () DO
        IF links = NIL THEN 
          RETURN Obliq.NewArray (Obliq.Vals {});
        ELSE
          WITH n = TextList.Length (links),
               vals = NEW (REF Obliq.Vals, n) DO
            FOR i := 0 TO n - 1 DO
              vals[i] := Obliq.NewText (TextList.Nth (links, i));
            END;
            RETURN Obliq.NewArray (vals^);
          END;
        END;
      END;
    | WebCode.GetTitle =>
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      TYPECASE webvbt.page OF
      | NULL => 
        RETURN Obliq.NewText ("");
      | WebVBT.HTMLPage (page) => 
        RETURN Obliq.NewText (page.html.title);
      ELSE
        RETURN Obliq.NewText ("<Untitled>");
      END;
    | WebCode.Search =>
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
      ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
      RETURN Obliq.NewBool (webvbt.search(text1));
    | WebCode.AttachReadyProc => 
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      webvbt.readyProc := args[2];
      RETURN ObValue.valOk;
    | WebCode.AttachHotlinkProc => 
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      webvbt.hotLinkProc := args[2];
      RETURN ObValue.valOk;
    | WebCode.AttachIsMapProc =>
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      webvbt.isMapProc := args[2];
      RETURN ObValue.valOk;
    | WebCode.AttachIsIndexProc =>
      TYPECASE args[1] OF | ValWebVBT (node) => webvbt := node.vbt;
      ELSE ObValue.BadArgType (1, "web", self.name, opCode.name, loc); END;
      webvbt.isIndexProc := args[2];
      RETURN ObValue.valOk;
    | WebCode.AbsoluteURL =>
      TYPECASE args[1] OF | ObValue.ValText(node) => text1:=node.text;
      ELSE ObValue.BadArgType(1, "text", self.name, opCode.name, loc); END;
      TYPECASE args[2] OF | ObValue.ValText(node) => text2:=node.text;
      ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
      RETURN Obliq.NewText (Web.AbsoluteURL (text1, text2));
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <* ASSERT FALSE *>
    END;
  END EvalWeb;


TYPE
  MyWebVBT = WebVBT.T BRANDED OBJECT
    val        : ValWebVBT;
    loc        : SynLocation.T;
    readyProc  : ObValue.ValFun := NIL;
    hotLinkProc: ObValue.ValFun := NIL;
    isMapProc  : ObValue.ValFun := NIL;
    isIndexProc: ObValue.ValFun := NIL;
  OVERRIDES
    hotlink := Hotlink;
    ready   := Ready;
    ismap   := IsMap;
    isindex := IsIndex;
  END;


PROCEDURE Ready (self: MyWebVBT; remImages: CARDINAL) =
  VAR 
    args: ARRAY [1..2] OF ObValue.Val;
  BEGIN
    args[1] := self.val;
    args[2] := Obliq.NewInt (remImages);
    TRY
      IF self.readyProc # NIL THEN
        EVAL Obliq.Call (self.readyProc, args, self.loc);
      END;
    EXCEPT
    | ObValue.Error (packet) => 
      SynWr.Text (SynWr.out,
           "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
      ObValue.ErrorMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    | ObValue.Exception (packet) => 
      SynWr.Text(SynWr.out, 
           "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
      ObValue.ExceptionMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    END;
  END Ready;


PROCEDURE Hotlink (                    self: MyWebVBT; 
                                       link: TEXT; 
                   <*UNUSED*> READONLY cd  : VBT.MouseRec) =
  VAR 
    args: ARRAY [1..2] OF ObValue.Val;
  BEGIN
    args[1] := self.val;
    args[2] := Obliq.NewText (link);
    TRY
      IF self.hotLinkProc # NIL THEN
        EVAL Obliq.Call (self.hotLinkProc, args, self.loc);
      END;
    EXCEPT
    | ObValue.Error (packet) => 
      SynWr.Text (SynWr.out,
           "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
      ObValue.ErrorMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    | ObValue.Exception (packet) => 
      SynWr.Text(SynWr.out, 
           "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
      ObValue.ExceptionMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    END;
  END Hotlink;


PROCEDURE IsMap(                    self: MyWebVBT; 
                                    link: TEXT; 
                <*UNUSED*> READONLY cd  : VBT.MouseRec) =
  VAR 
    args: ARRAY [1..2] OF ObValue.Val;
  BEGIN
    args[1] := self.val;
    args[2] := Obliq.NewText (link);
    TRY
      IF self.isMapProc # NIL THEN
        EVAL Obliq.Call (self.isMapProc, args, self.loc);
      END;
    EXCEPT
    | ObValue.Error (packet) => 
      SynWr.Text (SynWr.out,
           "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
      ObValue.ErrorMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    | ObValue.Exception (packet) => 
      SynWr.Text(SynWr.out, 
           "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
      ObValue.ExceptionMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    END;
  END IsMap;


PROCEDURE IsIndex (self: MyWebVBT; typein: TEXT) =
    VAR 
    args: ARRAY [1..2] OF ObValue.Val;
  BEGIN
    args[1] := self.val;
    args[2] := Obliq.NewText (typein);
    TRY
      IF self.isIndexProc # NIL THEN
        EVAL Obliq.Call (self.isIndexProc, args, self.loc);
      END;
    EXCEPT
    | ObValue.Error (packet) => 
      SynWr.Text (SynWr.out,
           "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
      ObValue.ErrorMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    | ObValue.Exception (packet) => 
      SynWr.Text(SynWr.out, 
           "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
      ObValue.ExceptionMsg (SynWr.out, packet);
      SynWr.Flush (SynWr.out);
    END;
  END IsIndex;


BEGIN
END ObLibWeb.
