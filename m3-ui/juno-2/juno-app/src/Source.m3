(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 14:01:00 PST 1997 by heydon                   *)
(*      modified on Wed Aug  2 16:17:14 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:58 PDT 1992 by myers                    *)
<* PRAGMA LL *>

MODULE Source;

IMPORT CurrCmd, JunoBuild, View, JunoError, Editor, Drawing, JunoConfig;
FROM JunoHandleLexErr IMPORT HandleLexErr;
IMPORT JunoAST, JunoCompileErr, JunoToken, JunoLex, JunoParse, JunoUnparse;
IMPORT TextPort, TextEditVBT;
IMPORT VBT, Axis, Rect;
IMPORT Text, Rd, Wr, Thread, TextRd, TextWr, Stdio;

REVEAL
  T = Public BRANDED "Source.T" OBJECT
    port: Port
  OVERRIDES
    init := Init;
    update := Update
  END;

TYPE
  Port = TextPort.T BRANDED "Source.Port" OBJECT
    src: T;				 (* the Source.T around this port *)
    width := -1;
    ignoreModifies := FALSE;
  OVERRIDES
    <* LL.sup = VBT.mu *>
    key      := Key;
    <* LL.sup = VBT.mu.SELF *>
    reshape  := Reshape;
    shape    := Shape;
    <* LL.sup < VBT.mu *>
    modified := Modified
  END;

(* A "Source.T"'s filter child is a "TextEditVBT.T" with a port of type
   "Port". The "width" of a "Port" is the most recent width it was unparsed
   to, or "-1" if it was unparsed to a window with an empty domain. *)

PROCEDURE Init(self: T; root: View.Root): T =
  VAR
    port := NEW(Port, src := self).init(
      wrap := FALSE, font := JunoConfig.codeFont);
    child := NEW(TextEditVBT.T, tp := port).init();
  BEGIN
    self.root := root;
    self.port := port;
    RETURN View.T.init(self, child);
  END Init;

PROCEDURE Update(s: T) =
  <* LL.sup <= VBT.mu *>
  VAR port := s.port; BEGIN
    IF Rect.IsEmpty(VBT.Domain(port))
      THEN port.width := -1
      ELSE UpdatePort(port, Editor.Width(port))
    END
  END Update;

PROCEDURE UpdatePort(port: Port; width: INTEGER) =
  <* LL.sup <= VBT.mu *>
  VAR v := port.src; BEGIN
    <* ASSERT v.root.astTrue *>
    IF (NOT v.root.sTrue) OR width # port.width THEN
      VAR
        wr := TextWr.New();
        cpos := TextPort.Index(port);
        ast := CurrCmd.GetAST(v.root.ccmd);
        <* FATAL Thread.Alerted, Wr.Failure *>
      BEGIN
        JunoUnparse.Cmd(wr, ast, LAST(CARDINAL),
          width := width, prec := JunoConfig.realPrec);
        SetTextPort(port, TextWr.ToText(wr));
        TextPort.Normalize(port, cpos);
        Wr.Close(wr);
        v.root.sTrue := TRUE;
        port.width := width
      END
    END
  END UpdatePort;

PROCEDURE SetTextPort(port: Port; t: TEXT) =
  BEGIN
    port.ignoreModifies := TRUE;
    TextPort.SetText(port, t);
    port.ignoreModifies := FALSE
  END SetTextPort;

(* If continuous unparsing is turned off, then it is possible that the AST
   is up-to-date, but the source does not reflect changes made through the
   drawing view. In this case, the user should not be able to type in the
   source window until it has been updated from the AST. *)

PROCEDURE Key(self: Port; READONLY cd: VBT.KeyRec) =
  <* LL.sup = VBT.mu *>
  BEGIN
    WITH root = self.src.root DO
      IF root.astTrue AND NOT root.sTrue
    	THEN JunoError.Display(self, "Oops! You forgot to click Run.")
    	ELSE TextPort.T.key(self, cd)
      END
    END
  END Key;

PROCEDURE Reshape(port: Port; READONLY cd: VBT.ReshapeRec) =
(* "Reshape" reformats its contents according to the new width (if any), then
   reshapes the textport normally. *)
  <* LL.sup = VBT.mu.port *>
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      port.width := -1
    ELSE
      IF port.src.root.astTrue THEN
        UpdatePort(port, Editor.Width(port))
      END
    END;
    TextPort.T.reshape(port, cd)
  END Reshape;

PROCEDURE Shape(port: Port; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  <* LL.sup = VBT.mu.port *>
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN VBT.DefaultShape
    ELSE
      VAR res := TextPort.T.shape(port, ax, n); BEGIN
        res.lo := 0;
        res.hi := VBT.DefaultShape.hi;
        RETURN res
      END
    END
  END Shape;

PROCEDURE Modified(port: Port) =
(* This procedure is called by the underlying TextPort when its text is
   modified. *)
  <* LL.sup < VBT.mu *>
  BEGIN
    TextPort.T.modified(port);
    IF port.ignoreModifies
      THEN TextPort.SetModified(port, FALSE)
      ELSE port.src.modified(how := View.ModKind.Explicit)
    END
  END Modified;

PROCEDURE ShowError(
    s: T; ast: JunoAST.T;
    READONLY err: JunoCompileErr.ErrVal;
    ts: VBT.TimeStamp) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR txt: TEXT; start, finish: INTEGER; BEGIN
    VAR wr := TextWr.New(); BEGIN
      JunoUnparse.P(wr, ast, width := Editor.Width(s.port),
        prec := JunoConfig.realPrec, errast := err.ast);
      txt := TextWr.ToText(wr);
      Wr.Close(wr)
    END;
    start := Text.FindChar(txt, '\001');
    finish := Text.FindChar(txt, '\002');
    IF start >= 0 AND finish > start THEN
      txt := Text.Sub(txt, 0, start)
        & Text.Sub(txt, start + 1, finish - start - 1)
        & Text.Sub(txt, finish + 1);
      SetTextPort(s.port, txt);
      JunoError.P(s.port, err.msg, start, finish - 1, ts)
    ELSE
      Wr.PutText(Stdio.stderr, err.msg & "\n");
      Wr.PutText(Stdio.stderr, "Error AST:\n");
      JunoUnparse.Debug(err.ast);
      Wr.PutText(Stdio.stderr, "Original AST:\n");
      JunoUnparse.Debug(ast)
    END
  END ShowError;

PROCEDURE Parse(s: T; time: VBT.TimeStamp): JunoAST.Cmd =
  BEGIN
    (* use cached version if it is valid *)
    IF s.root.astTrue THEN RETURN CurrCmd.GetAST(s.root.ccmd) END;

    (* otherwise, parse the contents of the textport *)
    VAR res := ParseFromPort(s.port, time); BEGIN
      IF res # NIL THEN CurrCmd.ChangeAST(s.root.ccmd, res) END;
      RETURN res
    END
  END Parse;

PROCEDURE ParseFromPort(port: Port; time: VBT.TimeStamp): JunoAST.Cmd =
(* Returns the parsed current command from "port", or "NIL" if there
   was a lex or parse error. In the event of either error, the error is
   underlined using timestamp "time", and an error window is displayed. *)
  <* FATAL Rd.Failure, Thread.Alerted, Wr.Failure *>
  VAR
    res: JunoAST.Cmd;
    rd := TextRd.New(TextPort.GetText(port));
    tokens: CARDINAL;
  BEGIN
    TRY JunoParse.Command(rd, res, tokens) EXCEPT
      JunoLex.Error (err) =>
        VAR wr := TextWr.New(); start, finish: INTEGER; BEGIN
  	  JunoUnparse.Cmd(wr, res, tokens,
            width := Editor.Width(port), prec := JunoConfig.realPrec);
  	  HandleLexErr(err, rd, wr, start, finish);
  	  SetTextPort(port, TextWr.ToText(wr));
          Wr.Close(wr);
  	  JunoError.P(port, JunoLex.ErrorText(err.kind), start, finish, time)
        END;
        res := NIL
    | JunoParse.Error (err) =>
        IF tokens = 0 AND err.found.kind = JunoToken.Kind.EndMarker THEN
          res := JunoAST.SkipVal
        ELSE
          VAR wr := TextWr.New(); start, finish: CARDINAL; BEGIN
            JunoUnparse.Cmd(wr, res, tokens,
              width := Editor.Width(port), prec := JunoConfig.realPrec);
            Wr.PutChar(wr, '\n');
            start := Wr.Index(wr);
            Wr.PutText(wr, JunoToken.ToText(err.found));
            finish := Wr.Index(wr);
            Wr.PutChar(wr, ' ');
            Wr.PutText(wr, err.additional);
            Wr.PutText(wr, Rd.GetText(rd, LAST(CARDINAL)));
            SetTextPort(port, TextWr.ToText(wr));
            Wr.Close(wr);
            JunoError.P(port, "Parse error", start, finish, time)
          END;
          res := NIL
        END
    END;
    Rd.Close(rd);
    RETURN res
  END ParseFromPort;

PROCEDURE Compile(s: T; time: VBT.TimeStamp; skipify: BOOLEAN): BOOLEAN =
  BEGIN
    IF s.root.astTrue AND s.root.ccmd.codeValid
      AND s.root.ccmd.skipify = skipify THEN
      RETURN TRUE
    END;
    RETURN Compile2(s, time, skipify)
  END Compile;

PROCEDURE Compile2(s: T; time: VBT.TimeStamp; skipify: BOOLEAN): BOOLEAN =
  VAR port: Port := s.port; ast: JunoAST.Cmd := Parse(s, time); BEGIN
    (* check for parse error *)
    IF ast = NIL THEN RETURN FALSE END;
    (* recompile and update drawing if necessary *)
    WITH cc = s.root.ccmd DO
      IF NOT cc.codeValid OR skipify # cc.skipify THEN
        TRY
          VAR astp := ast; BEGIN
            IF skipify THEN astp := CurrCmd.Skipify(ast) END;
            cc.slot := JunoBuild.CurrCmd(astp,
              CurrCmd.GetScope(cc), checkTotal := TRUE);
            cc.codeValid := TRUE;
            cc.skipify := skipify
          END
        EXCEPT
          JunoCompileErr.Error (err) =>
            ShowError(s, ast, err, time);
            RETURN FALSE
        END;
        TextPort.SetModified(port, FALSE);
        s.root.astTrue := TRUE
      END
    END;
    RETURN TRUE
  END Compile2;

PROCEDURE Make(s: T; time: VBT.TimeStamp; skipify: BOOLEAN): BOOLEAN =
  BEGIN
    IF NOT Compile(s, time, skipify) THEN RETURN FALSE END;
    Drawing.Make(s.root.drawing, skipify);
    s.update();
    RETURN TRUE
  END Make;

PROCEDURE GetText(s: T): TEXT =
  BEGIN 
    RETURN TextPort.GetText(s.port) 
  END GetText;

PROCEDURE SetText(s:T; txt: TEXT) =
  BEGIN
    TextPort.SetText(s.port, txt)
  END SetText;
  
BEGIN
END Source.
