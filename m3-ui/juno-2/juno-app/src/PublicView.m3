(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 14:57:07 PST 1996 by heydon                   *)
<* PRAGMA LL *>

MODULE PublicView;

IMPORT Editor, JunoConfig;
IMPORT JunoAST, JunoUnparse;
IMPORT TextPort;
IMPORT VBT, Rect;
IMPORT Wr, Formatter, TextWr, Thread;

<* FATAL Wr.Failure, Thread.Alerted *>

REVEAL
  T = Public BRANDED "PublicView.T" OBJECT
    e: Editor.T;
    width: INTEGER := -1;
  OVERRIDES
    init := Init;
    reshape := Reshape;
  END;

(* If "t" is a public view on a module editor, then "t.width" is the most
   recent width at which the text was unparsed. Since the text in a module
   public view is read-only, we don't have to unparse the trees if the width
   has not changed since the last unparsing. *)

PROCEDURE Init(pv: T; e: Editor.T): T =
  BEGIN
    EVAL TextPort.T.init(pv, font := JunoConfig.codeFont,
      readOnly := TRUE, wrap := FALSE);
    pv.e := e;
    RETURN pv;
  END Init;

PROCEDURE Reshape(pv: T; READONLY cd: VBT.ReshapeRec) =
  <* LL.sup = VBT.mu.v *>
  BEGIN
    IF NOT Rect.IsEmpty(cd.new) THEN
      (* the public view child was just selected *)
      RedoText(pv)
    END;
    TextPort.T.reshape(pv, cd)
  END Reshape;

PROCEDURE RedoText(pv: T) =
  VAR width := Editor.Width(pv); BEGIN
    IF pv.width # width THEN
      pv.width := width;
      Unparse(pv, width)
    END
  END RedoText;

PROCEDURE Unparse(pv: T; width: CARDINAL) =
  VAR wr := TextWr.New(); BEGIN
    UnparseTrees(Editor.Trees(pv.e), wr, width);
    TextPort.SetModified(pv, TRUE);
    TextPort.SetText(pv, TextWr.ToText(wr));
    TextPort.SetModified(pv, FALSE);
    Wr.Close(wr)
  END Unparse;

PROCEDURE UnparseTrees(f: Editor.Forest; wr: Wr.T; width: CARDINAL) =
  PROCEDURE NewLine(fmt: Formatter.T) =
    BEGIN Formatter.NewLine(fmt, freshLine := FALSE) END NewLine;
  VAR fmt := Formatter.New(wr, width); BEGIN
    WHILE f # NIL DO
      TYPECASE f.tree OF
        JunoAST.Module (m) =>
          JunoUnparse.ToFmt(fmt, m, indent := 0,
            prec := JunoConfig.realPrec, private := FALSE);
          NewLine(fmt); NewLine(fmt); Formatter.Flush(fmt)
      | JunoAST.Comment (c) =>
          IF NOT c.private THEN
            JunoUnparse.ToFmt(fmt, c, indent := 0,
              prec := JunoConfig.realPrec, private := FALSE);
            NewLine(fmt); NewLine(fmt); Formatter.Flush(fmt)
          END
      | JunoAST.UIDecl (d) =>
          IF d.name = Editor.PointToolSym OR d.name = Editor.TextToolSym
             OR d.name = Editor.SetToolSym THEN
            JunoUnparse.ToFmt(fmt, d, indent := 0,
              prec := JunoConfig.realPrec, private := FALSE);
            NewLine(fmt); Formatter.Flush(fmt)
          END
      | JunoAST.Decl (d) =>
          IF NOT d.private THEN
            JunoUnparse.ToFmt(fmt, d, indent := 0,
              prec := JunoConfig.realPrec, private := FALSE);
            NewLine(fmt); Formatter.Flush(fmt)
          END
      ELSE (* SKIP *)
      END;
      f := f.next
    END;
    Formatter.Close(fmt)
  END UnparseTrees;

BEGIN
END PublicView.
