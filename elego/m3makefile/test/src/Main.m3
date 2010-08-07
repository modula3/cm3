(*--------------------------------------------------------------------------*)
MODULE Main;

IMPORT Msg, M3makefile, Params, OSError, TextSeq, Text;

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToText(seq : TextSeq.T; sep := " "; maxCol := 0;
                        contToken := "\\\n") : TEXT =
  VAR
    t    := "";
    col  := 0;
    e    :  TEXT;
    len  :  CARDINAL;
    slen := Text.Length(sep);
  BEGIN
    IF seq.size() = 1 THEN
      RETURN seq.get(0);
    ELSIF seq.size() > 1 THEN
      t := seq.get(0);
      IF maxCol > 0 THEN
        col := Text.Length(t);
      END;
      FOR i := 1 TO seq.size() - 1 DO
        IF maxCol > 0 THEN
          e := seq.get(i);
          len := Text.Length(e);
          IF col + len + slen > maxCol THEN
            col := len + slen;
            t := t & sep & contToken & sep & e;
          ELSE
            t := t & sep & e;
            INC(col, len + slen);
          END;
        ELSE
          t := t & sep & seq.get(i);
        END;
      END;
    END;
    RETURN t;
  END TextSeqToText;

(*--------------------------------------------------------------------------*)
PROCEDURE WriteDeclList(t : TEXT; l : M3makefile.M3DeclarationList) =
  BEGIN
    Msg.T("----------------------------------------------------------------");
    Msg.T(t);
    WHILE l # NIL DO
      IF l.type # NIL AND l.name # NIL THEN
        Msg.T("type = " & l.type & ", name = " & l.name);
      ELSIF l.type # NIL THEN
        Msg.T("type = " & l.type & ", name = NIL");
      END;
      Msg.T("args = " & TextSeqToText(l.args));
      l := l.next;
    END;
  END WriteDeclList;
    
(*--------------------------------------------------------------------------*)
VAR
  fn  : TEXT;
  mkf : M3makefile.T;
BEGIN (* Main *)
  Msg.tFlag := TRUE;
  Msg.dFlag := TRUE;
  IF Params.Count < 2 THEN
    Msg.Fatal("usage: m3makefile-test <m3makefile>");
  END;
  fn := Params.Get(1);
  Msg.T("reading file " & fn);
  TRY
    mkf := NEW(M3makefile.T).init(fn);
  EXCEPT
    OSError.E => Msg.Fatal("OSError");
  | M3makefile.ParseError(e) => Msg.Fatal("ParseError: " & e);
  END;
  WriteDeclList("Elements", mkf.elements());
  WriteDeclList("DeclList(import)", mkf.declList("import"));
  Msg.T("----------------------------------------------------------------");
  Msg.T("decls(import): " & TextSeqToText(mkf.decls("import")));
  Msg.T("----------------------------------------------------------------");
  Msg.T("target name: " & mkf.targetName());
  IF mkf.targetType() = M3makefile.TargetType.Program THEN
    Msg.T("target is a program");
  ELSIF mkf.targetType() = M3makefile.TargetType.Library THEN
    Msg.T("target is a library");
  ELSE
    Msg.T("target type is unknown");
  END;
  IF mkf.unknownElements() THEN
    Msg.T("contains unknown elements")
  END
END Main.
