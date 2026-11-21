MODULE Cooker;
IMPORT Env;
IMPORT Text;
IMPORT TextList;
IMPORT Process;
IMPORT Fmt;
IMPORT Term;

(* terminal I/O utilities *)

PROCEDURE Input(prompt:="> "; completer: Completer := NIL;
                previous: TextList.T := NIL; default:="";
                fatalControl:=TRUE; emptySelectsLast:=FALSE;
                term: Term.T := NIL): TEXT =
  VAR
    c: CHAR;
    t := default;
    p := Text.Length(t);
    p0 := p;
    prev, next: TextList.T;
    shadow := t;
  PROCEDURE WipeLine() =
    BEGIN
      p0 := Text.Length(t); term.wr("\015"&prompt&t&"\033[K"); p:=p0;
    END WipeLine;
  PROCEDURE Recall(VAR from, to: TextList.T) =
    BEGIN
      IF from#NIL THEN
        to := TextList.Cons(shadow, to);
        shadow := from.head;
        from := from.tail;
        t := shadow;
        WipeLine();
      END;
    END Recall;
  PROCEDURE BWord(del: BOOLEAN) =
    BEGIN
      IF p#0 THEN
        WHILE p#0 AND Text.GetChar(t,p-1)=' ' DO DEC(p); END;
        WHILE p#0 AND Text.GetChar(t,p-1)#' ' DO DEC(p); END;
        IF del THEN
          t:=Text.Sub(t,0,p)&Text.Sub(t,p0);
          term.wr("\033["&Fmt.Int(p0-p)&"D"&Text.Sub(t,p)&"\033[K");
          p0:=Text.Length(t);
        END;
      END;
    END BWord;
  PROCEDURE FWord(del: BOOLEAN) =
    BEGIN
      IF p#Text.Length(t) THEN
        INC(p);
        WHILE p#Text.Length(t) AND Text.GetChar(t,p-1)#' ' DO INC(p); END;
        WHILE p#Text.Length(t) AND (p=1 OR Text.GetChar(t,p-2)=' ') DO
          INC(p); END;
        IF del THEN
          t:=Text.Sub(t,0,p0)&Text.Sub(t,p);
          p:=p0;
          term.wr(Text.Sub(t,p)&"\033[K");
          p0:=Text.Length(t);
        END;
      END;
    END FWord;
  PROCEDURE FDel() =
    BEGIN
      t := Text.Sub(t, 0, p) & Text.Sub(t, p+1);
      term.wr(Text.Sub(t, p)&" ");
      p0:=Text.Length(t)+1;
    END FDel;
  BEGIN
    prev := previous;
    next := NIL;
    IF term = NIL THEN
      term := Term.Default();
    END;
    term.wr(prompt & default);
    LOOP
      p0:=p;
      c := term.getChar();
      CASE c OF
      | '\033' =>
        c := term.getChar();
        CASE c OF
        | '[' =>
          c := term.getChar();
          CASE c OF
          | 'A' => IF next=NIL THEN shadow:=t; END; Recall(prev, next);
          | 'B' => Recall(next, prev);
          | 'C' => p:=MIN(Text.Length(t),p+1);
          | 'D' => p:=MAX(0,p-1);
          ELSE
          END;
        | 'B','b','\177','\010' => BWord(c='\177' OR c= '\010');
        | 'F','f','D','d' => FWord(c='D' OR c='d');
        ELSE
        END;
      | '\020' => IF next=NIL THEN shadow:=t; END; Recall(prev, next);
      | '\016' => Recall(next, prev);
      | '\004','\003','\032' =>
        IF c='\004' AND Text.Length(t)#0 THEN
          FDel();
        ELSIF fatalControl THEN
          Print(); Print("^C"); Process.Exit(1);
        ELSE
          RETURN Text.FromChar(c);
        END;
      | '\011' => IF completer#NIL THEN completer.do(t); WipeLine(); END;
      | '\015' =>
        IF emptySelectsLast AND Text.Equal(t,"") AND previous#NIL THEN 
          t:=previous.head; Print(t); RETURN t;
        ELSE
          term.wr("",TRUE,TRUE); RETURN t;
        END;
      | '\013' => term.wr("\033[K"); t:=Text.Sub(t,0,p);
      | '\006' => p:=MIN(Text.Length(t),p+1);
      | '\002' => p:=MAX(0,p-1);
      | '\001' => p:=0;
      | '\005' => p:=Text.Length(t);
      | '\302','\342','\210','\377' => BWord(c='\210' OR c='\377');
      | '\306','\346','\304','\344' => FWord(c='\304' OR c='\344');
      | '\177','\010'=> IF p>0 THEN DEC(p); term.wr("\010"); FDel(); END;
      ELSE
        IF ORD(c)>=32 AND ORD(c)<128 THEN
          term.wr(Text.FromChar(c));INC(p0);
          t:=Text.Sub(t,0,p)&Text.FromChar(c)&Text.Sub(t,p);
          INC(p);
          IF p#Text.Length(t) THEN
            term.wr(Text.Sub(t,p));
            p0:=Text.Length(t);
          END;
        ELSIF CookerDebug THEN
          Print(); Print("COOKERDEBUG: Charcode=" & Fmt.Unsigned(ORD(c),base:=8)); Print();
        END;
      END;
      (* Fix Cursor *)
      IF p<p0 THEN
        term.wr("\033["&Fmt.Int(p0-p)&"D");
      ELSIF p>p0 THEN
        term.wr("\033["&Fmt.Int(p-p0)&"C");
      END;
    END;
  END Input;

PROCEDURE Print(t:="") =
  BEGIN
    Term.WrLn(t);
  END Print;

VAR
  CookerDebug := Env.Get("COOKERDEBUG") # NIL;

BEGIN
END Cooker.
