(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 11:12:32 PST 1995 by kalsow                   *)
(*      modified on Thu Jun  3 10:09:10 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 21:55:36 PDT 1992 by muller                   *)
<* PRAGMA LL *>

MODULE Manpage;

IMPORT Fmt, FormsVBT, MTextRd, Rd, RdUtils, Rsrc, Text, TextEditVBT,
       TextPort, Thread, VBT, VTDef, VText;


TYPE
  T = FormsVBT.Closure OBJECT
        rd, revRd: MTextRd.T;
        textport : TextPort.T;
        length   : CARDINAL;
        er       : ErrorReporter;
        ready    : Thread.Condition;
        done                          := FALSE; <* LL = VBT.mu *>
        helpfindfirst, helpfindnext, helpfindprev, helpfindtext, notfound,
          helpcase: TEXT := NIL;
        caseSensitive := FALSE
      OVERRIDES
        apply := Help
      END;
  HelpThreadClosure = Thread.Closure OBJECT
                        t       : T;
                        resource: TEXT;
                        path    : Rsrc.Path
                      OVERRIDES
                        apply := ReadManpage
                      END;

PROCEDURE Init (fv           : FormsVBT.T;
                resource     : TEXT;
                er           : ErrorReporter;
                helpfindfirst                  := "helpfindfirst";
                helpfindnext                   := "helpfindnext";
                helpfindprev                   := "helpfindprev";
                helpfindtext                   := "helpfindtext";
                manpagetext                    := "manpagetext";
                notfound                       := "notfound";
                helpcase                       := "helpcase";
                path         : Rsrc.Path       := NIL              )
  RAISES {FormsVBT.Error} =
  VAR
    t := NEW (T, er := er, ready := NEW (Thread.Condition),
              helpfindfirst := helpfindfirst, helpfindnext := helpfindnext,
              helpfindprev := helpfindprev, helpfindtext := helpfindtext,
              notfound := notfound, helpcase := helpcase);
    htc := NEW (HelpThreadClosure, t := t, resource := resource, path := path);
  BEGIN
    TYPECASE FormsVBT.GetVBT (fv, manpagetext) OF
    | TextEditVBT.T (x) => t.textport := x.tp
    ELSE
      RAISE FormsVBT.Error (
              "\"" & manpagetext & "\" is not the name of a TextEdit form")
    END;
    FormsVBT.Attach (fv, helpfindfirst, t);
    FormsVBT.Attach (fv, helpfindnext, t);
    FormsVBT.Attach (fv, helpfindprev, t);
    FormsVBT.Attach (fv, helpfindtext, t);
    IF helpcase # NIL THEN FormsVBT.Attach (fv, helpcase, t) END;
    EVAL Thread.Fork (htc)
  END Init;

PROCEDURE ReadManpage (htc: HelpThreadClosure): REFANY =
  <* LL = 0 *>
  VAR
    rd: Rd.T;
    t        := htc.t;
  PROCEDURE oops (msg: TEXT) =
    BEGIN
      LOCK VBT.mu DO
        t.er.apply (
          Fmt.F ("Sorry, couldn't read the manpage in %s\nError: %s\n",
                 htc.resource, msg))
      END
    END oops;
  BEGIN
    TRY                         (* Fetch the file in non-event time. *)
      rd := Rsrc.Open (htc.resource, htc.path);
      LOCK VBT.mu DO
        (* MText and (therefore) VText support a "text segment" that is
           actually just a reader, and they don't actually copy bytes until
           they're needed. *)
        WITH vtext = TextPort.GetVText (t.textport),
             mtext = vtext.mtext                     DO
          VText.ReplaceFile (
            vtext, begin := 0, end := LAST (CARDINAL), file := rd);
          VBT.Mark (t.textport);
          (* Create forward- and reverse-readers for searching. *)
          t.rd := MTextRd.New (mtext);
          t.revRd := MTextRd.New (mtext, reverse := TRUE);
          t.length := Rd.Length (rd)
        END
      END
    EXCEPT
    | Rd.Failure (ref) => oops (RdUtils.FailureText (ref))
    | Rd.EndOfFile => oops ("End of file")
    | VTDef.Error (code) => oops (VTDef.ErrorCodeTexts [code])
    | Thread.Alerted => oops ("interrupted (Thread.Alerted)")
    | Rsrc.NotFound => oops ("No such resource: " & htc.resource)
    END;
    LOCK VBT.mu DO t.done := TRUE END;
    Thread.Signal (t.ready);
    RETURN NIL
  END ReadManpage;

PROCEDURE Help (t: T; fv: FormsVBT.T; name: TEXT; time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR
    pattern: TEXT;
    pos    : INTEGER;
    n      : CARDINAL;
    x      : TextPort.Extent;
  PROCEDURE show (start: INTEGER) RAISES {FormsVBT.Error} =
    BEGIN
      IF pos >= 0 THEN
        TextPort.Select (
          t.textport, time, start, start + n, replaceMode := TRUE);
        TextPort.Normalize (t.textport, start)
      ELSIF t.notfound # NIL THEN
        FormsVBT.PopUp (fv, t.notfound);
        EVAL Thread.Fork (NEW (PDNF, t := t, fv := fv))
      END
    END show;
  BEGIN
    (* Wait for ReadManpage to finish. *)
    WHILE NOT t.done DO Thread.Wait (VBT.mu, t.ready) END;
    x := TextPort.GetSelection (t.textport);
    TRY
      TRY
        pattern := FormsVBT.GetText (fv, t.helpfindtext)
      EXCEPT
      | FormsVBT.Unimplemented =>
          t.er.apply (t.helpfindtext & " is not a text component [Manpage]");
          RETURN
      END;
      n := Text.Length (pattern);
      IF n = 0 THEN
        RETURN
      ELSIF Text.Equal (name, t.helpfindfirst)
              OR Text.Equal (name, t.helpfindtext)
              OR Text.Equal (name, t.helpfindnext) THEN
        IF Text.Equal (name, t.helpfindnext)
             OR Text.Equal (name, t.helpfindtext) THEN
          Rd.Seek (t.rd, x.r)
        ELSE
          Rd.Seek (t.rd, 0)
        END;
        IF t.caseSensitive THEN
          pos := RdUtils.Find (t.rd, pattern)
        ELSE
          pos := RdUtils.Find (t.rd, pattern, RdUtils.ToUpperCaseASCII)
        END;
        show (pos)
      ELSIF Text.Equal (name, t.helpfindprev) THEN
        Rd.Seek (t.revRd, t.length - x.l);
        IF t.caseSensitive THEN
          pos := RdUtils.Find (t.revRd, TextReverse (pattern));
        ELSE
          pos := RdUtils.Find (
                   t.revRd, TextReverse (pattern), RdUtils.ToUpperCaseASCII)
        END;
        show (t.length - pos - n)
      ELSIF t.helpcase # NIL AND Text.Equal (name, t.helpcase) THEN
        t.caseSensitive := FormsVBT.GetBoolean (fv, name)
      END
    EXCEPT
    | FormsVBT.Error (msg) => t.er.apply (msg)
    | FormsVBT.Unimplemented =>
        t.er.apply (name & " is not a Boolean component [Manpage]")
    | Thread.Alerted =>
    | Rd.Failure (ref) => t.er.apply (RdUtils.FailureText (ref))
    END
  END Help;

TYPE
  PDNF = Thread.Closure OBJECT
           t : T;
           fv: FormsVBT.T
         OVERRIDES
           apply := PopDownNotFound
         END;

PROCEDURE PopDownNotFound (cl: PDNF): REFANY =
  BEGIN
    Thread.Pause (2.0D0);
    LOCK VBT.mu DO
      TRY
        FormsVBT.PopDown (cl.fv, cl.t.notfound)
      EXCEPT
      | FormsVBT.Error (msg) => cl.t.er.apply (msg)
      END
    END;
    RETURN NIL
  END PopDownNotFound;
  

PROCEDURE TextReverse (t: TEXT): TEXT =
  VAR
    len : CARDINAL          := Text.Length (t);
    buf : REF ARRAY OF CHAR;
    i, j: CARDINAL;
    c   : CHAR;
  BEGIN
    buf := NEW (REF ARRAY OF CHAR, len);
    Text.SetChars (buf^, t);
    i := 0;
    j := len - 1;
    WHILE i < j DO
      c := buf [i];
      buf [i] := buf [j];
      buf [j] := c;
      INC (i);
      DEC (j)
    END;
    RETURN Text.FromChars (buf^)
  END TextReverse;

BEGIN END Manpage.
