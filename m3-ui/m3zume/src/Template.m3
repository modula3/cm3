(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Aug 22 15:47:48 PDT 1996 by najork  *)
(*      modified on Thu Feb  9 09:04:34 PST 1995 by kalsow  *)

MODULE Template;

IMPORT Wr, Thread, EventFile, Text, TextF, Process, TextTextTbl;

TYPE
  OutBuf = RECORD
    wr      : Wr.T          := NIL;
    len     : INTEGER       := 0;
    imports : TextTextTbl.T := NIL;
    newline : BOOLEAN       := FALSE;
    buf     : ARRAY [0..1023] OF CHAR;
  END;

TYPE
  Marker = RECORD
    full_line : BOOLEAN;
    len       : [0..255];
    name      : TEXT;
  END;

CONST
  ModeName = ARRAY EventFile.ArgMode OF TEXT { "", "READONLY " };

CONST (* marker types *)
  Markers = ARRAY OF Marker {
    (*  0 *) Marker { TRUE,   9, "#{_OUTPUT" },
    (*  1 *) Marker { TRUE,   9, "#{_UPDATE" },
    (*  2 *) Marker { TRUE,  11, "#{_FEEDBACK" },
    (*  3 *) Marker { TRUE,   2, "#{" },  (* all events *)
    (*  4 *) Marker { TRUE,   2, "#}" },
    (*  5 *) Marker { TRUE,   2, "#|" },
    (*  6 *) Marker { FALSE, 12, "#(_ALGNAME_)" },
    (*  7 *) Marker { FALSE, 13, "#(_VIEWNAME_)" },
    (*  8 *) Marker { FALSE, 12, "#(_IMPORTS_)" },
    (*  9 *) Marker { FALSE, 10, "#(_EVENT_)" },
    (* 10 *) Marker { FALSE, 15, "#(_EVENTSTYLE_)" },
    (* 11 *) Marker { FALSE, 11, "#(_ARGSTR_)" },
    (* 12 *) Marker { FALSE, 18, "#(_NONULL_ARGSTR_)" },
    (* 13 *) Marker { FALSE, 16, "#(_SEMI_ARGSTR_)" },
    (* 14 *) Marker { FALSE, 16, "#(_ARGSTR_SEMI_)" },
    (* 15 *) Marker { FALSE, 13, "#(_ARGTYPES_)" },
    (* 16 *) Marker { FALSE, 19, "#(_COMMA_ARGTYPES_)" },
    (* 17 *) Marker { FALSE, 20, "#(_SPACED_ARGTYPES_)" },
    (* 18 *) Marker { FALSE, 13, "#(_ARGNAMES_)" },
    (* 19 *) Marker { FALSE, 19, "#(_COMMA_ARGNAMES_)" },
    (* 20 *) Marker { FALSE, 12, "#(_ARGNAME_)" },
    (* 21 *) Marker { FALSE, 12, "#(_ARGTYPE_)" },
    (* 22 *) Marker { FALSE, 11, "#(_ARGFMT_)" },
    (* 23 *) Marker { FALSE, 14, "#(_EVENTPRIO_)" },
    (* 24 *) Marker { FALSE, 12, "#(_ALGDATA_)" },
    (* 25 *) Marker { FALSE, 12, "#(_ARGMODE_)" }
  };


PROCEDURE Generate (READONLY evt: EventFile.T;  algorithm, view: TEXT;
                    template: TEXT;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    cur       : INTEGER := 0;
    len       : INTEGER := Text.Length (template);
    buf       : OutBuf;
    restart   : INTEGER;

    in_event_loop := FALSE;
    event_restart : INTEGER;
    event_kind    : INTEGER;
    cur_event     : EventFile.Event := NIL;

    in_arg_loop   := FALSE;
    arg_restart   : INTEGER;
    cur_arg       : EventFile.EventArg := NIL;

  PROCEDURE StartLoop (kind: INTEGER) =
    BEGIN
      IF (in_event_loop) OR (in_arg_loop) THEN BadTemplate (); END;
      cur_event := NextEvent (evt.events, kind);
      IF (cur_event = NIL) THEN
        cur := SkipLoop (template, restart);
      ELSE
        in_event_loop := TRUE;
        event_restart := restart;
        event_kind    := kind;
        cur           := restart;
      END;
    END StartLoop;

  BEGIN
    buf.wr := wr;

    WHILE (cur < len) DO
      IF (template[cur] # '#') THEN
        Out (buf, template[cur]);  INC (cur);
      ELSE
        CASE Classify (template, cur, restart) OF

        |  0 => (* #{_OUTPUT *)
                StartLoop (1);

        |  1 => (* #{_UPDATE *)
                StartLoop (2);

        |  2 => (* #{_FEEDBACK *)
                StartLoop (3);

        |  3 => (* #{ -- any event or arg *)
                IF NOT in_event_loop THEN
                  StartLoop (0);
                ELSIF NOT in_arg_loop THEN
                  IF cur_event.args = NIL THEN
                    cur := SkipLoop (template, restart);
                  ELSE
                    in_arg_loop := TRUE;
                    arg_restart := restart;
                    cur_arg := cur_event.args;
                    cur := restart;
                  END;
                ELSE
                  BadTemplate ();
                END;

        |  4 => (* #} *)
                IF (in_arg_loop) THEN
                  cur_arg := cur_arg.next;
                  IF (cur_arg = NIL) THEN
                    in_arg_loop := FALSE;
                    cur := restart;
                  ELSE
                    cur := arg_restart;
                  END;
                ELSIF (in_event_loop) THEN
                  cur_event := NextEvent (cur_event.next, event_kind);
                  IF (cur_event = NIL) THEN
                    in_event_loop := FALSE;
                    event_kind    := 0;
                    cur := restart;
                  ELSE
                    cur := event_restart;
                  END;
                ELSE
                  BadTemplate ();
                END;

        |  5 => (* #| *)
                IF (cur_arg = NIL) OR (cur_arg.next = NIL) THEN
                  cur := SkipLoop (template, restart);
                  in_arg_loop := FALSE;
                ELSE
                  cur := restart;
                END;

        |  6 => (* (_ALGNAME_) *)
                OutT (buf, algorithm);
                cur := restart;

        |  7 => (* (_VIEWNAME_) *)
                OutT (buf, view);
                cur := restart;

        |  8 => (* (_IMPORTS_) *)
                GenImports (evt.imports, buf);
                cur := restart;

        |  9 => (* (_EVENT_) *)
                IF (cur_event = NIL) THEN BadTemplate () END;
                OutT (buf, cur_event.name);
                cur := restart;

        | 10 => (* (_EVENTSTYLE_) *)
                IF (cur_event = NIL) THEN BadTemplate () END;
                OutT (buf, EventFile.EventKindName [cur_event.kind]);
                cur := restart;

        | 11 => (* (_ARGSTR_) *)
                GenArgs (buf, cur_event.args);
                cur := restart;

        | 12 => (* (_NONULL_ARGSTR_) *)
                IF (cur_event.args = NIL) THEN
                  OutT (buf, "dummy : INTEGER");
                ELSE
                  GenArgs (buf, cur_event.args);
                END;
                cur := restart;

        | 13 => (* (_SEMI_ARGSTR_) *)
                IF (cur_event.args # NIL) THEN
                  OutT (buf, "; ");
                  GenArgs (buf, cur_event.args);
                END;
                cur := restart;

        | 14 => (* (_ARGSTR_SEMI_) *)
                IF (cur_event.args # NIL) THEN
                  GenArgs (buf, cur_event.args);
                  OutT (buf, "; ");
                END;
                cur := restart;

        | 15 => (* (_ARGTYPES_) *)
                IF (cur_event.args # NIL) THEN
                  GenArgTypes (buf, cur_event.args, ", ");
                END;
                cur := restart;

        | 16 => (* (_COMMA_ARGTYPES_) *)
                IF (cur_event.args # NIL) THEN
                  OutT (buf, ", ");
                  GenArgTypes (buf, cur_event.args, ", ");
                END;
                cur := restart;

        | 17 => (* (_SPACED_ARGTYPES_) *)
                IF (cur_event.args # NIL) THEN
                  GenArgTypes (buf, cur_event.args, " ");
                END;
                cur := restart;

        | 18 => (* (_ARGNAMES_) *)
                IF (cur_event.args # NIL) THEN
                  GenArgNames (buf, cur_event.args);
                END;
                cur := restart;

        | 19 => (* (_COMMA_ARGNAMES_) *)
                IF (cur_event.args # NIL) THEN
                  OutT (buf, ", ");
                  GenArgNames (buf, cur_event.args);
                END;
                cur := restart;

        | 20 => (* (_ARGNAME_) *)
                IF (cur_arg = NIL) THEN BadTemplate () END;
                OutT (buf, cur_arg.name);
                cur := restart;

        | 21 => (* (_ARGTYPE_) *)
                IF (cur_arg = NIL) THEN BadTemplate () END;
                OutT (buf, cur_arg.type);
                cur := restart;

        | 22 => (* (_ARGFMT_) *)
                IF (cur_arg = NIL) THEN BadTemplate () END;
                OutT (buf, cur_arg.printer);
                cur := restart;

        | 23 => (* (_EVENTPRIO_) *)
                IF (cur_event = NIL) THEN BadTemplate () END;
                OutT (buf, cur_event.priority);
                cur := restart;

        | 24 => (* (_ALGDATA_) *)
                IF (evt.alg_data # NIL) THEN
                  OutT (buf, evt.alg_data);
                END;
                cur := restart;

        | 25 => (* (_ARGMODE_) *)
                IF (cur_arg = NIL) THEN BadTemplate () END;
                OutT (buf, ModeName [cur_arg.mode]);
                cur := restart;

        ELSE (* unrecognized '#' *)
                Out (buf, template[cur]);  INC (cur);
        END;
      END;
    END;
    Flush (buf);
  END Generate;

PROCEDURE GenImports (im: EventFile.Import;  VAR buf: OutBuf)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WHILE (im # NIL) DO
      Out  (buf, '$');
      OutT (buf, im.interface);
      Out  (buf, '\n');
      im := im.next;
    END;
    Out (buf, '!');
  END GenImports;

PROCEDURE GenArgs (VAR buf: OutBuf;  a: EventFile.EventArg)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR in_comma_list: BOOLEAN := FALSE;
  BEGIN
    OutT (buf, " ");
    WHILE (a # NIL) DO
      IF NOT in_comma_list THEN OutT (buf, ModeName [a.mode]); END;
      OutT (buf, a.name);
      IF (a.next # NIL)
        AND (a.mode = a.next.mode)
        AND (a.type = a.next.type)
        AND (a.type = a.next.type)
        AND (a.printer = a.next.printer) THEN
        OutT (buf, ", ");
        in_comma_list := TRUE;
      ELSE
        OutT (buf, ": ");
        OutT (buf, a.type);
        IF (a.next # NIL) THEN OutT (buf, "; "); END;
        in_comma_list := FALSE;
      END;
      a := a.next;
    END;
  END GenArgs;

PROCEDURE GenArgTypes (VAR buf: OutBuf;  a: EventFile.EventArg;  sep: TEXT)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    WHILE (a # NIL) DO
      OutT (buf, a.type);
      a := a.next;
      IF (a # NIL) THEN OutT (buf, sep); END;
    END;
  END GenArgTypes;

PROCEDURE GenArgNames (VAR buf: OutBuf;  a: EventFile.EventArg)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    WHILE (a # NIL) DO
      OutT (buf, a.name);
      a := a.next;
      IF (a # NIL) THEN OutT (buf, ", "); END;
    END;
  END GenArgNames;

PROCEDURE NextEvent (e: EventFile.Event;  kind: INTEGER): EventFile.Event =
  TYPE EK = EventFile.EventKind;
  CONST XX = ARRAY [1..3] OF EK { EK.Output, EK.Update, EK.Feedback };
  BEGIN
    IF (kind = 0) THEN RETURN e END;
    WHILE (e # NIL) AND (e.kind # XX[kind]) DO e := e.next; END;
    RETURN e;
  END NextEvent;

PROCEDURE SkipLoop (tmp: TEXT;  cur: INTEGER): INTEGER =
  VAR len := Text.Length (tmp);  depth := 1;  restart: INTEGER;
  BEGIN
    WHILE (cur < len) DO
      IF (tmp[cur] # '#') THEN
        INC (cur);
      ELSE
        CASE Classify (tmp, cur, restart) OF

        |  0..3 => (* #{_OUTPUT, #{_UPDATE, #{_FEEDBACK, #{  *)
                INC (depth);
                cur := restart;

        |  4 => (* #} *)
                DEC (depth);
                IF (depth = 0) THEN  RETURN restart;  END;
                cur := restart;
        ELSE
          INC (cur);
        END;
      END;
    END;
    RETURN cur;
  END SkipLoop;

PROCEDURE Classify (tmp: TEXT;  cur: INTEGER;
                    VAR(*OUT*)restart: INTEGER): INTEGER =
  VAR
    tlen := Text.Length (tmp);
    nlen, tx, nx: INTEGER;
    full : BOOLEAN;
    nm   : TEXT;
  BEGIN
    FOR i := FIRST (Markers) TO LAST (Markers) DO
      nm   := Markers[i].name;
      nlen := Markers[i].len;
      full := Markers[i].full_line;
      nx   := 0;
      tx   := cur;
      IF (NOT full) OR (cur = 0) OR (tmp[cur-1] = '\n') THEN
        WHILE (nx < nlen) AND (tx < tlen) AND (tmp[tx] = nm[nx]) DO
          INC (tx); INC (nx);
        END;
        IF (nx >= nlen) THEN
          (* we found a match! *)
          IF full THEN
            (* scan to the beginning of the next line *)
            WHILE (tx < tlen) AND (tmp[tx] # '\n') DO INC (tx); END;
            INC (tx);
          END;
          restart := tx;
          RETURN i;
        END;
      END;
    END;
    (* failed... *)
    restart := cur + 1;
    RETURN -1;
  END Classify;

PROCEDURE OutT (VAR b: OutBuf;  t: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    FOR i := 0 TO LAST (t^)-1 DO Out (b, t[i]) END;
  END OutT;

PROCEDURE Out (VAR b: OutBuf;  c: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (b.imports = NIL) THEN
      (* we're in normal "pass thru" mode *)
      IF (b.newline) AND (c = '$') THEN
        Flush (b);
        b.imports := NEW (TextTextTbl.Default).init ();
      ELSE
        IF (b.len >= NUMBER (b.buf)) THEN Flush (b); END;
        b.buf [b.len] := c;
        INC (b.len);
        b.newline := (c = '\n');
      END;
    ELSE
      (* we're collecting imports *)
      IF (c = '\n') THEN
        IF (b.len > 0) THEN
          EVAL b.imports.put (Text.FromChars(SUBARRAY(b.buf, 0, b.len)), NIL);
          b.len := 0;
        END;
      ELSIF (c = '$') THEN
        (* skip *)
      ELSIF (c = '!') THEN
        EmitImports (b);
      ELSIF (c = '\r') THEN
        (* skip *)
      ELSIF (c = ' ') THEN
        (* skip *)
      ELSIF (b.len < NUMBER (b.buf)) THEN
        b.buf[b.len] := c;
        INC (b.len);
      ELSE
        Process.Crash ("m3zume: imported interface name too long!");
      END;
    END;
  END Out;

PROCEDURE EmitImports (VAR buf: OutBuf)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    imports := buf.imports;
    iter    := imports.iterate ();
    indent  := FIRST(INTEGER);
    intf    : TEXT;
    txt     : TEXT;
    len     : INTEGER;
  BEGIN
    (* revert to "pass thru" mode *)
    buf.imports := NIL;  
    buf.len := 0;

    WHILE iter.next (intf, txt) DO
      len := Text.Length (intf);
      IF (indent + len >= 65) THEN  OutT (buf, ";\n"); indent := 0;  END;
      IF (indent <= 0)
        THEN  OutT (buf, "<*NOWARN*> IMPORT ");  indent := 18;
        ELSE  OutT (buf, ", ");                  INC (indent, 2);
      END;
      OutT (buf, intf);
      INC (indent, len);
    END;
    IF (indent > 0) THEN OutT (buf, ";"); END;
  END EmitImports;

PROCEDURE Flush (VAR b: OutBuf)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (b.len # 0) THEN
      Wr.PutString (b.wr, SUBARRAY (b.buf, 0, b.len));
    END;
    b.len := 0;
  END Flush;

PROCEDURE BadTemplate () =
  BEGIN
    Process.Crash ("m3zume: bad template file");
  END BadTemplate;

BEGIN
END Template.
