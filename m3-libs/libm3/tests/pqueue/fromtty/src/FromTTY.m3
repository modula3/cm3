(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Sep 23 10:37:42 PDT 1994 by heydon                   *)

MODULE FromTTY EXPORTS Main;

(* SYNTAX
     FromTTY [ filename ]

   DESCRIPTION

   This module provides an interactive way to test the generic "PQueue.Default"
   implementation from a line-oriented tty. If a "filename" is specified,
   commands are read from the named file. Otherwise, they are read from
   standard input.

   The available commands are:

|    i <text>  -- inserts <text> and returns an index of its element
|    d <elt>   -- deletes the element with index <elt>
|    sz        -- returns the current size of the queue
|    min       -- returns the priority (text) of the minimum queue element
|    dmin      -- deletes the min queue element, returning its priority
|    c <elt> <text> -- changes the priority of <elt> to <text>
|    showelts  -- shows all queue elements
|    showheap  -- show heap representation

*)

IMPORT TextPQ, TextPQRep;
IMPORT IO, SortedIntRefTbl, Lex, TextRd, Rd, FileRd, Text, Fmt, Convert;
IMPORT Thread, FloatMode, Params, Process, OSError, Stdio;

CONST
  Cmds = ARRAY OF TEXT { "init", "fromelts", "i", "d", "sz", "min", "dmin",
    "c", "showelts", "showheap" };

VAR
  pq := NEW(TextPQ.Default).init(sizeHint := 0);
  tbl := NEW(SortedIntRefTbl.Default).init();
  ln: TEXT; lnRd: Rd.T;
  next: CARDINAL := 1;			 (* index of next element *)

TYPE
  Cmd = { Init, FromElts, Insert, Delete, Size, Min, DeleteMin, Change,
    ShowElts, ShowHeap, Unknown };
  Elt = TextPQ.Elt BRANDED OBJECT deleted := FALSE END;

PROCEDURE ParseCmd(cmd: TEXT): Cmd =
  BEGIN
    FOR i := 0 TO LAST(Cmds) DO
      IF Text.Equal(cmd, Cmds[i]) THEN RETURN VAL(i, Cmd) END
    END;
    RETURN Cmd.Unknown
  END ParseCmd;

PROCEDURE ShowHelp() =
  BEGIN
    IO.Put("Available commands are:\n\n");
    IO.Put("  " & Cmds[0] & " [ <size> ]\n");
    IO.Put("  " & Cmds[1] & " [ <text> <text> ... ]\n");
    IO.Put("  " & Cmds[2] & " <text>\n");
    IO.Put("  " & Cmds[3] & " <elt>\n");
    IO.Put("  " & Cmds[4] & "\n");
    IO.Put("  " & Cmds[5] & "\n");
    IO.Put("  " & Cmds[6] & "\n");
    IO.Put("  " & Cmds[7] & " <elt> <text>\n");
    IO.Put("  " & Cmds[8] & "\n");
    IO.Put("  " & Cmds[9] & "\n\n");
    IO.Put("By default, the initial priority queue is empty with\n");
    IO.Put("size zero; you can use \"init\" and \"fromElts\" to\n");
    IO.Put("initialize it differently.\n");
  END ShowHelp;

<* FATAL IO.Error, Rd.Failure, Thread.Alerted, FloatMode.Trap *>

VAR
  fromFile := FALSE;
  elts := NEW(TextPQRep.EltsArray, 10); (* used in "fromelts" command *)

BEGIN
  (* parse command-line *)
  IF Params.Count > 2 THEN
    IO.Put("Syntax: " & Params.Get(0) & " [filename]\n");
    Process.Exit(n := 1)
  ELSIF Params.Count = 2 THEN
    VAR rd: Rd.T; nm := Params.Get(1); BEGIN
      TRY rd := FileRd.Open(nm) EXCEPT
        OSError.E =>
          IO.Put("Error: unable to open \"" & nm & "\"\n");
          Process.Exit(n := 2)
      END;
      Stdio.stdin := rd;
      fromFile := TRUE
    END
  END;

  (* read commands *)
  LOOP
    IO.Put("> ");
    IF IO.EOF() THEN
      IF fromFile THEN IO.Put("^D") END;
      EXIT
    END;
    ln := IO.GetLine();
    IF fromFile THEN IO.Put(ln); IO.Put("\n") END;
    lnRd := TextRd.New(ln);
    TRY
      CASE ParseCmd(Lex.Scan(lnRd)) OF
        Cmd.Init =>
          VAR sz: INTEGER; BEGIN
            Lex.Skip(lnRd);
            sz := Lex.Int(lnRd);
            EVAL pq.init(sizeHint := sz);
            IO.Put("LAST(pq.heap^) = " & Fmt.Int(LAST(pq.heap^)));
            IO.Put("; pq.sz = " & Fmt.Int(pq.sz) & "\n")
          END
      | Cmd.FromElts =>
          VAR txt: TEXT; elt: TextPQ.Elt; i := 0; BEGIN
            LOOP
              Lex.Skip(lnRd);
              IF Rd.EOF(lnRd) THEN EXIT END;
              txt := "\"" & Lex.Scan(lnRd) & "\"";
              elt := NEW(Elt, priority := txt);
              IF i > LAST(elts^) THEN
                VAR new := NEW(TextPQRep.EltsArray, 2 * NUMBER(elts^)); BEGIN
                  SUBARRAY(new^, 0, NUMBER(elts^)) := elts^;
                  elts := new
                END
              END;
              elts[i] := elt; INC(i);
              EVAL tbl.put(next, elt);
              IO.Put(txt & " => " & Fmt.Int(next) & "\n");
              INC(next)
            END;
            EVAL pq.fromArray(SUBARRAY(elts^, 0, i))
          END
      | Cmd.Insert =>
          VAR txt: TEXT; elt: TextPQ.Elt; BEGIN
            Lex.Skip(lnRd);
            txt := "\"" & IO.GetLine(lnRd) & "\"";
            elt := NEW(Elt, priority := txt);
            pq.insert(elt);
            EVAL tbl.put(next, elt);
            IO.Put(txt & " => " & Fmt.Int(next) & "\n");
            INC(next)
          END
      | Cmd.Delete =>
          VAR num: INTEGER; ref: REFANY; elt: Elt; BEGIN
            Lex.Skip(lnRd);
            num := Lex.Int(lnRd);
            IF NOT tbl.get(num, ref) THEN RAISE TextPQ.NotInQueue END;
            elt := ref;
            pq.delete(elt);
            elt.deleted := TRUE;
            IO.Put(elt.priority & " => deleted\n")
          END
      | Cmd.Size =>
          IO.Put(Fmt.Int(pq.size()) & "\n")
      | Cmd.Min =>
          IO.Put(pq.min().priority & "\n")
      | Cmd.DeleteMin =>
          VAR elt: Elt := pq.deleteMin(); BEGIN
            elt.deleted := TRUE;
            IO.Put(elt.priority & "\n")
          END
      | Cmd.Change =>
          VAR num: INTEGER; txt: TEXT; ref: REFANY; elt: Elt; BEGIN
            Lex.Skip(lnRd);
            num := Lex.Int(lnRd);
            IF NOT tbl.get(num, ref) THEN RAISE TextPQ.NotInQueue END;
            elt := ref;
            Lex.Skip(lnRd);
            txt := "\"" & IO.GetLine(lnRd) & "\"";
            pq.change(elt, txt);
            IO.Put(elt.priority & " => " & Fmt.Int(num) & "\n")
          END
      | Cmd.ShowElts =>
          VAR it := tbl.iterateOrdered(); num: INTEGER; ref: REFANY; BEGIN
            WHILE it.next(num, ref) DO
              VAR elt: Elt := ref; BEGIN
                IO.Put(Fmt.Pad(Fmt.Int(num), 3) & " ");
                IF elt.deleted
                  THEN IO.Put("<deleted>")
                  ELSE IO.Put(elt.priority)
                END;
                IO.Put("\n")
              END
            END
          END
      | Cmd.ShowHeap =>
          IO.Put("LAST(pq.heap^) = " & Fmt.Int(LAST(pq.heap^)));
          IO.Put("; pq.sz = " & Fmt.Int(pq.sz) & "\n");
          IO.Put("pq.heap = Elts{");
          VAR i, nilCnt: CARDINAL := 0; BEGIN
            WHILE i <= LAST(pq.heap^) DO
              WITH elt = pq.heap[i] DO
              	IF elt = NIL THEN
              	  REPEAT
              	    INC(nilCnt); INC(i)
              	  UNTIL i > LAST(pq.heap^) OR pq.heap[i] # NIL;
              	  IO.Put("NIL");
              	  IF nilCnt > 1 THEN
              	    IO.Put(" (X" & Fmt.Int(nilCnt) & ")")
              	  END;
              	  nilCnt := 0
              	ELSE
              	  IO.Put(elt.priority);
              	  INC(i)
              	END
              END;
              IF i <= LAST(pq.heap^) THEN IO.Put(", ") END
            END
          END;
          IO.Put("}\n")
      | Cmd.Unknown => ShowHelp()
      END
    EXCEPT
      TextPQ.Empty => IO.Put("Queue is empty\n")
    | TextPQ.NotInQueue => IO.Put("Not a queue element\n")   
    | Convert.Failed => IO.Put("Bad element spec\n")
    | Lex.Error => (* SKIP *)
    END
  END;
  IO.Put("\n")
END FromTTY.
