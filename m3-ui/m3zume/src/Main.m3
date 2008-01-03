(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Mon Feb  6 17:06:09 PST 1995 by kalsow  *)

MODULE Main;

IMPORT Rd, Wr, OSError, Thread, FileRd, FileWr;
IMPORT Text, TextList, Params, Bundle, Pathname, Process, IO;
IMPORT EventFile, Template, ZoomBundle;

TYPE
  Mode = { Modula_3, Obliq, Obliq3D, Juno, Three_D };

CONST
  M3_pieces = ARRAY OF TEXT {
    "AlgClass.i3", NIL,
    "AlgClass.m3", NIL,
    "ViewClass.i3", NIL,
    "ViewClass.m3", NIL,
    "IE.i3", NIL,
    "IE.m3", NIL,
    "DataView.i3", NIL,
    "DataView.m3", NIL,
    "TranscriptView.i3",  NIL,
    "TranscriptView.m3",  NIL,
    "TranscriptView.fv",  NIL,
    "EventData.fv",  NIL
  };

CONST
  M3_3D_pieces = ARRAY OF TEXT {
    "AlgClass.i3", NIL,
    "AlgClass.m3", NIL,
    "ViewClass.i3", NIL,
    "ViewClass.m3", NIL,
    "IE3D.i3", "IE.i3",
    "IE3D.m3", "IE.m3",
    "DataView.i3", NIL,
    "DataView.m3", NIL,
    "TranscriptView.i3", NIL,
    "TranscriptView.m3", NIL,
    "TranscriptView.fv", NIL,
    "EventData.fv", NIL,
    "ViewClass3D.i3", "3DViewClass.i3",
    "ViewClass3D.m3", "3DViewClass.m3"
  };

CONST
  Obliq_pieces = ARRAY OF TEXT { "ObliqView.i3", NIL, "ObliqView.m3", NIL };
  Obliq_3D_pieces = ARRAY OF TEXT { "Obliq3DView.i3",NIL,"Obliq3DView.m3",NIL};
  Juno_pieces = ARRAY OF TEXT { "JunoView.i3", NIL, "JunoView.m3", NIL };
  
VAR
  mode         : Mode := Mode.Modula_3;
  template_dir : TEXT := NIL;
  inputs       : TextList.T := NIL;
  work_list    : TextList.T := NIL;
  view_name    : TEXT := NIL;

PROCEDURE DoIt () =
  BEGIN
    ParseOptions ();
    IF TextList.Length (inputs) <= 0 THEN
      Die ("no input files specified");
    END;
    WHILE (inputs # NIL) DO
      ProcessFile (inputs.head);
      inputs := inputs.tail;
    END;
  END DoIt;

PROCEDURE ParseOptions () =
  VAR i := 1;  n := Params.Count;  arg: TEXT;
  BEGIN
    SetWorkList (M3_pieces, NIL);

    WHILE (i < n) DO
      arg := Params.Get (i);
      IF Text.Equal (arg, "-templates") THEN
        INC (i);
        IF (i >= n) THEN Die ("missing directory after \"-templates\""); END;
        template_dir := Params.Get (i);
      ELSIF Text.Equal (arg, "-Obliq") THEN
        mode := Mode.Obliq;
        INC (i);
        IF (i >= n) THEN Die ("missing file after \"-Obliq\""); END;
        arg := Params.Get (i);
        SetWorkList (Obliq_pieces, Pathname.LastBase (arg));
      ELSIF Text.Equal (arg, "-Obliq3D") THEN
        mode := Mode.Obliq3D;
        INC (i);
        IF (i >= n) THEN Die ("missing file after \"-Obliq3D\""); END;
        arg := Params.Get (i);
        SetWorkList (Obliq_3D_pieces, Pathname.LastBase (arg));
      ELSIF Text.Equal (arg, "-Juno") THEN
        mode := Mode.Juno;
        INC (i);
        IF (i >= n) THEN Die ("missing file after \"-Juno\""); END;
        arg := Params.Get (i);
        SetWorkList (Juno_pieces, Pathname.LastBase (arg));
      ELSIF Text.Equal (arg, "-3D") THEN
        mode := Mode.Three_D;
        SetWorkList (M3_3D_pieces, NIL);
      ELSE
        inputs := TextList.Cons (arg, inputs);
      END;
      INC (i);
    END;
    inputs := TextList.ReverseD (inputs);
  END ParseOptions;

PROCEDURE SetWorkList (READONLY x: ARRAY OF TEXT;   aux: TEXT) =
  VAR input, output: TEXT;
  BEGIN
    work_list := NIL;
    view_name := aux;
    FOR i := FIRST (x) TO LAST (x) BY 2 DO
      input := x[i];
      output := x[i+1];
      work_list := TextList.Cons (input, work_list);
      IF (output = NIL) THEN output := input; END;
      IF (aux # NIL) THEN output := aux & output; END;
      work_list := TextList.Cons (output, work_list);
    END;
    work_list := TextList.ReverseD (work_list);    
  END SetWorkList;

PROCEDURE ProcessFile (fname: TEXT) =
  VAR
    alg, ext, msg: TEXT;
    input, output: TEXT;
    rd: Rd.T;
    wr: Wr.T;
    event_file: EventFile.T;
    x: TextList.T;
  BEGIN
    ext := Pathname.LastExt (fname);
    IF Text.Equal (ext, "evt") THEN
      (* ok *)
    ELSIF Text.Equal (ext, "") THEN
      fname := Pathname.Join (NIL, fname, "evt");
    ELSE
      Die ("input file has unrecognized extension: " & fname);
    END;

    alg := Pathname.LastBase (fname);
    rd := OpenInput (fname);
    msg := EventFile.Parse (rd, event_file);
    IF (msg # NIL) THEN Die ("file " & fname & ", " & msg); END;
    CloseInput (rd, fname);

    x := work_list;
    WHILE (x # NIL) DO
      input  := x.head;        x := x.tail;
      output := alg & x.head;  x := x.tail;
      wr := OpenOutput (output);
      TRY
        Template.Generate (event_file, alg, view_name,
                           FindTemplate (input), wr);
      EXCEPT
      | Wr.Failure     => Die ("unable to write file " & output);
      | Thread.Alerted => Die ("interrupted while writing " & output);
      END;
      CloseOutput (wr, output);
    END;
  END ProcessFile;

PROCEDURE FindTemplate (nm: TEXT): TEXT =
  VAR res, fname: TEXT;  rd: Rd.T;
  BEGIN
    IF (template_dir = NIL) THEN
      res := Bundle.Get (ZoomBundle.Get (), nm);
      IF (res = NIL) THEN Die ("unable to find template " & nm) END;
    ELSE
      fname := Pathname.Join (template_dir, nm, NIL);
      rd := OpenInput (fname);
      TRY
        res := Rd.GetText (rd, LAST (CARDINAL));
      EXCEPT
      | Rd.Failure     => Die ("unable to read " & fname);
      | Thread.Alerted => Die ("interrupted while reading " & fname);
      END;
      CloseInput (rd, fname);
    END;
    RETURN res;
  END FindTemplate;

(*-------------------------------------------------------- misc. utilites ---*)

PROCEDURE OpenInput (fn: TEXT): Rd.T =
  BEGIN
    TRY
      RETURN FileRd.Open (fn);
    EXCEPT OSError.E =>
      Die ("unable to open input file: " & fn);
      RETURN NIL;
    END;
  END OpenInput;

PROCEDURE CloseInput (rd: Rd.T;  fn: TEXT) =
  BEGIN
    TRY
      Rd.Close (rd);
    EXCEPT
    | Rd.Failure     => Die ("unable to close input file: " & fn);
    | Thread.Alerted => Die ("interrupted while closing input: " & fn)
    END;
  END CloseInput;

PROCEDURE OpenOutput (fn: TEXT): Wr.T =
  BEGIN
    TRY
      RETURN FileWr.Open (fn);
    EXCEPT OSError.E =>
      Die ("unable to open output file: " & fn);
      RETURN NIL;
    END;
  END OpenOutput;

PROCEDURE CloseOutput (wr: Wr.T;  fn: TEXT) =
  BEGIN
    TRY
      Wr.Close (wr);
    EXCEPT
    | Wr.Failure     => Die ("unable to close output file: " & fn);
    | Thread.Alerted => Die ("interrupted while closing output: " & fn)
    END;
  END CloseOutput;

PROCEDURE Die (msg: TEXT) =
  BEGIN
    IO.Put ("m3zoom: " & msg & "\n");
    Process.Exit (1);
  END Die;

BEGIN
  DoIt ();
END Main.
