MODULE Main;

IMPORT IO, Params, Process, Stdio, Text, Rd, Wr, OSError;
IMPORT M3CG, M3CG_Rd, M3CG_Wr, M3CG_BinRd, M3CG_BinWr, MxConfig, Target, M3C;
IMPORT FileRd, FileWr, Word;
FROM Thread IMPORT Alerted;

PROCEDURE TextFindEqual (a: TEXT; READONLY b: ARRAY OF TEXT): INTEGER =
BEGIN
  FOR i := FIRST (b) TO LAST (b) DO
    IF Text.Equal (a, b[i]) THEN
      RETURN i;
    END;
  END;
  RETURN -1;
END TextFindEqual;

PROCEDURE TextFindPrefix (a: TEXT; READONLY b: ARRAY OF TEXT): INTEGER =
VAR a_length := Text.Length (a);
BEGIN
  FOR i := FIRST (b) TO LAST (b) DO
    IF a_length >= Text.Length (b[i]) AND
        Text.Equal (Text.Sub (a, 0, Text.Length (b[i])), b[i]) THEN
      RETURN i;
    END;
  END;
  RETURN -1;
END TextFindPrefix;

PROCEDURE RdClose_DoNothing (a: Rd.T) RAISES {Rd.Failure, Alerted} <*NOWARN*>=
BEGIN END RdClose_DoNothing;

PROCEDURE WrClose_DoNothing (a: Wr.T) RAISES {Wr.Failure, Alerted} <*NOWARN*>=
BEGIN
END WrClose_DoNothing;

PROCEDURE DoIt () RAISES {Rd.Failure, Wr.Failure, Alerted, OSError.E} =
  TYPE Inhale_t = PROCEDURE (rd: Rd.T;  cg: M3CG.T);
       OutNew_t = PROCEDURE (wr: Wr.T): M3CG.T;
  CONST Inhale = ARRAY OF Inhale_t {M3CG_BinRd.Inhale, M3CG_Rd.Inhale};
  CONST OutNew = ARRAY OF OutNew_t {M3CG_BinWr.New, M3CG_Wr.New, M3C.New};
  CONST inout_text = ARRAY OF TEXT {"-in-", "-out-"};
  CONST types_text = ARRAY OF TEXT {"binary", "ascii", "c"};
  VAR arg : TEXT;
      inited := FALSE;
      rd_in := Stdio.stdin;
      wr_out := Stdio.stdout;
      arg_inout := 0;
      arg_type := 0;
      arg_length := 0;
      ch := '\000';
      inhale: Inhale_t := NIL;
      out_new: OutNew_t := NIL;
      rd_close := RdClose_DoNothing;
      wr_close := WrClose_DoNothing;
      any := FALSE;
  BEGIN
    TRY
      IF Params.Count < 2 THEN
        Usage ();
      END;
      IF Params.Count = 2 THEN
        arg := Params.Get (1);
        arg_type := TextFindEqual (arg, ARRAY OF TEXT { "-binary", "-ascii" });
        IF arg_type < 0 THEN
          Usage ();
          RETURN;
        END;
        Init ();
        Inhale[arg_type] (rd_in, OutNew[Word.Xor(arg_type, 1)] (wr_out));
        RETURN;
      END;
      FOR i := 1 TO Params.Count - 1 DO
        arg := Params.Get (i);
        arg_inout := TextFindPrefix (arg, inout_text);      
        IF arg_inout < 0 THEN
          Usage ();
        END;
        arg := Text.Sub (arg, Text.Length (inout_text[arg_inout]));
        arg_type := TextFindPrefix (arg, types_text);
        IF arg_type < 0 THEN
          Usage ();
        END;
        IF arg_inout = 0 THEN
          inhale := Inhale[arg_type];
        ELSE
          out_new := OutNew[arg_type];
        END;
        arg := Text.Sub (arg, Text.Length (types_text[arg_type]));
        arg_length := Text.Length (arg);
        IF arg_length > 0 THEN
          ch := Text.GetChar (arg, 0);
          IF NOT (ch = ':' OR ch = '=') THEN
            Usage ();
          END;
          arg := Text.Sub (arg, 1);
          IF arg_inout = 0 THEN
            rd_close (rd_in);
            rd_in := FileRd.Open (arg);
            rd_close := Rd.Close;
          ELSE
            wr_close (wr_out);
            wr_out := FileWr.Open (arg);
            wr_close := Wr.Close;
          END;
        END;
        IF inhale # NIL AND out_new # NIL THEN
          IF NOT inited THEN
            Init ();
            inited := TRUE;
          END;
          inhale (rd_in, out_new (wr_out));
          any := TRUE;
          inhale := NIL;
          out_new := NIL;
          wr_close (wr_out);
          wr_close := WrClose_DoNothing;
          wr_out := NIL;
          rd_close (rd_in);
          rd_close := RdClose_DoNothing;
          rd_in := NIL;
        END;
      END;
      IF NOT any THEN
        Usage ();
      END;
    FINALLY
      wr_close (wr_out);
      rd_close (rd_in);
    END;
  END DoIt;

PROCEDURE Usage () =
VAR arg0 := Params.Get(0);
  BEGIN
    IO.Put ("usage: " & arg0 & " -ascii  < in.asc > out.bin" & Wr.EOL);
    IO.Put ("       " & arg0 & " -binary < in.bin > out.asc" & Wr.EOL);
    IO.Put ("       " & arg0 & " -[in|out]-[binary|ascii|c] < in > out" & Wr.EOL);
    IO.Put ("       " & arg0 & " -[in|out]-[binary|ascii|c][:=file] < in > out" & Wr.EOL);
    IO.Put ("       example: " & arg0 & " -in-binary -out-c < in.bin > out.c" & Wr.EOL);
    IO.Put ("       example: " & arg0 & " -in-binary=in.bin -out-c:out.c" & Wr.EOL);
    Process.Exit (1);
  END Usage;

PROCEDURE Init () =
  VAR machine: TEXT;
  BEGIN
    machine := MxConfig.Get ("TARGET");
    IF machine = NIL THEN
      IO.Put ("unable to find TARGET definition in configuration file" & Wr.EOL);
      Process.Exit (1);
    ELSIF NOT Target.Init (machine) THEN
      IO.Put ("unable to initialize Target: " & machine & Wr.EOL);
      Process.Exit (1);
    END;
  END Init;

BEGIN
  DoIt (); <*NOWARN*>
END Main.
