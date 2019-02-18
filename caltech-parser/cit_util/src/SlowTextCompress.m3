(* $Id$ *)

MODULE SlowTextCompress;
IMPORT Rd, Wr, UnixFilter, ProcUtils;
IMPORT TextWr, Debug, Thread;
FROM Text IMPORT Length;
IMPORT Fmt;

<* FATAL Thread.Alerted *>

CONST Command = ARRAY Mode OF TEXT { "bzip2 -cz", "bzip2 -cd" };
(*CONST Command = ARRAY Mode OF TEXT { "./script -cz", "./script -cd" };(* test code *) *)

PROCEDURE RR(mode : Mode; source : Rd.T) : Rd.T =
  BEGIN
    RETURN UnixFilter.RR(Command[mode],source)
  END RR;

PROCEDURE WW(mode : Mode; target : Wr.T) : Wr.T =
  BEGIN
    RETURN UnixFilter.WW(Command[mode],target)
  END WW;

PROCEDURE RW(mode : Mode; source : Rd.T; target : Wr.T) = 
  BEGIN
    UnixFilter.RW(Command[mode],source, target)
  END RW;

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT  RAISES { ProcUtils.ErrorExit } =
  VAR
    wrIn : Wr.T;
    wr := NEW(TextWr.T).init();
    writer := ProcUtils.WriteHere(wr);
    c : ProcUtils.Completion;
  BEGIN
    Debug.Out("SlowTextCompress.Text: running \"" & Command[mode] & "\"");
    c := ProcUtils.RunText(Command[mode], stdout := writer, 
                           stdin := ProcUtils.GimmeWr(wrIn));

    Wr.PutText(wrIn, in);
    Wr.Close(wrIn);

    c.wait();
    WITH out = TextWr.ToText(wr) DO
      IF Debug.GetLevel() >= Debug.DefaultLevel THEN
        Debug.Out("SlowTextCompress.Text: in " & Fmt.Int(Length(in)) & 
                  " bytes, out " & Fmt.Int(Length(out)) & " bytes")
      END;
      RETURN out
    END
  END Text;

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T) RAISES { ProcUtils.ErrorExit } =
  VAR
    writer := ProcUtils.WriteHere(out);
    wrIn : Wr.T;
    c : ProcUtils.Completion;
    
    errWr := NEW(TextWr.T).init();
  BEGIN
    Debug.Out("SlowTextCompress.RdWr starting");
    c := ProcUtils.RunText(Command[mode], 
                           stdout := writer, 
                           stdin := ProcUtils.GimmeWr(wrIn),
                           stderr := ProcUtils.WriteHere(errWr));


    TRY
      LOOP
        WITH c = Rd.GetChar(in) DO Wr.PutChar(wrIn,c) END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(in); Wr.Close(wrIn)
    END;
    
    TRY
      c.wait();
    EXCEPT 
      ProcUtils.ErrorExit(e) =>
        Debug.Out("SlowTextCompress.errWr: " & TextWr.ToText(errWr));
        RAISE ProcUtils.ErrorExit(e)
    END;


    Debug.Out("SlowTextCompress.RdWr done");
    Wr.Close(out)
  END RdWr;

BEGIN END SlowTextCompress.
