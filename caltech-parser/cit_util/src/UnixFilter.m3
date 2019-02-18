(* $Id$ *)

MODULE UnixFilter;
IMPORT Rd, Wr;
IMPORT Pathname, ProcUtils;

PROCEDURE RR(cmd : TEXT; source : Rd.T; wd0 : Pathname.T) : Rd.T =
  VAR
    rd : Rd.T;
    writer := ProcUtils.GimmeRd(rd);
    reader := ProcUtils.ReadHere(source);
  BEGIN
    EVAL ProcUtils.RunText(cmd, stdout := writer, stdin := reader, wd0 := wd0);
    RETURN rd
  END RR;

PROCEDURE WW(cmd : TEXT; target : Wr.T; wd0 : Pathname.T) : Wr.T =
  VAR
    wr : Wr.T;
    reader := ProcUtils.GimmeWr(wr);
    writer := ProcUtils.WriteHere(target);
  BEGIN
    EVAL ProcUtils.RunText(cmd, stdout := writer, stdin := reader, wd0 := wd0);
    RETURN wr
  END WW;

PROCEDURE RW(cmd : TEXT; source :Rd.T; target :Wr.T; 
             wd0 : Pathname.T) =
  VAR
    reader := ProcUtils.ReadHere(source);
    writer := ProcUtils.WriteHere(target);
  BEGIN  
    EVAL ProcUtils.RunText(cmd, stdout := writer, stdin := reader, wd0 := wd0)
  END RW;


BEGIN END UnixFilter.
