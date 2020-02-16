(* $Id$ *)

MODULE Main;
IMPORT TextMagLayerTbl;
IMPORT Process, Stdio, Wr, Params;
IMPORT StdCellDB, StdCell, Env;
IMPORT MagCell;
IMPORT Thread;
IMPORT Fmt;
IMPORT OSError, Rd;
IMPORT Text, Char;
IMPORT MagTransform;

(* magsize [-magic] [-recursive] <type_1> ... <type_n> *)

<* FATAL Thread.Alerted, Wr.Failure, OSError.E, Rd.Failure, StdCellDB.Error *>
EXCEPTION ParamErr;
        
(* parameter things should be broken out as a "database" *)
          
PROCEDURE GetParam(n : CARDINAL) : TEXT RAISES { ParamErr } =
  BEGIN   
    IF n < Params.Count THEN RETURN Params.Get(n) ELSE RAISE ParamErr END
  END GetParam;


PROCEDURE PrintOut(wr : Wr.T; 
                   type : TEXT; transform : MagTransform.T;
                   cell : MagCell.T) RAISES { Wr.Failure } =
  BEGIN
    IF cell.empty() THEN
      Wr.PutText(wr, "0 0 0 0 "&type&"\n")
    ELSE
      VAR
        box := MagTransform.Rect(cell.getBBox(),transform);
      BEGIN
        Wr.PutText(wr, Fmt.Int(box.ll.x) & " " &
          Fmt.Int(box.ll.y) & " " &
          Fmt.Int(box.ur.x) & " " &
          Fmt.Int(box.ur.y) & " " & type & "\n")
      END
    END
  END PrintOut;

PROCEDURE PrintOutGPL(wr : Wr.T; 
                      <*UNUSED*>type : TEXT; 
                      transform : MagTransform.T; 
                      cell : MagCell.T) RAISES { Wr.Failure } =
  BEGIN
    IF NOT cell.empty() THEN
      VAR
        box := MagTransform.Rect(cell.getBBox(),transform);
        llx := Fmt.Int(box.ll.x);
        lly := Fmt.Int(box.ll.y);
        urx := Fmt.Int(box.ur.x);
        ury := Fmt.Int(box.ur.y);
      BEGIN
        (* CCW *)
        Wr.PutText(wr, llx & " " & lly & "\n"); (* SW *)
        Wr.PutText(wr, llx & " " & ury & "\n"); (* NW *)
        Wr.PutText(wr, urx & " " & ury & "\n"); (* NE *)
        Wr.PutText(wr, urx & " " & lly & "\n"); (* SE *)
        Wr.PutText(wr, llx & " " & lly & "\n"); (* SW *)
        Wr.PutChar(wr, '\n')
      END
    END
  END PrintOutGPL;

TYPE Printer = PROCEDURE(wr : Wr.T; typenam : TEXT;
                         trans : MagTransform.T; cell : MagCell.T) RAISES { Wr.Failure };

TYPE PrinterObj = OBJECT printer : Printer END;


PROCEDURE CellProc(sub : MagCell.T; <*UNUSED*>id : TEXT; trans : MagTransform.T;
                   <*UNUSED*>pid : TEXT; args : REFANY) RAISES { Wr.Failure } =
  VAR
    printerObj : PrinterObj := args;
    printer := printerObj.printer;
  BEGIN
    printer(Stdio.stdout, sub.getName(), trans, sub)
  END CellProc;

<* FATAL ParamErr *>
VAR
  basePath := Env.Get("BASEPATH");
  layerDB := NEW(TextMagLayerTbl.Default).init();
  db : StdCellDB.T;
  stdcell : StdCell.T;
  magcel : MagCell.T;
  i := 1;
  newparam : TEXT;
  doMagic, gnuplot, recursive := FALSE;
BEGIN
  IF basePath = NIL THEN
    Process.Crash("Please define BASEPATH before running magsize!")
  END;

  db := NEW(StdCellDB.T).populate(layerDB, fillInLayers := TRUE);

  WHILE Char.Equal(Text.GetChar(Params.Get(i),0), '-') DO
    newparam := GetParam(i);
    IF Text.Equal(newparam, "-magic") THEN 
      doMagic := TRUE; i:=i+1
    ELSIF Text.Equal(newparam, "-gnuplot") THEN
      gnuplot := TRUE; i:=i+1
    ELSIF Text.Equal(newparam, "-recursive") THEN
      recursive := TRUE; i:=i+1
    ELSE 
      Process.Crash("Unknown command-line flag \"" & newparam & "\"!")
    END
  END;

  FOR j := i TO Params.Count - 1 DO
    VAR
      celtype := Params.Get(j);
    BEGIN
      IF doMagic THEN
        TRY
          magcel := NEW(MagCell.Labelled).lookup(celtype,
                                                 layerDB,
                                                 fillInLayers := TRUE)
        EXCEPT
          MagCell.NotFound => Process.Crash("Magic cell \"" & celtype & 
            "\" not found.")
        |
          MagCell.SyntaxError => Process.Crash("Syntax error reading cell \"" &
            celtype & "\".")
        END
      ELSE
        IF NOT db.get(celtype, stdcell) THEN
          Process.Crash("Can't find cell of type \"" & celtype & "\"!")
        END;
        magcel :=stdcell.getLayout(celtype, basePath, widths := NIL);
      END;
      VAR
        printer : Printer;
      BEGIN
        IF gnuplot THEN
          printer := PrintOutGPL
        ELSE
          printer := PrintOut
        END;
        IF recursive THEN
          <* FATAL ANY *>
          BEGIN
            magcel.subCellMap(CellProc, 
                              args := NEW(PrinterObj, printer := printer))
          END
        ELSE
          printer(Stdio.stdout, celtype, MagTransform.Unitary, magcel)
        END
      END
    END
  END
END Main.
