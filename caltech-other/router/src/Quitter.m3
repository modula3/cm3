(* $Id$ *)

MODULE Quitter;
IMPORT GridPointSet, GridPointSetDef;
IMPORT Rd, Scan, Lex, FloatMode, Text, TextReader, GridPoint;
IMPORT Thread;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
    fn : TEXT;
    v, vt : LONGREAL;
    ft : INTEGER;
    f : CARDINAL;
    pts : GridPointSet.T;
  OVERRIDES
    read := Read;
    ripupPoints := RipupPoints;
    endVelocity := EndVelocity;
    endFailures := EndFailures;
  END;

PROCEDURE RipupPoints(t : T) : GridPointSet.T =
  BEGIN RETURN t.pts.copy() END RipupPoints;

PROCEDURE EndVelocity(t : T) : LONGREAL =
  BEGIN RETURN t.v END EndVelocity;

PROCEDURE EndFailures(t : T) : CARDINAL =
  BEGIN RETURN t.f END EndFailures;

CONST White=" \t";

PROCEDURE Read(t : T; rd : Rd.T) : T RAISES { Error } =
  BEGIN
    t.pts := NEW(GridPointSetDef.T).init();

    TRY
      LOOP
        VAR
          line := Rd.GetLine(rd);
          reader := NEW(TextReader.T).init(line);
          k := reader.nextE(White);
        BEGIN
          IF Text.Equal(k, "QUITFILE") THEN
            t.fn := reader.nextE(White)
          ELSIF Text.Equal(k, "R") THEN
            VAR
              str := reader.nextE(White);
              p := GridPoint.Parse(str);
            BEGIN
              EVAL t.pts.insert(p)
            END
          ELSIF Text.Equal(k, "VELOCITYTHRESHOLD") THEN
            t.vt := Scan.LongReal(reader.nextE(White))
          ELSIF Text.Equal(k, "VELOCITY") THEN
            t.v := Scan.LongReal(reader.nextE(White))
          ELSIF Text.Equal(k, "FAILURETHRESHOLD") THEN
            t.ft := Scan.Int(reader.nextE(White))
          ELSIF Text.Equal(k, "ACTUALFAILURES") THEN
            t.f := Scan.Int(reader.nextE(White))
          ELSIF Text.Equal(k, "END") THEN
            RETURN t
          ELSE
            RAISE Error("Unknown keyword \"" & k & "\"")
          END;
          IF NOT reader.isEmpty() THEN
            RAISE Error("Extra junk in file")
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error("Short read")
    |
      Rd.Failure => RAISE Error("I/O error during read")
    |
      Lex.Error, FloatMode.Trap => RAISE Error("Parse error expecting number")
    |
      TextReader.NoMore => RAISE Error("Not enough args reading")
    |
      GridPoint.ParseError => RAISE Error("Parse error expecting GridPoint")
    END
  END Read;

BEGIN END Quitter.
