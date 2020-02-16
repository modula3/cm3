(* $Id$ *)

MODULE MagCellReader;
IMPORT MagCell, MagCellExtendable;
FROM MagCell IMPORT NotFound, T, SyntaxError;
IMPORT MagLayer AS Layer, TextMagLayerTbl AS TextLayerTbl;
IMPORT MagSubCell AS SubCell;
IMPORT Rd, Thread;
IMPORT MagTransform, MagRect, MagArrayData, TextReader, Lex, FloatMode;
IMPORT MagLabel;
IMPORT Debug, Fmt, Text, Scan;
IMPORT TextRefTbl;
IMPORT MagRouteLayer; (* for automatic filling-in of layers... *)

(* read a magic file from a reader *)

CONST DoDebug = FALSE;

VAR layerDBmu := NEW(MUTEX);

PROCEDURE ReadFromRd(res : MagCell.T; 
                     rd : Rd.T; 
                     path : TEXT; 
                     layerDB : TextLayerTbl.T;
                     fillInLayers : BOOLEAN; 
                     quiet : BOOLEAN := FALSE) : T RAISES { NotFound, Thread.Alerted, Rd.Failure, MagCell.SyntaxError } =

  PROCEDURE GetLine() : TEXT RAISES { Rd.EndOfFile, 
                                      Rd.Failure, Thread.Alerted } =
    BEGIN
      INC(lineno);
      RETURN Rd.GetLine(rd)
    END GetLine;

  VAR
    line, keyWord : TEXT;
    lineReader : TextReader.T;
    curLayers : Layer.T := NIL;
    lineno := 0;
    rect : MagRect.T; (* here for speedup ? *)
  BEGIN
      TRY
        LOOP
          line := GetLine();
          IF DoDebug THEN Debug.Out("Got \"" & line & "\"",110) END;
          
          (* parse line *)
          lineReader := NEW(TextReader.T).init(line);
          keyWord := lineReader.nextE(" ");

          IF Text.Equal(keyWord, "rect") THEN
            MagRect.ParseFromReader(lineReader, rect);
            IF NOT MagRect.IsProper(rect) THEN
              Debug.Error("Improper rect in file: \"" & path & "\": " & 
                Fmt.Int(lineno) & ": " & line)
            END;
            res.addRect(rect, curLayers)
          ELSIF Text.Equal(keyWord,"use") THEN

            (* we read other lines pertaining to subcells here, in this order:
               use
               array
               timestamp
               transform
               box
            *)
            VAR
              sub : SubCell.T;
            BEGIN
              IF DoDebug THEN Debug.Out("Reading subcell",200) END; 
              sub.c := res.lookup(lineReader.nextE(" "), layerDB, 
                                  fillInLayers, quiet);
              sub.useId := lineReader.nextE(" ");
              lineReader := NIL;
              LOOP
                VAR
                  line := GetLine();
                  reader := NEW(TextReader.T).init(line);
                  keyWord := reader.nextE(" ");
                  restOfLine := reader.nextE("");
                BEGIN
                  IF Text.Equal(keyWord, "array") THEN
                    IF DoDebug THEN Debug.Out("subcell is array",200) END;
                    sub.array := NEW(REF MagArrayData.T);
                    sub.array^ := MagArrayData.Parse(restOfLine)
                  ELSIF Text.Equal(keyWord, "timestamp") THEN
                    (* we ignore timestamp field, as we always read 
                       in everything in this code... 
                       normally the timestamp field would be used to
                       check that the bounding box is up to date, for
                       example *)
                    EVAL Scan.Int(restOfLine)
(*
                    sub.timeStamp := Scan.Int(restOfLine)
*)
                  ELSIF Text.Equal(keyWord, "transform") THEN
                    sub.transform := MagTransform.Parse(restOfLine)
                  ELSIF Text.Equal(keyWord, "box") THEN
                    sub.box := MagRect.Parse(restOfLine);
                    res.addSub(sub);
                    EXIT (* box ends a subcell use *)
                  ELSE 
                    Debug.Warning("Unknown keyword \"" & keyWord & "\"---messed up subcell declaration on line " & Fmt.Int(lineno));
                    RAISE SyntaxError
                  END
                END
              END (* LOOP *)
            END
          ELSIF Text.Equal(keyWord,"timestamp") THEN
            res.setTimeStamp( Scan.Int(lineReader.nextE("")) )
          ELSIF Text.Equal(keyWord,"rlabel") THEN
            VAR lab : MagLabel.T; BEGIN 
              MagLabel.ParseFromReader(lineReader, lab);
              res.addLabel( lab )
            END
          ELSIF Text.Equal(keyWord,"<<") THEN
            VAR 
              layerName := lineReader.nextE(" ");
            BEGIN
              LOCK layerDBmu DO
                IF NOT layerDB.get(layerName, curLayers) THEN
                  IF fillInLayers THEN
                    curLayers := NEW(MagRouteLayer.T).init(layerName);
                    EVAL layerDB.put(layerName, curLayers)
                  ELSE (* NOT fillInLayers *)
                    IF NOT badLayers.put(layerName,NIL) AND 
                      NOT Text.Equal(layerName, "end") AND 
                      NOT Text.Equal(layerName, "labels") THEN
                      Debug.Out("");
                      Debug.Warning("Unknown layer \"" & layerName & "\"");
                      RAISE SyntaxError
                    END;
                    curLayers := NIL
                  END
                END
              END
            END
          ELSIF Text.Equal(keyWord,"tech") THEN
          ELSIF Text.Equal(keyWord,"magic") THEN
          ELSE 
            Debug.Warning("Unknown keyword \"" & keyWord & "\"");
            RAISE SyntaxError
          END
        END (* LOOP *)
      EXCEPT
      | Rd.EndOfFile => (* skip *)
      | TextReader.NoMore, MagArrayData.ParseError, MagRect.ParseError,
      MagLabel.ParseError,
      FloatMode.Trap, Lex.Error, MagTransform.ParseError => 
      Debug.Error("Syntax error: \"" & path & "\" : " & Fmt.Int(lineno))
    | MagCellExtendable.ReadOnlyTimeStamp =>
      Debug.Error("Attempting to re-set cell timestamp at \"" & path & 
        "\" : " & Fmt.Int(lineno))
    END; (* TRY EXCEPT *)
    RETURN res
  END ReadFromRd;

VAR badLayers := NEW(TextRefTbl.Default).init();

BEGIN END MagCellReader.
