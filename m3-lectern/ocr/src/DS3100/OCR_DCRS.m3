(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Last modified on Tue Oct  4 12:10:07 PDT 1994 by mcjones *)

UNSAFE MODULE OCR_DCRS EXPORTS OCR;

IMPORT DCRS, Rd, Rect, Thread, Wr;

PROCEDURE IsFromPBMImplemented(): BOOLEAN =
  BEGIN RETURN TRUE END IsFromPBMImplemented;

TYPE WriteWords = DCRS.WordSink OBJECT
    wr: Wr.T;
    n: CARDINAL;
  METHODS
    init(wr: Wr.T): WriteWords := WWInit;
  OVERRIDES
    word := WriteWord;
    fontChange := FontChange;
    newLine := NewLine
  END;

PROCEDURE WWInit(ws: WriteWords; wr: Wr.T): WriteWords =
  BEGIN
    ws.wr := wr;
    ws.n := 0;
    RETURN ws
  END WWInit;

PROCEDURE WriteWord(ws: WriteWords;
    <*UNUSED*>segmentNumber, blockNumber: CARDINAL;
    w: TEXT;
    <*UNUSED*>READONLY r: Rect.T) RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutText(ws.wr, w); Wr.PutChar(ws.wr, '\n');
    INC(ws.n)
  END WriteWord;

TYPE WriteBounds = DCRS.WordSink OBJECT
    wr: Wr.T
  METHODS
    init(wr: Wr.T): WriteBounds := WBInit;
  OVERRIDES
    word := WriteBound;
    fontChange := FontChange;
    newLine := NewLine
  END;

PROCEDURE WBInit(ws: WriteBounds; wr: Wr.T): WriteBounds =
  BEGIN
    ws.wr := wr;
    RETURN ws
  END WBInit;

PROCEDURE WriteBound(ws: WriteBounds;
    <*UNUSED*>segmentNumber, blockNumber: CARDINAL;
    <*UNUSED*>w: TEXT;
    READONLY r: Rect.T) RAISES {Thread.Alerted, Wr.Failure} =
  VAR r1: ARRAY [0..3] OF BITS 32 FOR INTEGER;
  BEGIN
    r1[0] := r.west; r1[1] := r.south; r1[2] := r.east; r1[3] := r.north;
    Wr.PutString(ws.wr, LOOPHOLE(r1, ARRAY [0..4*4-1] OF CHAR))
  END WriteBound;

PROCEDURE FontChange(<*UNUSED*>ws: DCRS.WordSink;
    <*UNUSED*>segmentNumber, blockNumber: CARDINAL;
    <*UNUSED*>f: DCRS.Font;
    <*UNUSED*>italic, bold, underline: BOOLEAN;
    <*UNUSED*>pointSize: CARDINAL) =
  BEGIN END FontChange;

PROCEDURE NewLine(<*UNUSED*>ws: DCRS.WordSink; <*UNUSED*>segmentNumber, blockNumber: CARDINAL) =
  BEGIN END NewLine;

PROCEDURE FromPBM(
    rd: Rd.T;
    wr: Wr.T;
    VAR (*OUT*) nWords, nWordsBytes: CARDINAL;
    reject: CHAR := '#';
    resolution: CARDINAL := 300)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted, Error} =
  VAR r: DCRS.Resolution;
  BEGIN
    IF resolution <= 200 THEN r := DCRS.Resolution.DPI200
    ELSIF resolution <= 300 THEN r := DCRS.Resolution.DPI300
    ELSE r := DCRS.Resolution.DPI400
    END;
    TRY
      WITH i = DCRS.FromPBM(rd, r) DO
        TRY
          WITH ws = NEW(WriteWords).init(wr), 
               index = Wr.Index(wr) DO
            DCRS.GetWords(i, ws, reject);
            nWords := ws.n;
            nWordsBytes := Wr.Index(wr) - index
          END;
          WITH ws = NEW(WriteBounds).init(wr) DO
            DCRS.GetWords(i, ws, reject)
          END
        FINALLY DCRS.Close(i)
        END
      END
    EXCEPT DCRS.Error(code) => RAISE Error(code)
    END
  END FromPBM;

BEGIN
END OCR_DCRS.
