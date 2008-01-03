(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 11:34:23 PST 1995 by kalsow                   *)
(*      modified on Tue Jun 16 16:46:30 PDT 1992 by muller                   *)

MODULE GraphData;

IMPORT Thread, Text, RealRect, Rd, (*Wr,*) UnFmt;

<* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted *>

PROCEDURE Copy(g: T): T =
  VAR
    new: T;
    n: INTEGER;
  BEGIN
    n := NUMBER(g^);
    new := NEW(T, n);
    FOR i := 0 TO n-1 DO
      new^[i] := g^[i];
      DoEdges(new^[i], g^[i]);
    END;
    RETURN new;
  END Copy;

(* DoEdges:  a subroutine of Copy, here to avoid the multiple-indexing
   compiler bug. *)
PROCEDURE DoEdges(VAR to: VertexRecord; READONLY from: VertexRecord) =
  VAR
    m: INTEGER;
    newPoints: PointArray;
  BEGIN
    FOR j := 0 TO MaxEdge DO
      IF from.edge[j].segPoints # NIL THEN
        WITH e = from.edge[j] DO
          m := NUMBER(e.segPoints^);
          newPoints := NEW(PointArray, m);
          FOR k := 0 TO m-1 DO
            newPoints^[k] := e.segPoints^[k];
          END;
        END;
        to.edge[j].segPoints := newPoints;
      END;
    END;
  END DoEdges;

PROCEDURE Expand(g: T; size: CARDINAL): T =
  VAR
    new: T;
    n: INTEGER;
  BEGIN
    n := NUMBER(g^);
    IF size <= n THEN RETURN g END;
    new := NEW(T, size);
    FOR i := 0 TO n-1 DO
      new^[i] := g^[i];
    END;
    RETURN new;
  END Expand;

PROCEDURE CountNodes(g: T): CARDINAL =
  VAR
    count: CARDINAL;
  BEGIN
    count := 0;
    FOR i := 0 TO NUMBER(g^)-1 DO
      IF g^[i].present THEN
        INC(count);
      END;
    END;
    RETURN count;
  END CountNodes;


CONST
  Huge = 1.0E8;

PROCEDURE BoundingBox(g: T): RealRect.T =
  VAR
    n: INTEGER;
    pa: PointArray;
    xmin, xmax, ymin, ymax: REAL;
  BEGIN
    n := NUMBER(g^);
    xmin := Huge;  xmax := -Huge;
    ymin := Huge;  ymax := -Huge;

    FOR i := 0 TO n-1 DO
      IF g^[i].present THEN
        WITH ver = g^[i] DO
          IF ver.x < xmin THEN xmin := ver.x END;
          IF ver.x > xmax THEN xmax := ver.x END;
          IF ver.y < ymin THEN ymin := ver.y END;
          IF ver.y > ymax THEN ymax := ver.y END;

          (* The following is here to include segPoints in the
             calculation *)
          FOR j := 0 TO MaxEdge DO
            IF ver.edge[j].present AND (ver.edge[j].segPoints # NIL) THEN
              pa := ver.edge[j].segPoints;
              FOR k := 0 TO NUMBER(pa^)-1 DO
                WITH pt = pa^[k] DO
                  IF pt.h < xmin THEN xmin := pt.h END;
                  IF pt.h > xmax THEN xmax := pt.h END;
                  IF pt.v < ymin THEN ymin := pt.v END;
                  IF pt.v > ymax THEN ymax := pt.v END;
                END;
              END; (* FOR on segPoints *)
            END; (* IF segPoints # NIL *)
          END; (* FOR on edges *)

        END; (* WITH g^[i] *)
      END; (* IF present *)
    END; (* FOR i *)
    RETURN RealRect.FromEdges(xmin, xmax, ymin, ymax);
  END BoundingBox;


PROCEDURE RevPoints(in: PointArray): PointArray =
  VAR
    out: PointArray;
    n: INTEGER;
  BEGIN
    n := NUMBER(in^);
    out := NEW(PointArray, n);
    FOR i := 0 TO n-1 DO
      out^[i] := in^[n-i-1];
    END;
    RETURN out;
  END RevPoints;


PROCEDURE ExtractString(VAR line: Text.T): Text.T =
VAR
  text: Text.T;
  length: CARDINAL;
  i,j: CARDINAL;
BEGIN
  text := "";
  length := Text.Length(line);
  IF (length=0) THEN  RETURN text; END;
  i := 0;
  WHILE (i < length) AND (Text.GetChar(line, i) = ' ') DO
    i := i + 1;
  END;
  IF (i < length) THEN
    IF i < (length - 1) THEN
      j := i+1;
      WHILE (j < length) AND (Text.GetChar(line, j) # ' ') DO
        j := j + 1;
      END;      
      text := Text.Sub(line, i, j-i);
      line := Text.Sub(line, j, length-j);
    ELSE
      text := Text.Sub(line, i, 1);
      line := "";
    END;
  ELSE
    line := "";
  END;
  RETURN text;
END ExtractString;


PROCEDURE EdgeExists(g: T; a, b: CARDINAL): BOOLEAN =
  BEGIN
    IF g^[a].present THEN
      WITH ver = g^[a] DO
        FOR j := 0 TO MaxEdge DO
          IF ver.edge[j].present AND (ver.edge[j].dest = b) THEN
            RETURN TRUE;
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END EdgeExists;


PROCEDURE ReadBendPoints(rd: Rd.T): PointArray =
  VAR
    totalBendPoints: INTEGER;
    ch: CHAR;
    segPoints: PointArray;
    bendText: Text.T;
    text: Text.T;
  BEGIN
    ch := Rd.GetChar(rd);
    Rd.UnGetChar(rd);

    IF ch = 'B' THEN
      bendText := Rd.GetLine(rd);
      text := ExtractString(bendText); (* to eliminate BendPoints *)
      totalBendPoints := UnFmt.ToInt(ExtractString(bendText));
(*
      Wr.PutText(Stdio.stdout, "Read BendPoints --\n");
      Wr.Flush(Stdio.stdout);
*)

      IF totalBendPoints > 0 THEN
        segPoints := NEW(PointArray, totalBendPoints);
      ELSE
        segPoints := NIL;
      END;

      FOR j := 0 TO totalBendPoints-1 DO
         bendText := Rd.GetLine(rd);
(*
         Wr.PutText(Stdio.stdout, bendText);
         Wr.PutText(Stdio.stdout, "\n");
         Wr.Flush(Stdio.stdout);
*)
         segPoints^[j].h := UnFmt.ToReal(ExtractString(bendText));
         segPoints^[j].v := UnFmt.ToReal(ExtractString(bendText));
(*
         Wr.PutText(Stdio.stdout, "Read bend point\n");
         Wr.Flush(Stdio.stdout);
*)
      END;

    ELSE
      segPoints := NIL;
    END;
    RETURN segPoints;
  END ReadBendPoints;


PROCEDURE ReadEdge(rd: Rd.T): EdgeRecord =
VAR
  edge: EdgeRecord;
  edgeText: Text.T;
BEGIN
  edgeText := Rd.GetLine(rd);
  IF Text.Equal(edgeText,"") THEN
    edge.present := FALSE;
  ELSE
    edgeText := Rd.GetLine(rd);
    edge.dest := UnFmt.ToInt(ExtractString(edgeText));
    edge.destEdge := UnFmt.ToInt(ExtractString(edgeText));
(*    Wr.PutText(Stdio.stdout, "Read Edge");
      Wr.PutText(Stdio.stdout,"\n");
      Wr.Flush(Stdio.stdout);
*)
    edge.present := TRUE;
    edge.segPoints := ReadBendPoints(rd);
    edge.etc := NIL;
  END;
  RETURN edge;
END ReadEdge;


PROCEDURE ReadNode(rd: Rd.T): VertexRecord =
VAR
  edgeArray: ARRAY EdgeIndex OF EdgeRecord;
  e: EdgeRecord;
  vtext: Text.T;
  i: INTEGER;
  v: VertexRecord;
BEGIN
  vtext := Rd.GetLine(rd);
  IF Text.Equal(vtext,"") THEN
    v.present := FALSE
  ELSE
    v.present := TRUE;
    v.name := vtext;
    vtext := Rd.GetLine(rd);
    v.x := UnFmt.ToReal(ExtractString(vtext));
    v.y := UnFmt.ToReal(ExtractString(vtext));
    v.api := UnFmt.ToReal(ExtractString(vtext));
(*    Wr.PutText(Stdio.stdout, "Read vertex ");
      Wr.PutText(Stdio.stdout,"\n");
      Wr.Flush(Stdio.stdout);
*)
    i := 0;
    e := ReadEdge(rd);
    WHILE (e.present # FALSE) DO
      edgeArray[i] := e;
      e := ReadEdge(rd);
      i := i + 1;
    END;

    v.edge := edgeArray;
    v.etc := NIL;
  END;
  RETURN v;
END ReadNode;


PROCEDURE ReadGraph(rd: Rd.T):T =
VAR
  line: Text.T;
  n: INTEGER;
  text: Text.T;
  g: T;
BEGIN
  line := Rd.GetLine(rd);  
  text := ExtractString(line);
  n := UnFmt.ToInt(text);
  g := NEW(T, n);
  FOR i := 0 TO n-1 DO
    g^[i] := ReadNode(rd);
  END;
  RETURN g;
END ReadGraph;

BEGIN
END GraphData.








