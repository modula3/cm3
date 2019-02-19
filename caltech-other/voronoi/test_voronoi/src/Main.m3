(* $Id$ *)

MODULE Main;
IMPORT Voronoi;
IMPORT Debug, Fmt, Word, Tick;
IMPORT SyphPoint, SyphPointSeq, SyphPointMST;
IMPORT CardPair, CardPairSet, CardPairSetDef;
IMPORT CITRandom;
IMPORT Wr, FileWr;
IMPORT Stdio;

CONST
  MaxPts = 400;
  min = 0;
  max = 1000;
  Iters = 100;
VAR
  rand := NEW(CITRandom.T).init(FALSE);
VAR
  voronoiMu := NEW(MUTEX);
  linkArr := NEW(REF ARRAY OF ARRAY OF CARDINAL, 10, 3);
  nextLinkPtr := NEW(REF ARRAY OF CARDINAL, 10);

PROCEDURE PickSeq(pts : CARDINAL; elimDups : BOOLEAN) : SyphPointSeq.T =
  VAR
    seq := NEW(SyphPointSeq.T).init();
    added : CardPairSet.T;
    p : SyphPoint.T;
  BEGIN
    IF elimDups THEN
      added := NEW(CardPairSetDef.T).init();
    END;
    FOR i := 0 TO pts-1 DO
      p := SyphPoint.T { rand.integer(min,max),
                         rand.integer(min,max),
                         NIL };
      IF NOT elimDups OR NOT added.member(CardPair.T { p.x, p.y }) THEN 
         seq.addhi(p);
         IF elimDups THEN EVAL added.insert(CardPair.T { p.x, p.y }) END
      END
    END;
    RETURN seq
  END PickSeq;

VAR mst := NEW(SyphPointMST.T);

PROCEDURE RunIters(pts : CARDINAL; iters : CARDINAL; testDelaunay : BOOLEAN;
                   VAR edgesPerNode : REAL) =

  PROCEDURE ClearLinks() =
    BEGIN
      FOR i := 0 TO MIN(pts-1,LAST(nextLinkPtr^)) DO
        nextLinkPtr[i] := 0;
      END;
      numlinks := 0
    END ClearLinks;

  PROCEDURE AddLink(v1, v2 : CARDINAL) =
    BEGIN
      <* ASSERT v1 < v2 *>
      IF v1 > LAST(nextLinkPtr^) THEN
        ResizeLinks(MAX(NUMBER(linkArr^)*3 DIV 2 + 1,v1+1), NUMBER(linkArr[0]))
      END;
      FOR i := 0 TO nextLinkPtr[v1]-1 DO
        IF linkArr[v1,i] = v2 THEN RETURN END (* already got it *)
      END;
      IF nextLinkPtr[v1] > LAST(linkArr[v1]) THEN
        ResizeLinks(NUMBER(linkArr^), NUMBER(linkArr[v1])*3 DIV 2 + 1)
      END;
      linkArr[v1,nextLinkPtr[v1]] := v2;
      INC(nextLinkPtr[v1]);
      INC(numlinks)
    END AddLink;

  PROCEDURE ResizeLinks(v, l : CARDINAL) =
    VAR
      oldV := NUMBER(linkArr^);
      oldL := NUMBER(linkArr[0]);
      new := NEW(REF ARRAY OF ARRAY OF CARDINAL, v, l);
      newNext := NEW(REF ARRAY OF CARDINAL, v);
    BEGIN
      <* ASSERT v>=oldV AND l>=oldL *>
      FOR i := 0 TO oldV-1 DO
        FOR j := 0 TO oldL-1 DO
          new[i,j] := linkArr[i,j]
        END
      END;
      FOR i := 0 TO oldV-1 DO
        newNext[i] := nextLinkPtr[i]
      END;
      FOR i := oldV TO v-1 DO
        newNext[i] := 0
      END;
      linkArr := new;
      nextLinkPtr := newNext
    END ResizeLinks;

  VAR
    sum : REAL;
    numlinks : CARDINAL;
  BEGIN
    FOR i := 1 TO iters DO
      sum := 0.0;
      IF testDelaunay = FALSE THEN
        VAR
          netseq := PickSeq(pts, elimDups := FALSE);
          len: LONGREAL;
          from, to : SyphPoint.T;
          tot := 0.0d0;
        BEGIN
          mst := mst.init(netseq);
          FOR i := 0 TO mst.size() - 1 DO
            mst.getLink(i,from,to,len);
            tot := tot + len
          END;
          sum := sum + FLOAT(tot)
        END
      ELSE
        
        (* Delaunay triangulation first, then MST *)
        
        VAR
          netseq := PickSeq(pts, elimDups := FALSE);
          len: LONGREAL; 
          from, to : SyphPoint.T; 
          tot := 0.0d0; 
          links := NEW(CardPairSetDef.T).init();
          t : Voronoi.Triple;
        BEGIN 
          ClearLinks();
          Voronoi.Init();
          FOR i := 0 TO netseq.size() - 1 DO
            WITH p = netseq.get(i) DO
              Voronoi.AddSite(Voronoi.Point{FLOAT(p.x), FLOAT(p.y)})
            END
          END;
          Voronoi.Setup();
          
          Voronoi.Delaunay();
          WHILE Voronoi.NextTriple(t) DO
            AddLink(MIN(t.s1, t.s2), MAX(t.s1, t.s2));
            AddLink(MIN(t.s2, t.s3), MAX(t.s2, t.s3));
            AddLink(MIN(t.s3, t.s1), MAX(t.s3, t.s1))
          END;
          edgesPerNode := edgesPerNode + FLOAT(numlinks)/FLOAT(netseq.size())/ FLOAT(iters);
          Voronoi.Finish();
          
          mst := mst.init(netseq, links);
          FOR i := 0 TO mst.size() - 1 DO 
            mst.getLink(i,from,to,len); 
            tot := tot + len
          END; 
          sum := sum + FLOAT(tot)
        END
      END
    END
  END RunIters;


VAR
  now, time : Word.T;
  wrM := FileWr.Open("mst_delay.dat");
  wrD := FileWr.Open("delaunay_delay.dat");
  wrL := FileWr.Open("delaunay_links.dat");
  edgesPerNode : REAL;

BEGIN
(*
  LOOP
    RunIters(100, Iters, TRUE)
  END;
*)

  FOR pts := 4 TO MaxPts BY 2 DO
    Wr.PutText(Stdio.stdout, "\n" & Fmt.Int(pts) & " points...\n");

    edgesPerNode := 0.0;
    now := Tick.Now();
    RunIters(pts, Iters, TRUE, edgesPerNode);
    time := Tick.Now()-now;
    Wr.PutText(wrD, Fmt.Int(pts) & " " & Fmt.LongReal(Tick.ToSeconds(time)) & "\n");
    Wr.Flush(wrD);
    Wr.PutText(wrL, Fmt.Int(pts) & " " & Fmt.Real(edgesPerNode) & "\n");
    Wr.Flush(wrL);

    Wr.PutText(Stdio.stdout, "Delaunay : " & Fmt.LongReal(Tick.ToSeconds(time)) & " seconds\n");
    Wr.Flush(Stdio.stdout);

    now := Tick.Now();
    RunIters(pts, Iters, FALSE, edgesPerNode);
    time := Tick.Now()-now;
    Wr.PutText(wrM, Fmt.Int(pts) & " " & Fmt.LongReal(Tick.ToSeconds(time)) & "\n");
    Wr.Flush(wrM);
    Wr.PutText(Stdio.stdout, "non-Delaunay : " & Fmt.LongReal(Tick.ToSeconds(time)) & " seconds\n");
    Wr.Flush(Stdio.stdout)
  END;
  
  Wr.Close(wrM);
  Wr.Close(wrD);
  Wr.Close(wrL)
END Main.
