(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Tue Jan 31 15:40:35 PST 1995 by kalsow   *)
(*      modified on Thu Apr 28 17:14:30 PDT 1994 by najork   *)
(*      modified on Sat Oct 17 11:30:08 PDT 1992 by ramshaw  *)

MODULE HullInput;

IMPORT AlgTypes, FormsVBT, HullAlgClass, Random, Text, Thread, VBT,
       FileRd, OSError, Sx, Rd, RefList, ZeusPanel;

<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>

EXCEPTION Confusion;
<*FATAL Confusion *>

PROCEDURE GetSites (    alg   : HullAlgClass.T;
                    VAR nSites: INTEGER): AlgTypes.Sites RAISES
                    {Failure, Thread.Alerted} =
  VAR
    inStyle, fName : Text.T;
    n, k, x, y: INTEGER;
    res       : AlgTypes.Sites;
    inFile    : Rd.T;
    s, pt     : RefList.T;
    rand      : Random.T;
  BEGIN
    LOCK VBT.mu DO
      inStyle := FormsVBT.GetChoice(alg.data, "InStyle");
      IF Text.Equal(inStyle, "Random") THEN
        n := FormsVBT.GetInteger(alg.data, "N");
        IF FormsVBT.GetBoolean(alg.data, "UsePrev") THEN
          rand := NEW (Random.Default).init (fixed := TRUE);
        ELSE 
          rand := NEW (Random.Default).init ();
        END;
      ELSIF Text.Equal(inStyle, "File") THEN
        fName := FormsVBT.GetText(alg.data, "Name");
      ELSE
        RAISE Confusion
      END;
    END;
    IF Text.Equal(inStyle, "Random") THEN
      res := NEW(AlgTypes.Sites, n + 2); (* +2 for the sentinels *)
      k := 1;
      WHILE k <= n DO
        LOOP
          x := rand.integer(min := -10000, max := 10000);
          y := rand.integer(min := -10000, max := 10000);
          IF x * x + y * y > 100000000 THEN EXIT END;
          res[k] := AlgTypes.Site{k, Label(k), x, y};
          k := k + 1;
          EXIT;
        END;
      END;
    ELSE                        (* get input from file *)
      TRY  inFile := FileRd.Open (fName);
      EXCEPT OSError.E => 
          ZeusPanel.ReportError("Trouble reading file");
          RAISE Failure;
      END;
      TRY
        s := Sx.Read(inFile)
      EXCEPT
        Rd.EndOfFile, Sx.ReadError =>
          ZeusPanel.ReportError("Trouble reading file");
          RAISE Failure;
      END;
      TRY Rd.Close(inFile) EXCEPT Rd.Failure => END;
      n := RefList.Length(s);
      res := NEW(AlgTypes.Sites, n + 2);
      FOR k := 1 TO n DO
        pt := s.head;
        s := s.tail;
        res[k].uid := k;
        res[k].lab := NARROW(RefList.Nth (pt,0), TEXT);
        res[k].x := NARROW(RefList.Nth (pt,1), REF INTEGER)^;
        res[k].y := NARROW(RefList.Nth (pt,2), REF INTEGER)^;
      END;
    END;
    nSites := n;
    IF n = 0 THEN RETURN res END;
    VAR
      minX, minY, maxX, maxY        : INTEGER;
      xScale, xShift, yScale, yShift: INTEGER;
    BEGIN
      minX := res[1].x;
      maxX := res[1].x;
      minY := res[1].y;
      maxY := res[1].y;
      FOR i := 2 TO n DO
        minX := MIN(minX, res[i].x);
        maxX := MAX(maxX, res[i].x);
        minY := MIN(minY, res[i].y);
        maxY := MAX(maxY, res[i].y);
      END;
      xScale := 20000 DIV (maxX - minX + 1);
      yScale := 20000 DIV (maxY - minY + 1);
      xShift := (xScale * minX + xScale * maxX) DIV 2;
      yShift := (yScale * minY + yScale * maxY) DIV 2;
      FOR i := 1 TO n DO
        res[i].x := xScale * res[i].x - xShift;
        res[i].y := yScale * res[i].y - yShift;
      END;
    END;
    RETURN (res);
  END GetSites;

PROCEDURE Label (idx: INTEGER): Text.T =
  BEGIN
    <* ASSERT idx >= 1 *>
    IF idx <= 26 THEN
      RETURN (Text.FromChar(VAL(ORD('A') + idx - 1, CHAR)))
    ELSIF idx <= 52 THEN
      RETURN (Text.FromChar(VAL(ORD('a') + idx - 27, CHAR)))
    ELSE
      RETURN (Text.FromChar('-'))
    END;
  END Label;


BEGIN
END HullInput.
