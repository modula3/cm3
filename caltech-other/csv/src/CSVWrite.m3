(* $Id$ *)

MODULE CSVWrite;
IMPORT Wr, FinDate, Fmt;

IMPORT Thread;

<*FATAL Thread.Alerted*>

REVEAL
  Labeler = PublicLabeler BRANDED OBJECT 
    topLabel := "DATE";
  METHODS
  OVERRIDES
    setTopLabel := SetTopLabel;
  END;

PROCEDURE SetTopLabel(l : Labeler; tl : TEXT) =
  BEGIN l.topLabel := tl END SetTopLabel;

TYPE
  TALabeler = Labeler OBJECT
    t : REF ARRAY OF TEXT;
  OVERRIDES
    label := TALabel;
    n := TAn;
  END;

  FALabeler = Labeler OBJECT
    t : REF ARRAY OF FinDate.T;
  OVERRIDES
    label := FALabel;
    n := FAn;
  END;

PROCEDURE TALabel(t : TALabeler; i : CARDINAL) : TEXT =
  BEGIN RETURN t.t[i] END TALabel;

PROCEDURE TAn(t : TALabeler) : CARDINAL = BEGIN RETURN NUMBER(t.t^) END TAn;

PROCEDURE TextArrLabeler(READONLY t : ARRAY OF TEXT) : Labeler =
  VAR   tt  := NEW(REF ARRAY OF TEXT, NUMBER(t));
  BEGIN tt^ := t; RETURN NEW(TALabeler, t := tt) END TextArrLabeler;

PROCEDURE FALabel(t : FALabeler; i : CARDINAL) : TEXT =
  BEGIN RETURN FinDate.Format(t.t[i]) END FALabel;

PROCEDURE FAn(t : FALabeler) : CARDINAL = BEGIN RETURN NUMBER(t.t^) END FAn;

PROCEDURE FinDateArrLabeler(READONLY t : ARRAY OF FinDate.T) : Labeler =
  VAR   tt  := NEW(REF ARRAY OF FinDate.T, NUMBER(t));
  BEGIN tt^ := t; RETURN NEW(FALabeler, t := tt) END FinDateArrLabeler;

TYPE
  CLabeler = Labeler OBJECT
    number : CARDINAL;
  OVERRIDES
    label := CLabel;
    n := Cn;
  END;

PROCEDURE CLabel(<*UNUSED*>t : CLabeler; i : CARDINAL) : TEXT =
  BEGIN RETURN Fmt.Int(i) END CLabel;

PROCEDURE Cn(t : CLabeler) : CARDINAL = BEGIN RETURN t.number END Cn;

PROCEDURE CardinalLabeler(n : CARDINAL) : Labeler =
  BEGIN RETURN NEW(CLabeler, number := n) END CardinalLabeler;

PROCEDURE EmptyLabeler(n : CARDINAL) : Labeler =
  BEGIN RETURN NEW(CLabeler, number := n, label := ELabel) END EmptyLabeler;

PROCEDURE ELabel(<*UNUSED*>t : CLabeler; <*UNUSED*>i : CARDINAL) : TEXT =
  BEGIN RETURN "" END ELabel;

(**********************************************************************)

PROCEDURE Write2D(wr : Wr.T; 
                  rl : Labeler;
                  READONLY xx : Array2;
                  xl : Labeler;
                  READONLY yy : Array2;
                  yl : Labeler;
                  READONLY zz : Array2;
                  zl : Labeler;
                  READONLY tt : Array2;
                  tl : Labeler;
                  transpose : BOOLEAN) 
  RAISES { Wr.Failure } =

  TYPE Index = [-1..LAST(CARDINAL) ];

  PROCEDURE PickWidth(READONLY aa : Array2; VAR w : Index) =
    BEGIN
      IF NUMBER(aa) = 0 THEN w := -1 
      ELSIF transpose THEN w := LAST(aa) 
      ELSE w := LAST(aa[0]) 
      END
    END PickWidth;

  PROCEDURE Header(READONLY aa : Array2; al : Labeler) RAISES { Wr.Failure } =
    VAR l : Index;
    BEGIN
      PickWidth(aa,l);
      FOR i := 0 TO l DO
        Wr.PutChar(wr, ','); Wr.PutText(wr, al.label(i))
      END;
    END Header;

  PROCEDURE Row(i : CARDINAL;
                READONLY aa : Array2) RAISES { Wr.Failure } =
  VAR
    l : Index;
  BEGIN
    PickWidth(aa,l);
    FOR m := 0 TO l DO
      VAR x : LONGREAL;
      BEGIN
        IF transpose THEN x := aa[m,i] ELSE x := aa[i,m] END;
        Wr.PutChar(wr, ','); Wr.PutText(wr, Fmt.LongReal(x))
      END
    END;
  END Row;

  BEGIN

    Wr.PutText(wr, rl.topLabel);
    Header(xx,xl); Header(yy,yl); Header(zz,zl); Header(tt, tl);
    Wr.PutChar(wr, '\n');

    FOR i := 0 TO rl.n()-1 DO
      Wr.PutText(wr, rl.label(i));
      Row(i, xx); Row(i, yy); Row(i, zz); Row(i, tt);
      Wr.PutChar(wr, '\n')
    END
  END Write2D;

PROCEDURE Write1D(wr : Wr.T; 
                  rl : Labeler;
                  READONLY xx : Array1) 
  RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "DATE");
    Wr.PutChar(wr, ',');
    Wr.PutText(wr, "DATA");
    Wr.PutChar(wr, '\n');

    FOR i := 0 TO rl.n()-1 DO
      Wr.PutText(wr, rl.label(i));
      Wr.PutChar(wr, ',');
      Wr.PutText(wr, Fmt.LongReal(xx[i]));
      Wr.PutChar(wr, '\n')
    END
  END Write1D;

PROCEDURE WriteSelfCrossLabels(wr : Wr.T; cl : Labeler) 
  RAISES { Wr.Failure } =
  VAR n := cl.n();
  BEGIN
    Wr.PutText(wr, "DATE");
    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO
        Wr.PutChar(wr, ','); Wr.PutText(wr, cl.label(i))
      END
    END;
    Wr.PutChar(wr, '\n');
    Wr.PutText(wr, "DATE");
    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO
        Wr.PutChar(wr, ','); Wr.PutText(wr, cl.label(j))
      END
    END;
    Wr.PutChar(wr, '\n');

  END WriteSelfCrossLabels;

PROCEDURE WriteCovariances(wr          : Wr.T;
                           READONLY cv : Array4;
                           cl          : Labeler;
                           READONLY da : ARRAY OF FinDate.T;
                           s           : CARDINAL) 
  RAISES { Wr.Failure } =
  BEGIN
    WriteSelfCrossLabels(wr, cl);

    FOR k := FIRST(cv[0,0]) TO LAST(cv[0,0]) DO
      Wr.PutText(wr, FinDate.Format(da[k]));
    
      FOR i := FIRST(cv) TO LAST(cv) DO
        FOR j := FIRST(cv) TO LAST(cv) DO
          Wr.PutChar(wr, ',');
          Wr.PutText(wr, Fmt.LongReal(cv[i,j,k,s]))
        END
      END;
      Wr.PutChar(wr, '\n');
    END
  END WriteCovariances;

PROCEDURE WriteSCovariances(wr          : Wr.T;
                           READONLY cv : Array3;
                           cl          : Labeler;
                           READONLY da : ARRAY OF FinDate.T) 
  RAISES { Wr.Failure } =
  BEGIN
    WriteSelfCrossLabels(wr, cl);
    FOR k := FIRST(cv[0,0]) TO LAST(cv[0,0]) DO
      Wr.PutText(wr, FinDate.Format(da[k]));
    
      FOR i := FIRST(cv) TO LAST(cv) DO
        FOR j := FIRST(cv) TO LAST(cv) DO
          Wr.PutChar(wr, ',');
          Wr.PutText(wr, Fmt.LongReal(cv[i,j,k]))
        END
      END;
      Wr.PutChar(wr, '\n');
    END
  END WriteSCovariances;


BEGIN END CSVWrite.
