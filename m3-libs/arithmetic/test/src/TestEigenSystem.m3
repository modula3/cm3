(*  -*- modula-3 -*-
 *
 * Test Tred[1234].
 *)
MODULE Main;

IMPORT EigenSystem;
IMPORT Wr, Stdio, Math, Thread, Fmt;

<* FATAL Wr.Failure *>
<* FATAL Thread.Alerted *>
<* FATAL EigenSys.NoConvergence *>
<* FATAL EigenSys.ArrayTooSmall *>
<* FATAL EigenSys.ArraySizesDontMatch *>

PROCEDURE Print2(d,e: REF ARRAY OF LONGREAL;
                 dWR, eWR: ARRAY OF LONGREAL) RAISES {}=
  BEGIN
    Wr.PutText(Stdio.stdout, Fmt.Pad("i",4) &
      Fmt.Pad("Diagonale",18) &
      Fmt.Pad("Sub-Diagonale",18) &"\n" );
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(i),2) &
        Fmt.Pad(Fmt.LongReal(d[i],12,Fmt.Style.Sci),19) &
        Fmt.Pad(Fmt.LongReal(e[i],12,Fmt.Style.Sci),19) &"\n");
    END; (* for *)
    Wr.PutText( Stdio.stdout, "\nVergleich mit Wilkinson-Reinsch:\n");
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(i),4) &
        Fmt.Pad(Fmt.LongReal(d[i]-dWR[i]),18) &
        Fmt.Pad(Fmt.LongReal(e[i]-eWR[i]),18) &"\n");
    END; (* for *)
    
  END Print2;

PROCEDURE Print3(d,e,e2 : REF ARRAY OF LONGREAL;
                 dWR,eWR,e2WR: ARRAY OF LONGREAL) RAISES {}=
  BEGIN
    Wr.PutText(Stdio.stdout, Fmt.Pad("i",4) &
      Fmt.Pad("Diagonale",18) &
      Fmt.Pad("Sub-Diagonale",18) &
      Fmt.Pad("Sub-Diagonale**2",18) &"\n" );
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(i),2) &
        Fmt.Pad(Fmt.LongReal(d[i],12,Fmt.Style.Sci),19) &
        Fmt.Pad(Fmt.LongReal(e[i],12,Fmt.Style.Sci),19) &
        Fmt.Pad(Fmt.LongReal(e2[i],12,Fmt.Style.Sci),19) &"\n");
    END; (* for *)
    Wr.PutText( Stdio.stdout, "\nVergleich mit Wilkinson-Reinsch:\n");
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(i),4) &
        Fmt.Pad(Fmt.LongReal(d[i]-dWR[i]),18) &
        Fmt.Pad(Fmt.LongReal(e[i]-eWR[i]),18) &
        Fmt.Pad(Fmt.LongReal(e2[i]-e2WR[i]),18) &"\n");
    END; (* for *)
    
  END Print3;


PROCEDURE RunTql1(VAR d,e: REF ARRAY OF LONGREAL) RAISES {}=
  BEGIN
    EigenSys.Tql1( d, e);
    Wr.PutText(Stdio.stdout, "Eigenwerte\n");
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText( Stdio.stdout, Fmt.Pad(Fmt.LongReal( d[i],12),20) & "\n");
    END; (* for *)
  END RunTql1;

PROCEDURE RunTql2(VAR d,e: REF ARRAY OF LONGREAL;
                  VAR z: REF ARRAY OF ARRAY OF LONGREAL) RAISES {}=
  BEGIN
    EigenSys.Tql2( d, e, z);
    Wr.PutText(Stdio.stdout, "Eigenwerte\n");
    FOR i:=FIRST(d^) TO LAST(d^) DO
      Wr.PutText( Stdio.stdout, Fmt.Pad(Fmt.LongReal( d[i],12),20) & "\n");
    END; (* for *)
  END RunTql2;

PROCEDURE RunTred1(a: REF ARRAY OF ARRAY OF LONGREAL;
                   dWR, eWR, e2WR: ARRAY OF LONGREAL) 
    RAISES {}=
  VAR
    z: REF ARRAY OF ARRAY OF LONGREAL;
    d, e, e2: REF ARRAY OF LONGREAL;
  BEGIN
    z := NEW( REF ARRAY OF ARRAY OF LONGREAL, NUMBER(a^), NUMBER(a[0]));
    d := NEW( REF ARRAY OF LONGREAL, NUMBER(a^));
    e := NEW( REF ARRAY OF LONGREAL, NUMBER(a^));
    e2:= NEW( REF ARRAY OF LONGREAL, NUMBER(a^));
    FOR i:=FIRST(z^) TO LAST(z^) DO
      FOR j:=FIRST(z[0]) TO LAST(z[0]) DO
        z[i,j] := a[i,j];
      END; (* for *)
    END; (* for *)
    EigenSys.Tred1( NUMBER(d^), z, d, e, e2);
    Print3( d, e, e2, dWR, eWR, e2WR);
    RunTql1( d,e);
  END RunTred1;

PROCEDURE RunTred2(a: REF ARRAY OF ARRAY OF LONGREAL; 
                   dWR, eWR: ARRAY OF LONGREAL;) RAISES {}=
  VAR
    aLocal: REF ARRAY OF ARRAY OF LONGREAL;
    d, e: REF ARRAY OF LONGREAL;
  BEGIN
    aLocal := NEW( REF ARRAY OF ARRAY OF LONGREAL, NUMBER(a^), NUMBER(a[0]));
    d := NEW( REF ARRAY OF LONGREAL, NUMBER(a^));
    e := NEW( REF ARRAY OF LONGREAL, NUMBER(a^));
    FOR i:=FIRST(a^) TO LAST(a^) DO
      FOR j:=FIRST(a[0]) TO LAST(a[0]) DO
        aLocal[i,j] := a[i,j];
      END; (* for *)
    END; (* for *)
    EigenSys.Tred2( NUMBER(d^), aLocal, d, e);
    dWR[1] := -dWR[1];
    Print2( d, e, dWR, eWR);
    dWR[1] := -dWR[1];
    Wr.PutText(Stdio.stdout, "Transformationsmatrix\n");
    FOR i:=FIRST(a^) TO LAST(a^) DO
      FOR j:=FIRST(a[0]) TO LAST(a[0]) DO
        Wr.PutText( Stdio.stdout, Fmt.Pad(Fmt.LongReal(aLocal[i,j],10),15));
      END; (* for *)
      Wr.PutText( Stdio.stdout, "\n");
    END; (* for *)
    RunTql2( d,e,aLocal);
  END RunTred2;



PROCEDURE RunTestA() RAISES {}=
  VAR
    a : REF ARRAY OF ARRAY OF LONGREAL;
    d,e,e2 : REF ARRAY OF LONGREAL;
    aWR := ARRAY[0..24] OF LONGREAL
             { 10.0D0,  1.0D0,  2.0D0,  3.0D0,  4.0D0,
                1.0D0,  9.0D0, -1.0D0,  2.0D0, -3.0D0,
                2.0D0, -1.0D0,  7.0D0,  3.0D0, -5.0D0,
                3.0D0,  2.0D0,  3.0D0, 12.0D0, -1.0D0,
                4.0D0, -3.0D0, -5.0D0, -1.0D0, 15.0D0}; 
    eWR := ARRAY [0..4] OF LONGREAL
        {0.0d0, 7.49484677741D-1, -4.49626820120D0, 
            -2.15704099085D0, 7.14142842854D0};
    dWR := ARRAY [0..4] OF LONGREAL
        {9.29520217754D0, 1.16267115569D1, 1.09604392078D1,
            6.11764705885D0, 1.5D1};
    e2WR := ARRAY [0..4] OF LONGREAL
        {0.0D0, 5.61727282169D-1, 2.02164277371D1,
             4.65282583621D0, 5.1D1};
  BEGIN
    a := NEW( REF ARRAY OF ARRAY OF LONGREAL, 5, 5);
    d := NEW( REF ARRAY OF LONGREAL, 5);
    e := NEW( REF ARRAY OF LONGREAL, 5);
    e2 := NEW( REF ARRAY OF LONGREAL, 5);

    FOR i:=FIRST(a^) TO LAST(a^) DO
      FOR j:=FIRST(a[0]) TO LAST(a[0]) DO
        a[i,j] := aWR[i*NUMBER(a[0])+j];
      END; (* for *)
    END; (* for *)

    Wr.PutText( Stdio.stdout, "Test: Tred1\n");
    FOR i:=FIRST(a^) TO LAST(a^) DO
      FOR j:=FIRST(a[0]) TO LAST(a[0]) DO
        Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.LongReal(a[i,j]),12));
      END; (* for *)
      Wr.PutText( Stdio.stdout, "\n");
    END; (* for *)
    RunTred1(a,dWR,eWR,e2WR);

    Wr.PutText( Stdio.stdout, "Test: Tred2\n");
    FOR i:=FIRST(a^) TO LAST(a^) DO
      FOR j:=FIRST(a[0]) TO LAST(a[0]) DO
        Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.LongReal(a[i,j]),12));
      END; (* for *)
      Wr.PutText( Stdio.stdout, "\n");
    END; (* for *)
    RunTred2(a,dWR,eWR);

  END RunTestA;


BEGIN
  RunTestA();
END Main.
