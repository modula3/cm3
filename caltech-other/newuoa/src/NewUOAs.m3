MODULE NewUOAs;
IMPORT LRVector, LRScalarField;
IMPORT LRMatrix2 AS LRMat; IMPORT Matrix AS Mat;
FROM LRMatrix2 IMPORT NewM, NewV, AddV, MulMV, V, M, MulSV, AddSV, SubV,
                      Copy, ExtractColAsVector, SetCol, MulMM,
                      FormatM, FormatV;
FROM Matrix IMPORT Dim;
FROM Math IMPORT sqrt, pow;
IMPORT Debug;
IMPORT LongRealSeq AS LRSeq;
IMPORT NewUOA_M3;
FROM Fmt IMPORT Int, LongReal, F;

CONST Eps = 2.2204d-16;
CONST Inf = LAST(LONGREAL); (* not exactly right *)
CONST doDebug = FALSE;

TYPE LR = LONGREAL;

TYPE
  SubspaceOutput = RECORD
    B        : REF M;
    xnew     : REF V;
    fnew     : LONGREAL;
    g        : REF V;
    nf       : CARDINAL;
    fhist    : FHist;
  END;

TYPE
  FHist = LRSeq.T OBJECT METHODS
    append(s : REFANY) := FHA;
  END;

PROCEDURE FHA(h : FHist; arg : REFANY) = 
  BEGIN
    TYPECASE arg OF
      LRSeq.T(seq) =>
      FOR i := 0 TO seq.size()-1 DO
        h.addhi(seq.get(i))
      END
    |
      REF V(v) =>
      FOR i := FIRST(v^) TO LAST(v^) DO
        h.addhi(v[i])
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END FHA;

PROCEDURE Size(m : REFANY; idx :[0..1]:= 0) : CARDINAL =
  BEGIN
    TYPECASE m OF
      LRVector.T(v) =>
      <*ASSERT idx=0*>
      RETURN NUMBER(v^)
    |
      REF LRMat.M(z) =>
      CASE idx OF
        0 => RETURN NUMBER(z^)
      |
        1 => IF NUMBER(z^) = 0 THEN RETURN 0 ELSE RETURN NUMBER(z[0]) END
      ELSE
        <*ASSERT FALSE*>
      END
    ELSE <*ASSERT FALSE*>
    END
  END Size;

PROCEDURE RowMatrix(READONLY v : V) : REF M =
  VAR
    res := NEW(REF M, 1, NUMBER(v));
  BEGIN
    FOR i := FIRST(v) TO LAST(v) DO
      res[0, i] := v[i]
    END;
    RETURN res
  END RowMatrix;
     
PROCEDURE ColMatrix(READONLY v : V) : REF M =
  VAR
    res := NEW(REF M, NUMBER(v), 1);
  BEGIN
    FOR i := FIRST(v) TO LAST(v) DO
      res[i, 0] := v[i]
    END;
    RETURN res
  END ColMatrix;
     
PROCEDURE Norm(m : REFANY; b := 2.0d0) : LONGREAL =

  PROCEDURE Elem(q : LONGREAL) =
    BEGIN
      IF    b = 2.0d0 THEN
        s := s + q*q
      ELSIF b = Inf THEN
        s := MAX(s, ABS(q))
      END
    END Elem;

  VAR
    s := 0.0d0;
  BEGIN
    TYPECASE m OF
      LRVector.T(v) =>
      FOR i := FIRST(v^) TO LAST(v^) DO Elem(v[i]) END
    |
      REF LRMat.M(z) =>
      FOR i := FIRST(z^) TO LAST(z^) DO
        FOR j := FIRST(z[0]) TO LAST(z[0]) DO
          Elem(z[i,j])
        END
      END
    ELSE <*ASSERT FALSE*>
    END;

    IF    b = 2.0d0 THEN
      RETURN sqrt(s)
    ELSIF b = Inf THEN
      RETURN s
    ELSE
      <*ASSERT FALSE*>
    END
  END Norm;

PROCEDURE Nnz(m : REFANY) : CARDINAL =
  VAR
    cnt := 0;
  BEGIN
    TYPECASE m OF
      LRVector.T(v) =>
      FOR i := FIRST(v^) TO LAST(v^) DO
        IF v[i] # 0.0d0 THEN INC(cnt) END
      END
    |
      Mat.T(z) =>
      FOR i := FIRST(z^) TO LAST(z^) DO
        FOR j := FIRST(z[0]) TO LAST(z[0]) DO
          IF z[i,j] # 0.0d0 THEN INC(cnt) END
        END
      END
    ELSE <*ASSERT FALSE*>
    END;
    RETURN cnt
  END Nnz;

PROCEDURE CopyV(v : LRVector.T) : LRVector.T =
  BEGIN
    WITH res = NewV(NUMBER(v^)) DO
      res^ := v^;
      RETURN res
    END
  END CopyV;

PROCEDURE ZeroV(dim : CARDINAL) : LRVector.T =
  VAR
    res := NEW(LRVector.T, dim);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO res[i] := 0.0d0 END;
    RETURN res
  END ZeroV;

PROCEDURE Columns(READONLY m : M; start, cols : CARDINAL) : REF M =
  VAR
    res := NEW(REF M, NUMBER(m), cols);
  BEGIN
    FOR c := start TO start+cols-1 DO
      FOR r := 0 TO NUMBER(m)-1 DO
        res[r, c-start] := m[r,c]
      END
    END;
    RETURN res
  END Columns;

PROCEDURE AddToCols(VAR m : M; start, n : CARDINAL; READONLY what : M) =
  BEGIN
    FOR r := FIRST(what) TO LAST(what) DO
      FOR c := 0 TO n-1 DO
        m[r, c+start] := m[r, c+start] + what[r, c]
      END
    END
  END AddToCols;
  
PROCEDURE NaN(sz0 : CARDINAL; sz1 : [-1..LAST(CARDINAL)] := -1) : REFANY =

  PROCEDURE InitNaN() =
    BEGIN
      TYPECASE res OF
        LRVector.T(v) =>
        FOR i := FIRST(v^) TO LAST(v^) DO
          v[i]  := 0.0d0/0.0d0
        END
      |
        Mat.T(z) =>
        FOR i := FIRST(z^) TO LAST(z^) DO
          FOR j := FIRST(z[0]) TO LAST(z[0]) DO
            z[i,j] := 0.0d0/0.0d0
          END
        END
      ELSE <*ASSERT FALSE*>
      END
    END InitNaN;

  VAR
    res : REFANY;
  BEGIN
    IF sz1 = -1 THEN
      res := NewV(sz0)
    ELSE
      res := NewM(Dim { sz0, sz1 })
    END;
    InitNaN();
    RETURN res
  END NaN;

PROCEDURE AppendCols(a, b : REF V; c : REF M) : REF M =
  VAR
    w := 0;
    res : REF M;
    h : CARDINAL;
  BEGIN
    IF a # NIL THEN h := NUMBER(a^); INC(w) END;
    IF b # NIL THEN h := NUMBER(b^); INC(w) END;
    IF c # NIL THEN h := NUMBER(c^); INC(w, NUMBER(c[0])) END;

    res := NewM( Dim { h, w });
    w := 0;
    IF a # NIL THEN
      FOR r := 0 TO h-1 DO res[r,w] := a[r] END; INC(w)
    END;
    IF b # NIL THEN
      FOR r := 0 TO h-1 DO res[r,w] := b[r] END; INC(w)
    END;
    IF c # NIL THEN
      FOR j := 0 TO Size(c,1)-1 DO
        FOR r := 0 TO h-1 DO res[r, w+j] := c[r, j] END
      END
    END;

    RETURN res
  END AppendCols;

PROCEDURE AppendCols2(c : REF M; a : REF V) : REF M =
  VAR
    w := 0;
    res : REF M;
    h : CARDINAL;
  BEGIN
    IF c # NIL THEN h := Size(c, 0); INC(w, Size(c,1)) END;
    IF a # NIL THEN h := NUMBER(a^); INC(w) END;

    res := NewM( Dim { h, w });
    w := 0;
    IF c # NIL THEN
      FOR j := 0 TO Size(c,1)-1 DO
        FOR r := 0 TO h-1 DO res[r, w+j] := c[r, j] END
      END
    END;
    IF a # NIL THEN
      FOR r := 0 TO h-1 DO res[r,w] := a[r] END; INC(w)
    END;

    RETURN res
  END AppendCols2;

PROCEDURE CutbackCols(x : REF M; cols : CARDINAL) : REF M =
  VAR
    res := NewM(Dim { NUMBER(x^), cols });
  BEGIN
    FOR r := 0 TO NUMBER(x^)-1 DO
      FOR c := 0 TO cols-1 DO
        res[r,c] := x[r,c]
      END
    END;
    RETURN res
  END CutbackCols;
  
  (**********************************************************************)

TYPE
  NewuoaOptions = RECORD
    npt : CARDINAL;
    maxfun : CARDINAL;
    rhoend, rhobeg : LONGREAL;
  END;

  NewuoaResult = RECORD
    dopt      : LRVector.T;
    f         : LONGREAL;
    funcCount : CARDINAL;
    fhist     : FHist;
  END;

  Subfun = LRScalarField.T OBJECT
    stats : NewuoaResult;
    f     : LRScalarField.T;
    x     : LRVector.T;
    B     : REF LRMat.M;
  OVERRIDES
    eval := EvalSubfun;
  END;

PROCEDURE EvalSubfun(sf : Subfun; d : LRVector.T) : LONGREAL =
  VAR
    n := Size(sf.B, 0);
    q := ZeroV(n);
    v := ZeroV(n);
  BEGIN
    IF doDebug THEN
      Debug.Out(F("EvalSubfun: sf.B=\n%s\nd=\n%s", FormatM(sf.B^), FormatV(d^)))
    END;
    MulMV(sf.B^,d^,q^);
    AddV(sf.x^,q^,v^);
    WITH res = sf.f.eval(v) DO
      INC(sf.stats.funcCount);
      sf.stats.fhist.addhi(res);
      RETURN res
    END
  END EvalSubfun;
  
PROCEDURE DoNewuoa(v       : LRVector.T;
                   subfun  : Subfun;
                   options : NewuoaOptions) : NewuoaResult =
  VAR
    res := NewuoaResult { dopt      := v,
                          f         := FIRST(LONGREAL),
                          funcCount := 0,
                          fhist     := NEW(FHist).init() };
  BEGIN
    subfun.stats := res;
    res.f := NewUOA_M3.Minimize(v,
                                subfun,
                                options.npt,
                                options.rhobeg,
                                options.rhoend,
                                options.maxfun);
    RETURN res
  END DoNewuoa;
  
  (**********************************************************************)
  
PROCEDURE Minimize(x0             : LRVector.T;
                   fun            : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   ftarget     := FIRST(LONGREAL)) : Output =
  VAR
    n := NUMBER(x0^);
    maxsubspacedim := MIN(3, n);
    maxfun := 100*n;
    maxiter := 50;
    x := CopyV(x0);
    fx := fun.eval(x);
    nf := 1;
    fhist : FHist := NEW(FHist).init();
    output : Output;
    h, normd : LONGREAL;
    D, B : REF M;
    g : REF V;
    halt : BOOLEAN;
    npt : CARDINAL;
    smalld : CARDINAL;
    dim : CARDINAL;
    submaxfun : CARDINAL;
  BEGIN
    fhist.addhi(fx);
    IF doDebug THEN
      Debug.Out(F("A. fx=%s ftarget=%s", LongReal(fx), LongReal(ftarget)))
    END;
    IF fx <= ftarget THEN
      output.f := fx;
      output.x := x;
      output.message := F("target function value achieved %s <= %s",
                                LongReal(fx), LongReal(ftarget));
      output.iterations := 0;
      maxiter := 0
    END;

    h := rhobeg;
    normd := rhobeg;

    D := NewM(Dim {n,0});
    smalld := 0;

    halt := FALSE;
    FOR iter := 1 TO maxiter DO
      LOOP
        dim := MIN(Size(D,1)+2, maxsubspacedim);
        IF dim >= 5 THEN
          npt := 2*dim + 1
        ELSE
          npt := ((dim+1)*(dim+2)) DIV 2
        END;
        IF nf >= maxfun - npt - 6 OR
           nf >= maxfun - 2*n - npt -4 AND n >= 5000 THEN
          output.f := fx;
          output.x := x;
          output.message := "NEWUOAs terminates because the maximal number of function evaluation is (nearly) reached.";
          halt := TRUE;
          EXIT
        END;
        submaxfun := MIN(2*n, maxfun - nf - (npt+5));

        WITH subres = DefSubspace(fun, x, fx, h, rhoend, submaxfun, D) DO
          dim := Size(subres.B, 1);
          B := subres.B;
          g := subres.g;
          IF (dim > 0 AND Nnz(subres.B) = 0) OR
             (dim = 0 AND Norm(subres.g) > 1.0d-3 * h) OR
              dim > maxsubspacedim  THEN
            Debug.Error("DefSubspace returns wrong subspace dim")
          END;
          IF dim = 0 THEN
            Debug.Warning("DefSubspace returns an empty basis")
          END;
          x := subres.xnew;
          fx := subres.fnew;
          nf := nf + subres.nf;
          fhist.append(subres.fhist);
          IF doDebug THEN
            Debug.Out(F("B. fx=%s ftarget=%s", LongReal(fx), LongReal(ftarget)))
          END;
          IF fx <= ftarget THEN
            output.f := fx;
            output.x := x;
            output.message := F("target function value achieved %s <= %s",
                                LongReal(fx), LongReal(ftarget));
            halt := TRUE;
            EXIT
          END;
          IF dim > 0 AND
            Norm(subres.g)* sqrt(FLOAT(2*n,LR)/FLOAT(MIN(2*n, submaxfun),LR)) > rhoend
           THEN
            
            EXIT
          ELSIF h <= rhoend THEN
            output.f := fx;
            output.x := x;
            output.message := "NEWUOAs terminates because the first-order optimality is approximately achieved.";
            halt := TRUE;
            EXIT
          ELSE
            h := h/2.0d0
          END
        END
      END;

      IF halt THEN EXIT END;

      VAR
        newuoa_options : NewuoaOptions;
        dx : LRVector.T;
      BEGIN
        IF dim <= 5 THEN
          newuoa_options.npt := ((dim+1)*(dim+2)) DIV 2
        ELSE
          newuoa_options.npt := 2*dim + 1
        END;
        newuoa_options.maxfun := MIN(500*dim, maxfun-nf);
        WITH min = MIN(rhoend,1.0d0/pow(2.0d0,FLOAT(iter,LR))) DO
          newuoa_options.rhoend := MAX(Eps, 
                                       MAX(min,
                                           rhoend/FLOAT(MAX(n,50),LR)));
        END;
        newuoa_options.rhobeg := MAX(newuoa_options.rhoend,
                                     MAX(h,
                                         MAX(normd,
                                             0.5d0*newuoa_options.rhobeg)));
        WITH subfun    = NEW(Subfun, f := fun, x := x, B := B),
             newuoaRes = DoNewuoa(ZeroV(dim), subfun, newuoa_options),
             dopt      = newuoaRes.dopt,
             f         = newuoaRes.f,
             normd     = Norm(dopt) DO
          IF f > fx OR normd > 0.0d0 AND f = fx THEN
            Debug.Error("NewUOAs:NewUOA failed to solve the subproblem")
          END;
          IF normd > 0.0d0 THEN
            dx := ZeroV(n);
            MulMV(B^, dopt^, dx^);
            AddV(x^,dx^,x^);
            fx := f
          END;
          nf := nf + newuoaRes.funcCount;
          fhist.append(newuoaRes.fhist);
          IF doDebug THEN
            Debug.Out(F("C. fx=%s ftarget=%s", LongReal(fx), LongReal(ftarget)))
          END;
          IF fx <= ftarget THEN
            output.f := fx;
            output.x := x;
            output.message := F("target function value achieved %s <= %s",
                                LongReal(fx), LongReal(ftarget));
            EXIT
          END;
          IF normd <= 0.1d0 * rhoend THEN
            smalld := smalld + 1
          ELSE
            smalld := 0
          END;
          IF doDebug THEN
            Debug.Out(F("NewUOAs normd=%s rhoend=%s smalld=%s h=%s",
                        LongReal(normd), LongReal(rhoend), Int(smalld),
                        LongReal(h)))
          END;
          (*
          IF smalld >= 3 THEN
            output.f := fx;
            output.x := x;
            output.message := "NEWUOAs terminates because the stepsize is small for 3 consecutive iterations.";
            EXIT
          END;
          *)
          
          VAR
            oldh := h;
          BEGIN
            h := MAX(h/2.0d0,
                     MAX(rhoend/FLOAT(MAX(n,50),LR),
                         sqrt(Eps)));
            IF h = oldh AND smalld > 0 THEN
              output.f := fx;
              output.x := x;
              output.message := "NEWUOAs terminates because we achieved the minimum stepsize";
              EXIT
            END
          END;
            
          IF normd <= 0.1d0 * rhoend THEN
            dx := ZeroV(n)
          END;
          
          IF maxsubspacedim = 3 THEN
            D := AppendCols(dx, NIL, NIL)
          ELSE
            WITH neg = ZeroV(dim) DO
              MulSV(-1.0d0, g^, neg^);
              D := AppendCols(dx,
                              neg,
                              CutbackCols(D, MIN(Size(D,1), maxsubspacedim-4)))
            END
          END
        END
      END
    END(*FOR i*);

    output.funcCount := nf;
    output.fhist := fhist;
    RETURN output
  END Minimize;

TYPE
  MinIndT = RECORD
    min : LONGREAL;
    idx : CARDINAL;
  END;
  
PROCEDURE MinInd2(READONLY a, b : V) : MinIndT =
  VAR
    res := MinIndT { min := LAST(LONGREAL), idx := LAST(CARDINAL) };
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] < res.min THEN res.min := a[i]; res.idx := i END
    END;
    FOR i := FIRST(b) TO LAST(b) DO
      IF b[i] < res.min THEN res.min := b[i]; res.idx := i + NUMBER(a) END
    END;
    RETURN res
  END MinInd2;

PROCEDURE AddToRow(READONLY m : M; r : CARDINAL; v : LONGREAL) : REF M =
  VAR
    res := NEW(REF M, NUMBER(m), NUMBER(m[0]));
  BEGIN
    Copy(m, res^);
    FOR c := FIRST(m[0]) TO LAST(m[0]) DO
      res[r,c] := res[r,c] + v
    END;
    RETURN res
  END AddToRow;
  
PROCEDURE DefSubspace(f       : LRScalarField.T;
                      x       : LRVector.T;
                      fx      : LONGREAL;
                      h       : LONGREAL;
                      <*UNUSED*>rhoend  : LONGREAL;
                      maxfun  : CARDINAL;
                      D       : Mat.T) : SubspaceOutput =
  VAR
    o : SubspaceOutput;
    n := NUMBER(x^);
    H : REF V := NaN(n);
    fp, fn : REF V;
    pg : REF V;
    X : REF M;

  BEGIN
    IF doDebug THEN
      Debug.Out("DefSubspace : h=" & LongReal(h))
    END;
    o.g := NaN(n);

    o.fhist := NEW(FHist).init();
    
    IF TRUE (* maxfun >= 2*n *) THEN
      fp := NaN(n);
      fn := NaN(n);
      IF doDebug THEN
        Debug.Out("NewUOAs.DefSubspace : launching hints")
      END;
      FOR i := 0 TO n-1 DO
        VAR xtmp := CopyV(x); BEGIN
          xtmp[i] := x[i] + h;
          IF doDebug THEN Debug.Out("hint at " & FormatV(xtmp^)) END;
          f.evalHint(xtmp);
          xtmp[i] := x[i] - h;
          IF doDebug THEN Debug.Out("hint at " & FormatV(xtmp^)) END;
          f.evalHint(xtmp);
        END
      END;
      IF doDebug THEN
        Debug.Out("NewUOAs.DefSubspace : done launching hints")
      END;
      FOR i := 0 TO n-1 DO
        VAR xtmp := CopyV(x); BEGIN
          xtmp[i] := x[i] + h;
          fp[i] := f.eval(xtmp);
          xtmp[i] := x[i] - h;
          fn[i] := f.eval(xtmp);
        END
      END;
      o.nf := 2*n;

      VAR
        fpMfn : REF V := NaN(n);
        fpPfn : REF V := NaN(n);
        t1    : REF V := NaN(n);
      BEGIN
        SubV(fp^, fn^, fpMfn^);
        AddV(fp^, fn^, fpPfn^);
        MulSV(0.5d0*h, fpMfn^, o.g^);
        AddSV(-2.0d0*fx, fpPfn^, t1^);
        MulSV(1.0d0/h/h, t1^, H^)
      END;

      WITH minRes = MinInd2(fp^, fn^),
           ind    = minRes.idx DO

        o.fnew   := minRes.min;

        <*ASSERT minRes.idx < 2*n*>

        IF doDebug THEN Debug.Out(F("D=\n%s",FormatM(D^))) END;
        IF o.fnew < fx THEN
          IF ind < n THEN
            o.xnew := CopyV(x);
            o.xnew[ind] := x[ind] + h;
            o.g[ind] := o.g[ind] + h*H[ind];
            IF Nnz(D) > 0 THEN
              D := AddToRow(D^, ind, h)
            END
          ELSE
            o.xnew := CopyV(x);
            o.xnew[ind-n] := x[ind-n] - h;
            o.g[ind-n] := o.g[ind-n] - h*H[ind-n];
            IF Nnz(D) > 0 THEN
              D := AddToRow(D^, ind-n, h)
            END
          END
        ELSE
          o.fnew := fx;
          o.xnew := x
        END;
      END;
    END;
    pg := NaN(Size(o.g));
    SetPg(pg^, o.g^, H^);
    o.fhist.append(fp); o.fhist.append(fn);

    VAR
      gPpg   : REF V := NaN(Size(o.g));
      NgPpg  : REF V := NaN(Size(o.g));
    BEGIN
      AddV(o.g^,pg^,gPpg^);
      MulSV(-1.0d0, gPpg^, NgPpg^);
      IF doDebug THEN
        Debug.Out(F("D=\n%sNgPpg=\n%s", FormatM(D^), FormatV(NgPpg^)))
      END;
      X := AppendCols2(D, NgPpg)
    END;

    VAR
      dimtmp := Size(X,1);
      dim := 0;
      d : REF V := NaN(Size(X));
      normd := 0.0d0/0.0d0;
    BEGIN
      FOR i := 0 TO dimtmp-1 DO
        ExtractColAsVector(X^, i, d^);

        IF Norm(d) > 1.0d-3*h THEN
          MulSV(1.0d0/Norm(d, Inf),d^,d^);
          SetCol(X^, dim, d^);
          dim := dim + 1
        END
      END;
      X := Columns(X^, 0, dim);
      dimtmp := dim;
      dim := 0;
      o.B := NaN(Size(X,0),Size(X,1));
      <*ASSERT dimtmp = Size(X,1)*> (* dimtmp is # of cols of X *)
      FOR i := 0 TO dimtmp-1 DO
        (* extract ith col of X into d*)
        ExtractColAsVector(X^, i, d^);

        (* calc its norm *)
        normd := Norm(d);
        IF normd > 1.0d-8 THEN
          (* normalize significant column *)
          MulSV(1.0d0/normd, d^, d^);

          (* copy it to B *)
          SetCol(o.B^, dim, d^);
          IF doDebug THEN
            Debug.Out(F("i=%s dimtmp=%s X=\n%s", Int(i), Int(dimtmp),
                        FormatM(X^)))
          END;
          WITH dM = ColMatrix(d^),
               dT = RowMatrix(d^),
               sX = Columns(X^, i+1, dimtmp-(i+1)),
               (* what = -1 d dT sX 
                  size of what is h = height of d, w = width of sX *)
               dsq = NewM(Dim { Size(d), Size(d) } ),
               what = NewM(Dim { Size(d), dimtmp } ) DO
            MulMM(dM^, dT^, dsq^);
            MulMM(dsq^, sX^, what^);
            
            AddToCols(X^, i+1, dimtmp-(i+1), what^)
          END;
          INC(dim)
        END
      END;
      o.B := Columns(o.B^, 0, dim);
    END;
    RETURN o
  END DefSubspace;

PROCEDURE SetPg(VAR pg : V; READONLY g, H : V) =
  VAR
    threshold := MAX(Eps, 1.0d-6*MIN(1.0d0, MaxAbs(H)));
  BEGIN
    FOR i := FIRST(H) TO LAST(H) DO
      IF H[i] >= threshold THEN
        pg[i] := g[i]/H[i]
      ELSE
        pg[i] := g[i] * (-H[i]/threshold/threshold + 2.0d0/threshold)
      END
    END
  END SetPg;

PROCEDURE MaxAbs(READONLY v : V) : LONGREAL =
  VAR
    x := 0.0d0;
  BEGIN
    FOR i := FIRST(v) TO LAST(v) DO
      x := MAX(x, ABS(v[i]))
    END;
    RETURN x
  END MaxAbs;
  
BEGIN END NewUOAs.
