UNSAFE MODULE NewUOA_M3;
IMPORT LRVector, LRScalarField;
IMPORT NewUOA;
IMPORT Word;

CONST Tag = 16_c0edbabe;
      
TYPE
  T = RECORD
    tag  : Word.T;
    func : LRScalarField.T;
    x    : LRVector.T;
  END;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   npt            : CARDINAL;
                   rhobeg, rhoend : LONGREAL;
                   maxfun         : CARDINAL) : LONGREAL =

  VAR
    N, NPT         : ADDRESS;  n      : INTEGER;
    X              : ADDRESS;  
    RHOBEG, RHOEND : ADDRESS;
    IPRINT         : ADDRESS;  iprint : INTEGER;
    MAXFUN         : ADDRESS;  
    W              : ADDRESS;  w      : REF ARRAY OF LONGREAL;
    CALFUN         : NewUOA.Func;
    F              : ADDRESS;  f      : LONGREAL;
    INFO           : ADDRESS;  info   : INTEGER;
    ITAG           : ADDRESS;  t      : T;
  BEGIN
    n      := NUMBER(p^);
    <*ASSERT npt >= n+2*>
    <*ASSERT npt <= ((n+1)*(n+2)) DIV 2 *>
    iprint := 0;
    w      := NEW(REF ARRAY OF LONGREAL, (npt+13)*(npt+n)+(3*n*(n+3)) DIV 2);
    t      := T { tag := Tag, func := func, x := p };
    
    N      := ADR(n);
    NPT    := ADR(npt);
    X      := ADR(p[0]);
    RHOBEG := ADR(rhobeg);
    RHOEND := ADR(rhoend);
    IPRINT := ADR(iprint);
    MAXFUN := ADR(maxfun);
    W      := ADR(w[0]);
    CALFUN := Wrapper;
    F      := ADR(f);
    INFO   := ADR(info);
    ITAG   := ADR(t);

    VAR u := LOOPHOLE(ITAG, REF T);
    BEGIN
      <*ASSERT u.tag = Tag *>
    END;
      
    NewUOA.Call(N, NPT,
                X,
                RHOBEG, RHOEND,
                IPRINT,
                MAXFUN, W,
                CALFUN,
                F,
                INFO,
                ITAG);
    RETURN f
  END Minimize;

PROCEDURE Wrapper(N : ADDRESS; X : ADDRESS; Itag : ADDRESS) : LONGREAL =
  VAR
    t := LOOPHOLE(Itag, REF T);
    n := LOOPHOLE(N, REF INTEGER)^;
  BEGIN
    (* make sure we passed through the Fortran OK *)
    <*ASSERT t.tag = Tag*>
    <*ASSERT X = ADR(t.x[0])*>
    <*ASSERT n = NUMBER(t.x^)*>
    RETURN t.func.eval(t.x)
  END Wrapper;

BEGIN END NewUOA_M3.
