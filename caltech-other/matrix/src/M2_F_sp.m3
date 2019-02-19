UNSAFE MODULE M2_F_sp;
IMPORT MatrixF;
IMPORT RMatrix2 AS M2;

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V) =
  VAR cols := NUMBER(a[0]);
      rows := NUMBER(a);
  BEGIN
    <* ASSERT cols = NUMBER(b) *>
    <* ASSERT rows = NUMBER(prod) *>
    MatrixF.mulmv_sp_(ADR(a[0,0]),ADR(b[0]),ADR(prod[0]),ADR(rows),ADR(cols))
  END MulMV;

PROCEDURE MulMC(READONLY a : M2.M; READONLY b : M2.M; VAR prod : M2.V) =
  VAR cols := NUMBER(a[0]);
      rows := NUMBER(a);
  BEGIN
    <* ASSERT cols = NUMBER(b) *>
    <* ASSERT rows = NUMBER(prod) *>
    <* ASSERT NUMBER(b[0]) = 1 *>
    MatrixF.mulmv_sp_(ADR(a[0,0]),ADR(b[0,0]),ADR(prod[0]),ADR(rows),ADR(cols))
  END MulMC;

PROCEDURE MulMVC(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.M) =
  VAR cols := NUMBER(a[0]);
      rows := NUMBER(a);
  BEGIN
    <* ASSERT cols = NUMBER(b) *>
    <* ASSERT rows = NUMBER(prod) *>
    <* ASSERT NUMBER(prod[0]) = 1 *>
    MatrixF.mulmv_sp_(ADR(a[0,0]),ADR(b[0]),ADR(prod[0,0]),ADR(rows),ADR(cols))
  END MulMVC;

PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M) =
  VAR
    aDim := M2.GetDim(a);
    bDim := M2.GetDim(b);
    cDim := M2.GetDim(prod);
  BEGIN
    <* ASSERT aDim.rows = bDim.rows *>
    <* ASSERT bDim.cols = cDim.cols *>
    <* ASSERT aDim.cols = cDim.rows *>
    MatrixF.mul_mtransposem_sp_(ADR(a[0,0]),ADR(b[0,0]),ADR(prod[0,0]),
                                ADR(aDim.rows), ADR(aDim.cols), ADR(bDim.cols))
  END MulTransposeMM;

PROCEDURE IndexedDot(READONLY v : M2.V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : M2.V) : M2.Base =
  VAR
    n := NUMBER(idx);
    res : M2.Base;
  BEGIN
    <* ASSERT n = NUMBER(w) *>
    MatrixF.indexeddot_sp_(ADR(v[0]),ADR(idx[0]),ADR(n),ADR(w[0]),ADR(res));
    RETURN res
  END IndexedDot;

PROCEDURE Delta(READONLY v : M2.V; VAR d : M2.V) =
  VAR
    n := NUMBER(d);
  BEGIN
    <* ASSERT NUMBER(v) - 1 = n *>
    MatrixF.delta_sp_(ADR(v[0]),ADR(d[0]),ADR(n))
  END Delta;

BEGIN END M2_F_sp.
