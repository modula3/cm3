(* $Id$ *)

UNSAFE INTERFACE MatrixF;

<*EXTERNAL*>
PROCEDURE muld_(amatrix, bmatrix, prod, aRows, aCols, bCols : ADDRESS);

<*EXTERNAL*>
PROCEDURE mulmv_sp_(a, v, r, rows, cols : ADDRESS);

<*EXTERNAL*>
PROCEDURE lu2_backsubstitute_sp_(m, indx, b, n : ADDRESS);

<*EXTERNAL*>
PROCEDURE mul_mtransposem_sp_(a, b, c, arows, acols, bcols : ADDRESS);

<*EXTERNAL*>
PROCEDURE mulmv_dp_(a, v, r, rows, cols : ADDRESS);

<*EXTERNAL*>
PROCEDURE lu2_backsubstitute_dp_(m, indx, b, n : ADDRESS);

<*EXTERNAL*>
PROCEDURE mul_mtransposem_dp_(a, b, c, arows, acols, bcols : ADDRESS);

<*EXTERNAL*>
PROCEDURE indexeddot_sp_(v, idx, n, w, res : ADDRESS);

<*EXTERNAL*>
PROCEDURE indexeddot_dp_(v, idx, n, w, res : ADDRESS);

<*EXTERNAL*>
PROCEDURE delta_sp_(v, d, n : ADDRESS);

<*EXTERNAL*>
PROCEDURE delta_dp_(v, d, n : ADDRESS);

END MatrixF.
