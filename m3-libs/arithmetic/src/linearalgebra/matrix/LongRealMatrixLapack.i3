INTERFACE LongRealMatrixLapack;
(*Copyright (c) 1996, m3na project

   Abstract: Low level interfaces to LAPACK

   1/1/96 <name> Initial version *)

(*==========================*)

IMPORT LongRealBasic AS R;

(*
http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&threadm=61711f1e.0209151913.10beb5ae%40posting.google.com&rnum=1&prev=/groups%3Fq%3Darray%2Blayout%2Bgroup:comp.lang.modula3%2Bgroup:comp.lang.modula3%26hl%3Den%26lr%3D%26ie%3DUTF-8%26group%3Dcomp.lang.modula3%26selm%3D61711f1e.0209151913.10beb5ae%2540posting.google.com%26rnum%3D1
*)

TYPE SelectProc = PROCEDURE (x, y: R.T): BOOLEAN;

<*EXTERNAL dgees_*>
PROCEDURE GEES (READONLY JOBVS : CHAR;
                READONLY SORT  : CHAR;
                         SELECT: SelectProc;
                READONLY N     : INTEGER;
(*                READONLY A     : ARRAY OF ARRAY OF R.T;*)
                         A     : ADDRESS;
                READONLY LDA   : INTEGER;
                VAR      SDIM  : INTEGER;
                VAR      WR    : ARRAY OF R.T;
                VAR      WI    : ARRAY OF R.T;
                VAR      VS    : ARRAY OF ARRAY OF R.T;
                READONLY LDVS  : INTEGER;
                VAR      WORK  : ARRAY OF R.T;
                READONLY LWORK : INTEGER;
                VAR      BWORK : ARRAY OF BOOLEAN;
                VAR      INFO  : INTEGER                     );

(*==========================*)
END LongRealMatrixLapack.
