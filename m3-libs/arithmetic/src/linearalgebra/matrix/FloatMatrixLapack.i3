INTERFACE FloatMatrixLapack;
(*Copyright (c) 1996, m3na project

   Abstract: Low level interfaces to LAPACK
             Using them directly is very unsafe,
             please use the safe wrappers from
             EigenSystem

   1/1/96 <name> Initial version *)

(*==========================*)

IMPORT FloatBasic AS R;

(*
For more information on array layout in Modula 3
refer to the thread "Untraced references to open array types"
on the comp.lang.modula3 newsgroup
http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&threadm=61711f1e.0209141948.3a05f4a1%40posting.google.com&rnum=1&prev=/groups%3Fhl%3Den%26lr%3D%26ie%3DUTF-8%26selm%3D61711f1e.0209141948.3a05f4a1%2540posting.google.com
*)

TYPE SelectProc = PROCEDURE (x, y: R.T): BOOLEAN;

<*EXTERNAL xgees_*>
PROCEDURE GEES (READONLY JOBVS : CHAR;
                READONLY SORT  : CHAR;
                         SELECT: SelectProc;
                READONLY N     : INTEGER;
                READONLY A     : (*ARRAY OF ARRAY OF*) R.T;
                READONLY LDA   : INTEGER;
                VAR      SDIM  : INTEGER;
                VAR      WR    : (*ARRAY OF*) R.T;
                VAR      WI    : (*ARRAY OF*) R.T;
                VAR      VS    : (*ARRAY OF ARRAY OF*) R.T;
                READONLY LDVS  : INTEGER;
                VAR      WORK  : (*ARRAY OF*) R.T;
                READONLY LWORK : INTEGER;
                VAR      BWORK : (*ARRAY OF*) BOOLEAN;
                VAR      INFO  : INTEGER                     );

<*EXTERNAL xgees_*>
PROCEDURE GEES_ADDR (READONLY JOBVS : CHAR;
                READONLY SORT  : CHAR;
                         SELECT: SelectProc;
                READONLY N     : INTEGER;
                         A     : ADDRESS; (*  READONLY A     : ARRAY OF ARRAY OF R.T;*)
                READONLY LDA   : INTEGER;
                VAR      SDIM  : INTEGER;
                         WR    : ADDRESS; (*  VAR      WR    : ARRAY OF R.T;*)
                         WI    : ADDRESS; (*  VAR      WI    : ARRAY OF R.T;*)
                         VS    : ADDRESS; (*  VAR      VS    : ARRAY OF ARRAY OF R.T;*)
                READONLY LDVS  : INTEGER;
                         WORK  : ADDRESS; (*  VAR      WORK  : ARRAY OF R.T;*)
                READONLY LWORK : INTEGER;
                         BWORK : ADDRESS; (*  VAR      BWORK : ARRAY OF BOOLEAN;*)
                VAR      INFO  : INTEGER                     );

<*EXTERNAL xlamch_*>
PROCEDURE GetMachineParameter(READONLY cmach : CHAR):LONGREAL;

(*==========================*)
END FloatMatrixLapack.
