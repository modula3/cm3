(* $Id$ *)
UNSAFE INTERFACE UtimeWrap;
IMPORT UtimeOpsC;

TYPE T = UtimeOpsC.T;

PROCEDURE make_T() : T;

PROCEDURE delete_T(t : T);

END UtimeWrap.
