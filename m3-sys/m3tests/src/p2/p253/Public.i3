INTERFACE Public;
IMPORT Ctypes;

TYPE Private <: ROOT;

TYPE T = Private OBJECT
    a := 1;
    c := 3;
METHODS
    F1() := F1;
END;

<*EXTERNAL put_adr*> PROCEDURE PutI(t:Ctypes.char_star; VAR a:INTEGER);
<*EXTERNAL put_adr*> PROCEDURE PutT(t:Ctypes.char_star; a:T);

PROCEDURE F1(a:T);

END Public.
