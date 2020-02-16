INTERFACE CboolImpl;
FROM Ctypes IMPORT int;
IMPORT Cbool;

TYPE
  T = Cbool.T;
  t = Cbool.t;

(* This is a private interface to the internals of Rajit's bool_t *)
(* If you are using the functions here, you better know what you're doing! *)

<*EXTERNAL bool_r*>
PROCEDURE get_r(b : t) : t;

<*EXTERNAL bool_l*>
PROCEDURE get_l(b : t) : t;

<*EXTERNAL bool_isleaf*>
PROCEDURE isleaf(b : t) : int;

<*EXTERNAL bool_node_var*>
PROCEDURE node_var(B : T; b : t) : t;

<*EXTERNAL bool_copy*>
PROCEDURE copy(B : T; b : t) : t;

END CboolImpl.
