INTERFACE MathPosixC;
FROM Ctypes IMPORT int;

<*EXTERNAL "MathPosixC__frexp_result_glue"*>
PROCEDURE frexp_result_glue(x : LONGREAL) : LONGREAL;

<*EXTERNAL "MathPosixC__frexp_exp_glue"*>
PROCEDURE frexp_exp_glue(x : LONGREAL) : int;

<*EXTERNAL "MathPosixC__cabs_glue"*>
PROCEDURE cabs_glue(x, y : LONGREAL) : LONGREAL;  

<*EXTERNAL "MathPosixC__modf_result_glue"*>
PROCEDURE modf_result_glue(x : LONGREAL) : LONGREAL;

<*EXTERNAL "MathPosixC__modf_intpart_glue"*>
PROCEDURE modf_intpart_glue(x : LONGREAL) : LONGREAL;

END MathPosixC.

