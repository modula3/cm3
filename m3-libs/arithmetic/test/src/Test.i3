INTERFACE test;

FROM xReal64 IMPORT REAL64,EPS;

(*from the driver---*)
VAR verbosity:[0..3]:=3;
PROCEDURE msg(str:TEXT);
PROCEDURE newline();
PROCEDURE debug(level:[0..3]; ftn,str:TEXT);
PROCEDURE verify(ftn,str:TEXT;
                 expected,found:REAL64;
                 eps:REAL64:=EPS 
                 ):BOOLEAN;

(*---from the test units---*)
PROCEDURE test_WordEx():BOOLEAN;
PROCEDURE test_Bits():BOOLEAN;
PROCEDURE test_Integer():BOOLEAN;
PROCEDURE test_Real64():BOOLEAN;
PROCEDURE test_Cmplx():BOOLEAN;
PROCEDURE test_Vect():BOOLEAN;
PROCEDURE test_Mat():BOOLEAN;
PROCEDURE test_Root():BOOLEAN;
PROCEDURE test_Interp():BOOLEAN;
PROCEDURE test_Poly():BOOLEAN;
PROCEDURE test_SLE():BOOLEAN;
PROCEDURE test_Rand():BOOLEAN;
PROCEDURE test_FFT():BOOLEAN;
PROCEDURE test_BigInteger():BOOLEAN;
END test.
