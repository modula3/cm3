INTERFACE Test;

FROM xReal64 IMPORT REAL64,EPS;

(*from the driver---*)
VAR verbosity:[0..3]:=3;
PROCEDURE Msg(str:TEXT);
PROCEDURE NewLine();
PROCEDURE Debug(level:[0..3]; ftn,str:TEXT);
PROCEDURE Verify(ftn,str:TEXT;
                 expected,found:REAL64;
                 eps:REAL64:=EPS
                 ):BOOLEAN;

(*---from the Test units---*)
PROCEDURE TestWordEx():BOOLEAN;
PROCEDURE TestBits():BOOLEAN;
PROCEDURE TestInteger():BOOLEAN;
PROCEDURE TestReal64():BOOLEAN;
PROCEDURE TestComplex():BOOLEAN;
PROCEDURE TestVector():BOOLEAN;
PROCEDURE TestMatrix():BOOLEAN;
PROCEDURE TestRoot():BOOLEAN;
PROCEDURE TestInterpolation():BOOLEAN;
PROCEDURE TestPolynomial():BOOLEAN;
PROCEDURE TestSLE():BOOLEAN;
PROCEDURE TestRandom():BOOLEAN;
PROCEDURE TestFFT():BOOLEAN;
PROCEDURE TestBigInteger():BOOLEAN;
PROCEDURE TestGCD():BOOLEAN;
END Test.
