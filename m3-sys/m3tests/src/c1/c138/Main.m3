MODULE Main;
IMPORT RTIO, Compiler, RT0;
EXCEPTION E1;
EXCEPTION E2(INTEGER);
CONST Line = Compiler.ThisLine;

PROCEDURE Int(i: INTEGER) = BEGIN RTIO.PutInt(i); RTIO.PutText("\n"); END Int;
PROCEDURE NL() = BEGIN RTIO.PutText("\n"); END NL;

(* From RuntimeError.m3, RT0.i3

  RaiseActivation = RECORD
    exception : ExceptionPtr;
    arg       : ExceptionArg;
    module    : ModulePtr;  (* if available, the raising module that called RAISE *)
    line      : INTEGER;    (* if available, the source line number of the RAISE *)
    pc        : ADDRESS;    (* if available, the PC of the RAISE *)
    info0     : ADDRESS;    (* misc. info *)
    info1     : ADDRESS;    (* misc. info *)
    un_except : ExceptionPtr; (* the unhandled exception being reported *)
    un_arg    : ExceptionArg; (* the argument to the unhandled exception *)
  END;

PROCEDURE Self (): RT0.ExceptionPtr =
  BEGIN
    IF (self = NIL) THEN
      TRY
        RAISE E (T.Unknown);
      EXCEPT E =>
        self := LOOPHOLE (Compiler.ThisException(), RT0.ActivationPtr).exception;
      END;
    END;
    RETURN self;
  END Self;
*)
PROCEDURE ReturnFromTry(): INTEGER = BEGIN Int(10); TRY Int(20); RETURN 30; FINALLY Int(40); END; END ReturnFromTry;
(*PROCEDURE ReturnFromTryUplevelLocal(): INTEGER = VAR i := 1; BEGIN Int(50); RETURN 51; TRY Int(60); FINALLY Int(70+i); END; END ReturnFromTryUplevelLocal;*)
PROCEDURE ReturnFromTryUplevelParam(i:INTEGER): INTEGER = BEGIN TRY Int(80); RETURN 90; FINALLY Int(100+i); END; END ReturnFromTryUplevelParam;
PROCEDURE ReturnFromFinally(): INTEGER = BEGIN TRY Int(110); FINALLY Int(120); RETURN 130; END; END ReturnFromFinally;
PROCEDURE ReturnFromFinallyUplevelLocal(): INTEGER = VAR i := 1; BEGIN TRY Int(140); FINALLY RETURN 150 + i; END; END ReturnFromFinallyUplevelLocal;
PROCEDURE ReturnFromFinallyUplevelParam(i:INTEGER): INTEGER = BEGIN TRY Int(170); FINALLY RETURN 180 + i; END; END ReturnFromFinallyUplevelParam;

PROCEDURE ReturnRaisedInteger(): INTEGER = BEGIN TRY Int(191); RAISE E2(190) EXCEPT E2(i) => RETURN 190; END; RETURN Line(); END ReturnRaisedInteger;
PROCEDURE CatchInteger(): INTEGER = BEGIN TRY Int(210); RAISE E2(220); EXCEPT E2(i) => END; RETURN 230; END CatchInteger;
PROCEDURE CatchException(): INTEGER = BEGIN TRY Int(240); RAISE E1 EXCEPT E1 => Int(250) | E2(i) => Int(260) END; RETURN 270; END CatchException;

BEGIN
Int(ReturnFromTry());
(*Int(ReturnFromTryUplevelLocal());*)
Int(ReturnFromTryUplevelParam(Line()));
Int(ReturnFromFinally());
Int(ReturnFromFinallyUplevelParam(Line()));
Int(ReturnFromFinallyUplevelLocal());
Int(ReturnRaisedInteger());
Int(CatchInteger());
Int(CatchException());
RTIO.Flush();
END Main.
