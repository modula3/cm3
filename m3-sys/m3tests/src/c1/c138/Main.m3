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
PROCEDURE ReturnFromTry(): INTEGER = BEGIN TRY Int(1); RETURN Line(); FINALLY Int(2); END; END ReturnFromTry;
PROCEDURE ReturnFromTryUplevelLocal(): INTEGER = VAR i := 1; BEGIN Int(3); TRY Int(4); FINALLY RETURN Line() + i; END; END ReturnFromTryUplevelLocal;
PROCEDURE ReturnFromTryUplevelParam(i:INTEGER): INTEGER = BEGIN TRY Int(5); FINALLY Int(6); RETURN Line() + i; END; END ReturnFromTryUplevelParam;
PROCEDURE ReturnFromFinally(): INTEGER = BEGIN TRY Int(7); FINALLY Int(8); RETURN Line(); END; END ReturnFromFinally;
PROCEDURE ReturnFromFinallyUplevelLocal(): INTEGER = VAR i := 1; BEGIN TRY FINALLY RETURN Line() + i; END; END ReturnFromFinallyUplevelLocal;
PROCEDURE ReturnFromFinallyUplevelParam(i:INTEGER): INTEGER = BEGIN TRY FINALLY RETURN Line() + i; END; END ReturnFromFinallyUplevelParam;

PROCEDURE ReturnRaisedInteger(): INTEGER = BEGIN TRY RAISE E2(-Line()) EXCEPT E2(i) => RETURN i; END; RETURN Line(); END ReturnRaisedInteger;
PROCEDURE CatchInteger(): INTEGER = BEGIN TRY RAISE E2(-Line()); EXCEPT E2(i) => END; RETURN Line(); END CatchInteger;
PROCEDURE CatchException(): INTEGER = BEGIN TRY RAISE E1 EXCEPT E1 => | E2(i) => END;
RETURN Line(); END CatchException;

BEGIN
Int(ReturnFromTry());
Int(ReturnFromTryUplevelLocal());
Int(ReturnFromTryUplevelParam(Line()));
Int(ReturnFromFinally());
Int(ReturnFromFinallyUplevelParam(Line()));
Int(ReturnFromFinallyUplevelLocal());
Int(ReturnRaisedInteger());
Int(CatchInteger());
Int(CatchException());
RTIO.Flush();
END Main.
