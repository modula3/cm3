INTERFACE ExceptionInfo;

PROCEDURE Fmt(address : ADDRESS) : TEXT;
  (* address must be from Compiler.ThisException() *)

END ExceptionInfo.
