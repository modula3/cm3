(* $Id$ *)

INTERFACE SchemeReadLine;
IMPORT ReadLine, Scheme, NetObj, ReadLineError;

PROCEDURE MainLoop(rl : ReadLine.T; 
                   scm : Scheme.T) RAISES { NetObj.Error,
                                            ReadLineError.E };

PROCEDURE ReturningMainLoop(rl : ReadLine.T; 
                            scm : Scheme.T) : Scheme.Object
  RAISES { NetObj.Error, ReadLineError.E };

CONST Brand = "SchemeReadline";

END SchemeReadLine.
