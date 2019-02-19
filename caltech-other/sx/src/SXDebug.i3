(* $Id$ *)

INTERFACE SXDebug;
IMPORT SX;

PROCEDURE Trace(on : BOOLEAN);

PROCEDURE Register(sx : SX.T; callback : Callback);

TYPE Callback = OBJECT METHODS changed(sx : SX.T) END;

END SXDebug.
