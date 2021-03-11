(* $Id$ *)

INTERFACE VarProxyClass;
IMPORT VarUI;

TYPE
  Private = VarUI.PubVarProxy OBJECT
    mu : MUTEX;
    mode : VarUI.ProxyMode;
  END;

REVEAL VarUI.VarProxy <: Private;

END VarProxyClass.

  
