(* $Id$ *)

INTERFACE VarUI;
IMPORT ReadLineUI;
IMPORT SXInt, SXBool, SXLongReal, SXText;

(*
  a VarUI.T is a user interface that provides basic commands:
   
   set <variable> <value>
   show <variable>
   show                        # show all variables 

   future plan: we can add lexical scoping via a class interface
   for looking up proxies.
*)

TYPE
  T <: Public;

  Public = ReadLineUI.T OBJECT METHODS
    init(myName : TEXT) : T;

    addVar(varName : TEXT; varProxy : VarProxy; comment := "") : BOOLEAN;
  END;

TYPE 
  VarProxy <: PubVarProxy;
  
  ProxyMode = { ReadWrite, ReadOnly };
  
  PubVarProxy = OBJECT METHODS
    init(mode := ProxyMode.ReadWrite) : VarProxy;

    (* override the following *)
    (* varName are just provided for convenience to the implementor *)

    (* note that an implementor should import VarProxyClass and lock the
       mu MUTEX declared in that class before setting the variables from
       outside the UI (the UI will lock this mutex before calling the
       following methods) *)
    set(varName : TEXT; to : TEXT) RAISES { ReadLineUI.Error };
    show(varName : TEXT) : TEXT  RAISES { ReadLineUI.Error };

    (* the following should be called whenever the variable changes *)
    notify();

    registerCallback(cb : Callback);
  END;

  Callback = OBJECT METHODS notify(var : VarProxy); END;

  (* default ones that clients can use as variables directly *)

<*OBSOLETE*>
TYPE
  DefIntProxy <: PubIntProxy;

  PubIntProxy = VarProxy OBJECT METHODS
    init(val : INTEGER; mode := ProxyMode.ReadWrite) : DefIntProxy;
    getValue() : INTEGER;
    setValue(int : INTEGER);
    sx() : SXInt.T;
  END;

  DefTextProxy <: PubTextProxy;

  PubTextProxy = VarProxy OBJECT METHODS
    init(val : TEXT; mode := ProxyMode.ReadWrite) : DefTextProxy;
    getValue() : TEXT;
    setValue(to : TEXT);
    sx() : SXText.T;
  END;

  DefLRProxy <: PubLRProxy;

  PubLRProxy = VarProxy OBJECT METHODS
    init(val : LONGREAL; mode := ProxyMode.ReadWrite) : DefLRProxy;
    getValue() : LONGREAL;
    setValue(to : LONGREAL);
    sx() : SXLongReal.T;
  END;

  DefBoolProxy <: PubBoolProxy;

  PubBoolProxy = VarProxy OBJECT METHODS
    init(val : BOOLEAN; mode := ProxyMode.ReadWrite) : DefBoolProxy;
    getValue() : BOOLEAN;
    setValue(to : BOOLEAN);
    sx() : SXBool.T;
  END;

CONST Brand = "VarUI";

(* why aren't the proxies generic? --- only because of show and set!
   as we add more and more generic functionality to the proxies, we 
   should break them up as follows:

   <this interface>.PubXProxy
            |
       GenXProxy.T  (generic instance) - defines all but show and set
            |
       DefXProxy.T  (non-generic) - inherits from GenX, reveals def'ns of
                                    show and set.
*)

END VarUI.
