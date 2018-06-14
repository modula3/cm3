UNSAFE INTERFACE ContextC;
IMPORT Ctypes;
FROM CoroutineUcontext IMPORT Arg, Entry;

TYPE T = ADDRESS;

<*EXTERNAL ContextC__New*>
PROCEDURE New() : T;

<*EXTERNAL ContextC__MakeContext*>
PROCEDURE MakeContext(p      : Entry;
                      ssize  : Ctypes.int;
                      resume : T;
                      arg    : Arg): T;

<*EXTERNAL ContextC__Current*>
PROCEDURE Current() : T; (* creates a new T, which must eventually be
                            freed using DisposeContext *)

<*EXTERNAL ContextC__SwapContext*>
PROCEDURE SwapContext(from, to : T);  

<*EXTERNAL ContextC__DisposeContext*>
PROCEDURE DisposeContext(ctx : T);

<*EXTERNAL ContextC__GetCurrentCoroutine*>
PROCEDURE  GetCurrentCoroutine() : UNTRACED REF INTEGER;

<*EXTERNAL ContextC__SetCurrentCoroutine*>
PROCEDURE  SetCurrentCoroutine(c : UNTRACED REF INTEGER);

<*EXTERNAL ContextC__InitC*>
PROCEDURE  InitC(); (* must call this before any of the other routines in this interface *)

<*EXTERNAL ContextC__Dbg*>
PROCEDURE Dbg(q : INTEGER (* ASCII value *));

<*EXTERNAL ContextC__DbgPtr*>
PROCEDURE DbgPtr(a : ADDRESS);

  
END ContextC.
