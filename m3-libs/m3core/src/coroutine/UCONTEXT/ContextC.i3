UNSAFE INTERFACE ContextC;
IMPORT Ctypes;
FROM CoroutineUcontext IMPORT Arg, Entry;

TYPE T = ADDRESS;

<*EXTERNAL ContextC__MakeContext*>
PROCEDURE MakeContext(p      : Entry;
                      ssize  : Ctypes.int;
                      arg    : Arg): T;
  (* creates a new T, which must eventually be freed using DisposeContext *)

<*EXTERNAL ContextC__Current*>
PROCEDURE Current() : T;
  (* creates a new T, which must eventually be freed using DisposeContext *)

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

<*EXTERNAL ContextC__SetLink*>  
PROCEDURE SetLink(tgt, src : T); (* set return link of tgt to be src *)

<*EXTERNAL ContextC__GetStackBase*>
PROCEDURE GetStackBase(t : T) : ADDRESS;
  
<*EXTERNAL ContextC__PushContext*>
PROCEDURE PushContext(t : T) : ADDRESS;

END ContextC.
