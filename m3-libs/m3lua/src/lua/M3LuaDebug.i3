PROCEDURE Open(): State;
PROCEDURE GetRegistry(L: State);
PROCEDURE GetGcCount(L: State): INTEGER;


(* --- Debugging API --- *)
CONST

(* -- Event codes -- *)
HookCall    = 0;
HookReturn  = 1;
HookLine    = 2;
HookCount   = 3;
HookTailRet = 4;

(* -- Event masks -- *)
MaskCall   = Word.LeftShift(1, HookCall);
MaskReturn = Word.LeftShift(1, HookReturn);
MaskLine   = Word.LeftShift(1, HookLine);
MaskCount  = Word.LeftShift(1, HookCount);

TYPE

DebugPtr = UNTRACED REF DebugRecord;

PROCEDURE GetStack(L: State; level: Ctypes.int; ar: DebugPtr): Ctypes.int;





END M3LuaRaw.
