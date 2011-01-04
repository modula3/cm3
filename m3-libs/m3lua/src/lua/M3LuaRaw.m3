(* --------------------------------------------------------------------
 * FILE:     M3LuaRaw.m3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Low level bindings for the Lua scripting language
 * VERSION:  0.0.1 (07-Sep-2010) PPE
 * ----------------------------------------------------------------- *)
UNSAFE MODULE M3LuaRaw;

IMPORT Cstring, Ctypes;

IMPORT M3LuaBase;
FROM M3LuaBase IMPORT State, CallbackFunction;


PROCEDURE Pop(L: State; n: INTEGER) =
BEGIN
	SetTop(L, -(n+1));
END Pop;


PROCEDURE NewTable(L: State) =
BEGIN
   CreateTable(L, 0, 0);
END NewTable;


PROCEDURE Register(L: State; 
                   name: Ctypes.const_char_star; 
                   cfunc: CallbackFunction) =
BEGIN
	PushCallbackFunction(L, cfunc);
	SetGlobal(L, name);
END Register;


PROCEDURE PushCallbackFunction(L: State; cfunc: CallbackFunction) =
BEGIN
	PushCClosure(L, cfunc, 0);
END PushCallbackFunction;



PROCEDURE StrLen(L: State; idx: INTEGER): INTEGER =
BEGIN
	RETURN ObjLen(L, idx);
END StrLen;


PROCEDURE IsFunction(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeFunction);
END IsFunction;

(*
PROCEDURE IsCallbackFunction(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeFunction);
END IsCallbackFunction;
*)

PROCEDURE IsTable(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeTable);
END IsTable;


PROCEDURE IsLightUserData(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeLightUserData);
END IsLightUserData;


PROCEDURE IsNil(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeNil);
END IsNil;


PROCEDURE IsBoolean(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeBoolean);
END IsBoolean;


PROCEDURE IsThread(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeThread);
END IsThread;


PROCEDURE IsNone(L: State; idx: INTEGER): BOOLEAN =
BEGIN
	RETURN (Type(L, idx) = M3LuaBase.TypeNone);
END IsNone;


PROCEDURE IsNoneOrNil(L: State; idx: INTEGER): BOOLEAN =
VAR
   typ := Type(L, idx);
BEGIN
	RETURN (typ = M3LuaBase.TypeNone OR typ = M3LuaBase.TypeNil);
END IsNoneOrNil;



PROCEDURE PushLiteral(L: State; str: Ctypes.const_char_star) =
VAR len: INTEGER;
BEGIN
	IF (str = NIL) THEN
		len := 0;
	ELSE
		len := Cstring.strlen(str);
	END;
	PushLString(L, str, len);
END PushLiteral;


PROCEDURE SetGlobal(L: State; str: Ctypes.const_char_star) =
BEGIN
	SetField(L, M3LuaBase.GlobalsIndex, str);
END SetGlobal;


PROCEDURE GetGlobal(L: State; name: Ctypes.char_star) =
BEGIN
	GetField(L, M3LuaBase.GlobalsIndex, name);
END GetGlobal;


PROCEDURE ToString(L: State; idx: Ctypes.int): Ctypes.char_star =
BEGIN
	RETURN ToLString(L, idx, NIL);
END ToString;

(* FIXME uses luaL_newstate() [[Note the LuaL ]]
PROCEDURE Open(): State =
BEGIN
	RETURN NewState();
END Open;
*)


PROCEDURE GetRegistry(L: State) =
BEGIN
	PushValue(L, M3LuaBase.RegistryIndex);
END GetRegistry;


PROCEDURE GetGcCount(L: State): Ctypes.int =
BEGIN
	RETURN GarbageCollector(L, M3LuaBase.GcCount, 0);
END GetGcCount;


BEGIN
END M3LuaRaw.
