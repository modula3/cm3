(* --------------------------------------------------------------------
 * FILE:     M3LuaObject.m3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Object Oriented bindings for the Lua scripting language
 * VERSION:  0.0.1 (07-Nov-2010) PPE
 * ----------------------------------------------------------------- *)


UNSAFE MODULE M3LuaObject;

IMPORT	Ctypes, Cstddef, Cstdlib, Cstring;
IMPORT	M3LuaBase, M3LuaRaw, ObjectRegistry;


REVEAL T = Public BRANDED OBJECT
	m_state : M3LuaBase.State := NIL;
	m_id : Ctypes.void_star := NIL;

METHODS
	setState(state: M3LuaBase.State) := SetState;

OVERRIDES
	init       := Init;
	close      := Close;
	newThread  := NewThread;
	setAtPanic := SetAtPanic;

	(* -- generic stack manipulation -- *)
	gettop     := GetTop;
	settop     := SetTop;
	checkStack := CheckStack;

	dup        := Duplicate;
	pop        := Pop;
	remove     := Remove;
	insert     := Insert;
	replace    := Replace;
	xmovefrom  := XMoveFrom;
	xmoveto    := XMoveTo;

(* ===
	(* -- query methods --*)
	isNone(index: INTEGER): BOOLEAN;
	isNil(index: INTEGER): BOOLEAN;
	isNoneOrNill(index: INTEGER): BOOLEAN;
	isBoolean(index: INTEGER): BOOLEAN;
	isNumber(index: INTEGER): BOOLEAN;
	isString(index: INTEGER): BOOLEAN;
	isFunction(index: INTEGER): BOOLEAN;
	isUserData(index: INTEGER): BOOLEAN;
	isTable(index: INTEGER): BOOLEAN;
	isThread(index: INTEGER): BOOLEAN;

	isType(index: INTEGER; typ: LuaType): BOOLEAN;
	typeName(index: INTEGER) : TEXT;
	isEqual(index1, index2: INTEGER): BOOLEAN;
	isRawEqual(index1, index2: INTEGE): BOOLEAN;
	isLessThan(index1, index2: INTEGER): BOOLEAN;
	objLength(index: INTEGER): Cstddef.size_t;

	(* -- access methods -- *)
	ToNumber(index: INTEGER): Number;
	ToInteger(index: INTEGER): INTEGER;
	ToBoolean(index: INTEGER): BOOLEAN;
	ToString(index: INTEGER): TEXT;
	ToFunction(index: INTEGER): Function;
	ToUserData(index: INTEGER): ADDRESS;
	ToThread(index: INTEGER): T;
	ToPointer(index: INTEGER): ADDRESS;

	(* -- push values onto stack -- *)
	pushNill();
	pushNumber(x: Number);
	pushInteger(x: INTEGER);
	pushString(x: TEXT);
	pushBoolean(x: BOOLEAN);
	pushLightUserData(x: ADDRESS;
	pushThread(x: T);
	pushClosure(func: Funxtion, nargs: INTEGER);

	(* -- get values off of stack -- *)
	getTable(index: INTEGER);
	getField(index: INTEGER; fieldName: TEXT);
	rawGet(index: INTEGER);
	rawGetIndirect(index, n: INTEGER);
	getMetaTable(index: INTEGER): INTEGER;
	createTable(narr, nrec: INTEGER);
	newUserData(sz: Cstddef.size_t): ADDRESS;
	getFunctEnv(index: INTEGER);

	(* -- exchange values between stack and lua -- *)
	setTable(index: INTEGER);
	setField(index: INTEGER; fieldName: TEXT);
	rawSet(index: INTEGER);
	rawSetIndirect(index, n: INTEGER
	setMetaTable(index: INTEGER);
	setFunctEnv(index: INTEGER);
	setGlobal(name: TEXT);
	getGlobal(name: TEXT);

	(* -- load/call methods -- *)
	call(nargs, nresults: INTEGER);
	protected_call(nargs, nresults, errFunc: INTEGER);
	c_protected_call(nargs, nresults: INTEGER; errFunc: Function);
	load(reader: ReaderFunct; dt: ADDRESS; chunkName: TEXT): INTEGER;
	dump(writer: WriterFunct; data: ADDRESS): INTEGER;

	(* -- co-routine support -- *)
	yield(nresults: INTEGER): INTEGER;
	resume(narg: INTEGER): INTEGER;
	status(state: T): INTEGER;
	garbageCollector(what, data: ADDRESS): INTEGER;
	getGargabeCollectorCount(): INTEGER;

	(* -- miscellaneous -- *)
	error(): INTEGER;
	next(index: INTEGER): INTEGER;
	concat(n: INTEGER);
	getAllocFunc(userdata: ADDRESS): AllocFunct;
	setAllocFunc(alloc: AllocFunct; userdata: ADDRESS);
	setLevel(from, to: T);

	registerFunct(name: TEXT; func: Function);
	pushFunct(func: Function);
	strlen(index: INTEGER): INTEGER;

=== *)
	
END;


(* ========================================================= *)

PROCEDURE MAlloc(<*UNUSED*> userdata: Ctypes.void_star; 
			ptr: Ctypes.void_star; 
			osize: Cstddef.size_t;
			nsize: Cstddef.size_t): Ctypes.void_star = 
VAR
	nptr: Ctypes.void_star;
	xfer_size : Cstddef.size_t;
BEGIN
	IF nsize > 0 THEN
		nptr := Cstdlib.malloc(nsize);
	END;
	IF nsize > 0 AND osize > 0 THEN
		xfer_size := osize;
		IF nsize < osize THEN xfer_size := nsize; END;
		EVAL Cstring.memcpy(nptr, ptr, xfer_size);
	END;
	IF osize > 0 THEN
		Cstdlib.free(ptr);
	END;
	RETURN nptr;
END MAlloc;


PROCEDURE SetState(self: T; state: M3LuaBase.State) =
BEGIN
	self.m_state := state;
END SetState;


(* ========================================================= *)

PROCEDURE Init(self: T) : T =
BEGIN
	self.m_id := ObjectRegistry.Register(self);
	self.m_state := M3LuaRaw.NewState(MAlloc, self.m_id);
	RETURN self;
END Init;


PROCEDURE Close(self: T) =
BEGIN
	M3LuaRaw.Close(self.m_state);
	self.m_state := NIL;
	ObjectRegistry.UnRegister(self);
END Close;


PROCEDURE NewThread(self: T) : T =
VAR
	nt := NEW(T);
BEGIN
	nt.m_state := M3LuaRaw.NewThread(self.m_state);
	nt.m_id := ObjectRegistry.Register(nt);
	RETURN nt;
END NewThread;


PROCEDURE SetAtPanic(self: T; func: M3LuaBase.CallbackFunction) = 
BEGIN
	M3LuaRaw.AtPanic(self.m_state, func);
END SetAtPanic;


PROCEDURE GetTop(self: T) : INTEGER =
BEGIN
	RETURN M3LuaRaw.GetTop(self.m_state);
END GetTop;


PROCEDURE SetTop(self: T; newtop: INTEGER) =
BEGIN
	M3LuaRaw.SetTop(self.m_state, newtop);
END SetTop;


PROCEDURE CheckStack(self: T; sz: INTEGER) : INTEGER =
BEGIN
	RETURN M3LuaRaw.CheckStack(self.m_state, sz);
END CheckStack;


PROCEDURE Duplicate(self: T; index : INTEGER) =
BEGIN
	M3LuaRaw.PushValue(self.m_state, index);
END Duplicate;


PROCEDURE Pop(self: T) =
BEGIN
	M3LuaRaw.Remove(self.m_state, -1);
END Pop;


PROCEDURE Remove(self: T; index: INTEGER) =
BEGIN
	M3LuaRaw.Remove(self.m_state, index);
END Remove;


PROCEDURE Insert(self: T; index: INTEGER) =
BEGIN
	M3LuaRaw.Insert(self.m_state, index);
END Insert;


PROCEDURE Replace(self: T; index: INTEGER) = 
BEGIN
	M3LuaRaw.Replace(self.m_state, index);
END Replace;


PROCEDURE XMoveFrom(self, from: T; index: INTEGER) =
BEGIN
	M3LuaRaw.XMove(self.m_state, from.m_state, index);
END XMoveFrom;


PROCEDURE XMoveTo(self, to: T; index: INTEGER) =
BEGIN
	M3LuaRaw.XMove(to.m_state, self.m_state, index);
END XMoveTo;














BEGIN
END M3LuaObject.
