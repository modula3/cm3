(* --------------------------------------------------------------------
 * FILE:     M3LuaObject.i3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Object Oriented bindings for the Lua scripting language
 * VERSION:  0.0.1 (07-Nov-2010) PPE
 * ----------------------------------------------------------------- *)


INTERFACE M3LuaObject;

IMPORT Cstddef, M3LuaBase;
FROM M3LuaBase IMPORT CallbackFunction;


TYPE

T <: Public;
Public = OBJECT
METHODS
	init(): T;
	close();
	newThread(): T;
	setAtPanic(panic: M3LuaBase.CallbackFunction);

	(* -- generic stack manipulation -- *)
	gettop(): INTEGER;
	settop(newtop: INTEGER);
	checkStack(sz: INTEGER) : INTEGER;

	dup(index: INTEGER);
	pop();
	remove(index: INTEGER);
	insert(index: INTEGER);
	replace(index: INTEGER);
	xmovefrom(from: T; index: INTEGER);
	xmoveto(to: T; index: INTEGER);


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

	isType(index: INTEGER; typ: M3LuaBase.LuaType): BOOLEAN;
	typeName(index: INTEGER) : TEXT;
	isEqual(index1, index2: INTEGER): BOOLEAN;
	isRawEqual(index1, index2: INTEGER): BOOLEAN;
	isLessThan(index1, index2: INTEGER): BOOLEAN;
	objLength(index: INTEGER): Cstddef.size_t;

	(* -- access methods -- *)
	ToFloat(index: INTEGER): LONGREAL;
	ToInteger(index: INTEGER): INTEGER;
	ToBoolean(index: INTEGER): BOOLEAN;
	ToString(index: INTEGER): TEXT;
	ToFunction(index: INTEGER): M3LuaBase.CallbackFunction;
	ToUserData(index: INTEGER): ADDRESS;
	ToThread(index: INTEGER): T;
	ToPointer(index: INTEGER): ADDRESS;

	(* -- push values onto stack -- *)
	pushNill();
	pushNumber(x: LONGREAL);
	pushInteger(x: INTEGER);
	pushString(x: TEXT);
	pushBoolean(x: BOOLEAN);
	pushLightUserData(x: ADDRESS);
	pushThread(x: T);
	pushClosure(func: CallbackFunction; nargs: INTEGER);

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
	rawSetIndirect(index, n: INTEGER);
	setMetaTable(index: INTEGER);
	setFunctEnv(index: INTEGER);
	setGlobal(name: TEXT);
	getGlobal(name: TEXT);

	(* -- load/call methods -- *)
	call(nargs, nresults: INTEGER);
	protected_call(nargs, nresults, errFunc: INTEGER);
	c_protected_call(nargs, nresults: INTEGER; 
                         errFunc: CallbackFunction);
	load(reader: M3LuaBase.ReaderFunct; 
                         dt: ADDRESS; 
                         chunkName: TEXT): INTEGER;
	dump(writer: M3LuaBase.WriterFunct; data: ADDRESS): INTEGER;

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
	getAllocFunc(userdata: ADDRESS): M3LuaBase.AllocFunct;
	setAllocFunc(alloc: M3LuaBase.AllocFunct; userdata: ADDRESS);
	setLevel(from, to: T);

	registerFunct(name: TEXT; func: CallbackFunction);
	pushFunct(func: CallbackFunction);
	strlen(index: INTEGER): INTEGER;
	
END;



END M3LuaObject.
