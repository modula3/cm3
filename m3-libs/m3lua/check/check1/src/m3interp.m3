(* --------------------------------------------------------------------
 * COPYRIGHT: 2010, Peter Eiserloh
 * LICENSE: BSD Two Clause
 * FILE: m3interp.m3
 * AUTHOR: Peter Eiserloh
 * LANGUAGE: Modula-3
 * ----------------------------------------------------------------- *)

UNSAFE MODULE m3interp EXPORTS Main;

IMPORT	RTMisc, Ctypes, Cstddef, Cstring, Stdio, IO, M3toC,
	M3LuaBase, M3LuaRaw, SimpleMemory;

CONST
	Version = "0.0.1";
	VerDate = "15-Oct-2010";

VAR
	gState : M3LuaBase.State;
	gDone := FALSE;

(* ----------------------------------------- *)


(**
 * AllocateFunct() is specified by us to be used by the
 * LUA library for all its memory allocations.  It can
 * also be used to free any old pointers, or resize existing
 * ones.  
 * NOTE: The lua library may keep references to these memory
 *       locations, about which the M3 garbage collector doesn't know.
 *       All memory allocated must not be garbage collected by us,
 *       or even moved by the garbage collector.
 **)
PROCEDURE AllocFunct(<*UNUSED*> ud: Ctypes.void_star;
				ptr: Ctypes.void_star;
				osize: Cstddef.size_t;
				nsize: Cstddef.size_t): Ctypes.void_star =
VAR
	nptr: UNTRACED REF ARRAY OF CHAR;
	xfer: INTEGER;
BEGIN
	IF nsize > 0 THEN
		nptr := SimpleMemory.Alloc(nsize);
	END;
	xfer := MIN(osize, nsize);
	IF ptr # NIL THEN
		IF xfer > 0 THEN
			RTMisc.Copy(ptr, nptr, xfer);
		END;
		SimpleMemory.Free(ptr);
	END;
	RETURN nptr;
END AllocFunct;


TYPE

MyReaderUserData = RECORD
	str: Ctypes.const_char_star;
	offset: INTEGER;
END;


PROCEDURE MyLuaReader(<*UNUSED*>state: M3LuaBase.State;
			data: Ctypes.void_star;
			size: Ctypes.int_star) : Ctypes.const_char_star =
VAR
	userdata: REF MyReaderUserData;
BEGIN
	userdata := LOOPHOLE(data, REF MyReaderUserData);
	size^ := Cstring.strlen(userdata.str);
	RETURN userdata.str;
END MyLuaReader;


PROCEDURE LoadBuffer(state: M3LuaBase.State; str : TEXT) =
VAR
	userdata: MyReaderUserData;
	errCode: INTEGER;
BEGIN
	userdata.str := M3toC.FlatTtoS(str);
	userdata.offset := 0;
	errCode := M3LuaRaw.Load(state, 
			MyLuaReader, 
			ADR(userdata), 
			M3toC.FlatTtoS("input_stream"));
END LoadBuffer;



(* ----------------------------------------- *)



(**
 * m3lua_version - A callback function, invoked by
 * the lua scripting engine.  
 * Stack Usage:
 *      [ ] it uses no parameters from the lua parameter stack
 *      [string] it places a string onto the stack.
 **)
PROCEDURE m3lua_version(state: M3LuaBase.State): INTEGER =
BEGIN
	M3LuaRaw.PushString(state, M3toC.FlatTtoS(VerDate));
	M3LuaRaw.PushString(state, M3toC.FlatTtoS(Version));
	RETURN 1;
END m3lua_version;


PROCEDURE m3lua_quit(<*UNUSED*>state: M3LuaBase.State): INTEGER =
BEGIN
	gDone := TRUE;
	RETURN 0;
END m3lua_quit;


(* ----------------------------------------- *)

PROCEDURE Init() : BOOLEAN=
BEGIN
	gState := M3LuaRaw.NewState(AllocFunct, NIL);
	IF gState = NIL THEN
		IO.Put("ERROR: Failed to create new lua instance.\n");
		RETURN FALSE;
	END;
	M3LuaRaw.Register(gState, M3toC.FlatTtoS("version"), m3lua_version);
	M3LuaRaw.Register(gState, M3toC.FlatTtoS("quit"), m3lua_quit);
	RETURN TRUE;
END Init;



PROCEDURE Loop() =
VAR
	line    : TEXT;
	count   : INTEGER := 0;
	errCode : INTEGER := 0;
	errMsg  : Ctypes.const_char_star;
	errText : TEXT;
BEGIN
	WHILE NOT gDone DO
		(* prompt with lien number *)
		IO.PutInt(count);
		IO.Put("> ");

		TRY
			line := IO.GetLine(Stdio.stdin);
		EXCEPT
			IO.Error =>
				IO.Put("I/O Error: Aborting\n");
				RETURN;
		END;
		count := count + 1;
		IF count > 16 THEN
			(* DEBUG: force to close *)
			gDone := TRUE;
		END;

(* FIXME:
		(* load string into lua, and execute it *)
		LoadBuffer(gState, line);
		errCode := M3LuaRaw.ProtectedCall(gState, 0, 0, 0);
*)

		IF errCode # 0 THEN
			(* get error message from top of stack. *)
			errMsg := M3LuaRaw.ToString(gState, -1);
			errText := M3toC.StoT(errMsg);

			IO.Put("Line: "); IO.PutInt(count);
			IO.Put(", ERROR: "); IO.PutInt(errCode);
			IO.Put(": "); IO.Put(errText);
			IO.Put("\n");
		END;
	END;
END Loop;


PROCEDURE CleanUp() =
BEGIN
	IF (gState # NIL) THEN
		M3LuaRaw.Close(gState);
	END;
END CleanUp;


BEGIN
   IF Init() THEN
   	Loop();
   	CleanUp();
   END;
END m3interp.
