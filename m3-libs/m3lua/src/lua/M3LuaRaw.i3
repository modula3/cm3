(* --------------------------------------------------------------------
 * FILE:     M3LuaRaw.i3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Low level bindings for the Lua scripting language,
 *           and this modules is unsafe, containing the functions.
 * VERSION:  0.0.1 (07-Sep-2010) PPE
 * ----------------------------------------------------------------- *)

(* ------------------------------------------------------------
   These Modula-3 bindings closely follow the C-API provided in
   the lua.h header file.  The major concept is the parameter
   stack by which the Lua environment does impedence matching 
   to foreign environments such as "C" and now "Modula-3".  
   The two big issues are garbage collection, and that lua
   does not use fixed types, but rather its typeless variables 
   can have values of different types (ie, a variable can have
   values of different types at different times).  The parameter
   stack is implemented as a lua table.  

   Values of any of the Lua types can be found on the parameter 
   stack.  The items on the parameter stack are referenced by 
   an index.   Negative indices are referenced from the top of 
   the stack, where -1 is the top element.  Positive indices 
   (starting at 1) are referenced from the bottom of the stack, 
   so the stack can be used similar to an array.  Zero is an
   invalid index.  In addition, there are pseudo indices which
   access things such as global variables.

   Lua has a small number of primitive types, and one structured
   type (the table).  The Lua library also supports user defined
   data, which is not accessed by the Lua system, but is accessable
   to the host program.  This may be used by clients, especially
   when implementing an object oriented interface.

   The one lua structured type is the table.  It is very flexible, 
   and can perform as an array, record, hash table, or even 
   modules or objects.  

   Functions are first class anonymous values, and can be stored in 
   variables or the values of a table entry.  A closure is 
   created to contain the environment (local variables) when 
   a function is created.

 * --------------------------------------------------------- *)


UNSAFE INTERFACE M3LuaRaw;

IMPORT Ctypes, Cstddef;

IMPORT M3LuaBase;
FROM M3LuaBase IMPORT State, Number, Integer, CallbackFunction;


(* ---------------------------------------------------
 * State Manipulation:
 * ------------------------------------------------ *)

(**
 * NewState: Creates a new independent state, or RETURNs 
 * NULL on failure.  This takes an allocator callback 
 * function which will be used for all memory allocations
 * by lua when using this specific "state".  The second
 * argument is an opaque pointer that lua passes to every
 * call to the allocator.
 **)
<*EXTERNAL lua_newstate*> 
PROCEDURE NewState(func: M3LuaBase.AllocFunct; userdata: Ctypes.void_star): State;


<*EXTERNAL lua_close*>
PROCEDURE Close(L: State);


<*EXTERNAL lua_newthread*>
PROCEDURE NewThread(L: State) : State;

<*EXTERNAL lua_atpanic*>
PROCEDURE AtPanic(L: State; panicFunc: CallbackFunction);
 

(* ---------------------------------------------------
 * Basic Stack Manipulation: Index positions start at
 * one when viewed as an array.  Use negative index 
 * positions when referenced to the stack top, so an
 * index of -1 (negative one) refers to the top element.
 * ------------------------------------------------ *)

(**
 * GetTop - Returns the index of the top element.  
 * Since indices in a Lua table start with one (1),
 * this also indicates the number of elements on the
 * stack.  Zero (0) means an empty stack.
 **)
<*EXTERNAL lua_gettop*> 
PROCEDURE GetTop(L: State): Ctypes.int;


(**
 * SetTop - Changes the top of the stack to [idx],
 * garbage collecting any elements that were in slots
 * above the new top, and setting to nil, any new slots.
 **)
<*EXTERNAL lua_settop*> 
PROCEDURE SetTop(L: State; idx: Ctypes.int);

(**
 * PushValue - Push a copy of the value at the specified index [idx], 
 * onto the stack (ie, it duplicates the [idx] element).
 **)
<*EXTERNAL lua_pushvalue*> 
PROCEDURE PushValue(L: State; idx: Ctypes.int);

(**
 * Remove - remove an element at index position [idx], all upper elements
 * are shifted down.
 **)
<*EXTERNAL lua_remove*>
PROCEDURE Remove(L: State; idx: Ctypes.int);

(**
 * Insert - Move the top element to the specified index [idx] position,
 * shifting any elements up the stack as required.
 **)
<*EXTERNAL lua_insert*>
PROCEDURE Insert(L: State; idx: Ctypes.int);

(**
 * Replace - Move the top element to the specifed index [idx] position,
 * no shifting is performed.
 **)
<*EXTERNAL lua_replace*>
PROCEDURE Replace(L: State; idx: Ctypes.int);

(**
 * CheckStack - Ensure there are enough free slots in the stack, growing
 * the stack if necessary.  It will not shrink the stack.
 * RETURNs zero if it cant grow the stack.
 **)
<*EXTERNAL lua_checkstack*>
PROCEDURE CheckStack(L: State; sz: Ctypes.int): Ctypes.int;

(**
 * XMove - Exchange values between different threads of the same
 * global state.  Pops [n] values from state [from], and places 
 * them onto the top of state [to].
 **)
<*EXTERNAL lua_xmove*>
PROCEDURE XMove(from, to: State; n: Ctypes.int);




(* ---------------------------------------------------
 * Access functions (stack -> C) 
 * ------------------------------------------------ *)

(**
 * IsNumber - Checks the type of an element on the stack,
 * Returning true (1), if the element is a number 
 * (double precision floating point).
 **)
<*EXTERNAL lua_isnumber*>
PROCEDURE IsNumber(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * IsString - Checks the type of an element of the stack,
 * Returning true (1), if the element is a string.
 **)
<*EXTERNAL lua_isstring*>
PROCEDURE IsString(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * IsCallbackFunction - Checks the type of an element of the stack,
 * Returning true (1), if the element is a callback function.
 **)
<*EXTERNAL lua_iscfunction*>
PROCEDURE IsCallbackFunction(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * IsUserData - Checks the type of an element of the stack,
 * Returning true (1), if the element is user data either "light"
 * or "full".
 **)
<*EXTERNAL lua_isuserdata*>
PROCEDURE IsUserData(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * Type - Checks the type of an element of the stack,
 * Returning the type code.
 **)
<*EXTERNAL lua_type*>
PROCEDURE Type(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * TypeName - Checs the type of an element of the stack,
 * Uses the typecode, to generate a string representation
 * of the type.
 **)
<*EXTERNAL lua_typename*>
PROCEDURE TypeName(L: State; 
                   idx: Ctypes.int) : Ctypes.const_char_star;

(**
 * Equal - Compares two elements from the stack, returning true (1)
 * if the two elements  are equal according to the semantics of "=="
 * (may call meta-methods), and false (0) if not equal, also return
 * false if either index is bad.
 **)
<*EXTERNAL lua_equal*>
PROCEDURE Equal(L: State; idx1, idx2: Ctypes.int) : Ctypes.int;

(**
 * RawEqual - Compares two elements from the stack, returning true (1)
 * if the two elements are "primitively" equal.  This will not call
 * any meta-method.
 **)
<*EXTERNAL lua_rawequal*>
PROCEDURE RawEqual(L: State; idx1, idx2: Ctypes.int) : Ctypes.int;

(**
 * LessThan - Compares two elements from the stack.
 **)
<*EXTERNAL lua_lessthan*>
PROCEDURE LessThan(L: State; idx1, idx2: Ctypes.int) : Ctypes.int;

(**
 * ToNumber - Converts the stack element at [idx] to a number,
 * which by default is a double precision floating point.
 * On a failure to convert, will return a zero (0.0).
 **)
<*EXTERNAL lua_tonumber*>
PROCEDURE ToNumber(L: State; idx: Ctypes.int) : Number;

(**
 * ToInteger - Converts the Number or String at the stack 
 * position [idx] to an Integer.  If a floating point number
 * is found, it will be truncated to an Integer in an unspecified
 * way.
 **)
<*EXTERNAL lua_tointeger*>
PROCEDURE ToInteger(L: State; idx: Ctypes.int) : Integer;

(**
 * ToBoolean - Converts a stack element to a boolean, 
 * ie a zero (false) or a one (true).  
 * If not a false, or not nil return true.
 * If the index is invalid, return false.
 **)
<*EXTERNAL lua_toboolean*>
PROCEDURE ToBoolean(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * ToLString - Convert the Lua string into a C-string 
 * (ie, an array of characters).  This array may contain
 * null characters, and so one should not assume it can
 * be used with stdlib.  An extra null character is appended
 * after the returned characters to ensure it is a valid
 * c-string.  If [len] is non-NIL, places the number of bytes
 * in the string therein, including any nul characters except
 * for the extra terminal nul.
 **)
<*EXTERNAL lua_tolstring*>
PROCEDURE ToLString(L: State; 
                    idx: Ctypes.int;
                    len: Ctypes.int_star) : Ctypes.const_char_star;


(**
 * ToString - Return the pointer to the first character
 * of a string.  Equilent to ToLString() but using NIL for the [len].
 **)
PROCEDURE ToString(L: State; idx: Ctypes.int): Ctypes.const_char_star;


(**
 * ToCallbackFunction - Converts the valid index into a CallbackFunction
 * by ensuring that the element on the stack at the index position is
 * a CallbackFunction and returing it, of if not a CallbackFunction
 * return a NIL.  Also return NIL if the index is invalid.
 **)
<*EXTERNAL lua_tocfunction*>
PROCEDURE ToCallbackFunction(L: State; idx: Ctypes.int) : CallbackFunction;

(**
 * ToUserData
 **)
<*EXTERNAL lua_touserdata*>
PROCEDURE ToUserData(L: State; idx: Ctypes.int) : Ctypes.void_star;

(**
 * ToThread - Returns a pointer to a thread represented by State,
 * or if [idx] does not reference a thread, return NIL.
 **)
<*EXTERNAL lua_tothread*>
PROCEDURE ToThread(L: State; idx: Ctypes.int) : State;

(**
 * ToPointer - Converts the value on the stack at the index position
 * into a pointer (ie, ADDRESS).  The value may be userdata, a table,
 * thread, or function.  On failure returns NIL.
 **)
<*EXTERNAL lua_topointer*>
PROCEDURE ToPointer(L: State; idx: Ctypes.int) : Ctypes.const_void_star;


(**
 * ObjLen - Return the length of the value at the specifed
 * index.  For strings this is the number of characters in
 * the string.  For tables, this is the number of elements
 * as returned by the "#" operator.  For userdata this is the
 * number of bytes allocated.  All other values return zero.
 **)
<*EXTERNAL lua_objlen*>
PROCEDURE ObjLen(L: State; idx: Ctypes.int) : Cstddef.size_t;



(* ---------------------------------------------------
 * Push functions (C -> Stack) 
 * ------------------------------------------------ *)

(**
 * PushNil - Pushes the Nil value onto the stack.
 **)
<*EXTERNAL lua_pushnil*> 
PROCEDURE PushNil(L: State);

(**
 * PushNumber - Pushes the number [n] onto the stack.
 **)
<*EXTERNAL lua_pushnumber*> 
PROCEDURE PushNumber(L: State; n: Number);

(**
 * PushInteger - Pushes the integer [n] onto the stack.
 **)
<*EXTERNAL lua_pushinteger*> 
PROCEDURE PushInteger(L: State; n: Integer);

(**
 * PushLString - Pushes a string of character with the
 * specified length onto the stack.  This allows strings
 * to contain null as an internal character.
 **)
<*EXTERNAL lua_pushlstring*> 
PROCEDURE PushLString(L: State; 
                      s: Ctypes.const_char_star; 
                      len: Cstddef.size_t);


(**
 * PushString - Pushes a null terminated string onto the stack.
 **)
<*EXTERNAL lua_pushstring*> 
PROCEDURE PushString(L: State; s: Ctypes.char_star);


(**
 * PushCClosure - Pushes a new C closure onto the stack.  
 * Pop [n] elements from the stack, create a closure containing
 * those values and associate it with the callback function [fn],
 * push [fn] onto the stack.
 **)
<*EXTERNAL lua_pushcclosure*> 
PROCEDURE PushCClosure(L: State; 
                       fn: CallbackFunction;
                       n: Ctypes.int);


(**
 * PushCallbackFunction - Pushes a callback function onto the stack,
 * without a closure.
 **)
(* MACRO <*EXTERNAL lua_pushcfunction*> *)
PROCEDURE PushCallbackFunction(L: State; fn: CallbackFunction);


(**
 * PushBoolean - Push a boolean value [b] onto the stack.
 **)
<*EXTERNAL lua_pushboolean*> 
PROCEDURE PushBoolean(L: State; b: Ctypes.int);

(**
 * PushLightUserData - 
 **)
<*EXTERNAL lua_pushlightuserdata*> 
PROCEDURE PushLightUserData(L: State; p: Ctypes.void_star);


(**
 * PushThread - Push the thread represented by [L]
 * onto the stack.  Returns true (1) if this thread
 * is the main thread of its state.
 **)
<*EXTERNAL lua_pushthread*> 
PROCEDURE PushThread(L: State): Ctypes.int;


(* Lua defines some functions that take a variable number
   of arguments.  Modula-3 does not support those.

    Skip functions with variable number of arguments:
        lua_pushvfstring, 
        lua_pushfstring (formated string, similar to printf().
*)




(* ---------------------------------------------------
 * Get functions (Lua -> Stack) 
 * ------------------------------------------------ *)

(**
 * GetTable - Pushes a value from a field of a table 
   onto the stack.  The table is found using [idx], 
   then the key for the field of the table is found 
   on the stack (placed there previously by the user).
   Metamethods may be called on index failures.
 **)
<*EXTERNAL lua_gettable*>
PROCEDURE GetTable(L: State; idx: Ctypes.int);

(**
 * GetField - Similar to GetTable(), but the key to the
   field of the table is supplied in the argument list.
 **)
<*EXTERNAL lua_getfield*>
PROCEDURE GetField(L: State; 
                   idx: Ctypes.int; 
                   key: Ctypes.const_char_star);

(*
 * GetGlobal - Pushes a value from the globals named [name],
 * onto the parameter stack.
 * NOTE: Not external.
 *)
PROCEDURE GetGlobal(L: State; name: Ctypes.const_char_star);

(**
 * Rawget - Similar to gettable, but without using meta-tables.
 **)
<*EXTERNAL lua_rawget*>
PROCEDURE RawGet(L: State; idx: Ctypes.int);

(**
 * RawGetI - Pushes onto the stack the value t[n], where
 * {t} is a table on the stack at index position [idx],
 * and {n} is the index into that table.  This will not
 * invoke any meta-methods.
 **)
<*EXTERNAL lua_rawgeti*>
PROCEDURE RawGetI(L: State; idx, n: Ctypes.int);

(**
 * CreateTable - Create a new empty table an pushes it onto
 * the stack.  Pre allocates [narr] array elements, and 
 * [nrec] non-array elements.  Usefull when the size of
 * the desired table is already known (otherwise use NewTable).
 **)
<*EXTERNAL lua_createtable*>
PROCEDURE CreateTable(L: State; narr, nrec: Ctypes.int);

(**
 * NewTable - Create a new empty table, and push it onto the stack.
 **)
PROCEDURE NewTable(L: State);


(**
 * NewUserData - Allocate a new block of data with the
 * specifed size [sz], and push it onto the stack.
 * Returns a pointer to that new user data.  This is
 * a full user data, with possible associations of meta-tables.
 **)
<*EXTERNAL lua_newuserdata*>
PROCEDURE NewUserData(L: State; sz: Cstddef.size_t)
           : Ctypes.void_star;

(**
 * GetMetaTable - Pushes onto the stack the metatable of the
 * value at the specified index [idx].  If the index is invalid,
 * or the value does not have a metatable, push nothing, and 
 * return zero.
 **)
<*EXTERNAL lua_getmetatable*>
PROCEDURE GetMetaTable(L: State; objIndex: Ctypes.int)
           : Ctypes.int;

(**
 * GetFEnv - Pushes onto the stack the environment
 * table of the value at the specifed index [idx].
 **)
<*EXTERNAL getfenv*>
PROCEDURE GetFEnv(L: State; idx: Ctypes.int);


(* --- Set functions (stack -> Lua) --- *)

(**
 * SetTable - Set a value into a table.  The table is referenced
 * by index [idx].  The value to be inserted is at the top of the
 * stack, and the key into the table is the element below the value.
 * Both value and key are popped from the stack.  May trigger a
 * meta-method for the "newindex" event.
 **)
<*EXTERNAL lua_settable*>
PROCEDURE SetTable(L: State; idx: Ctypes.int);

(**
 * SetField - Similar to SetTable, but the key is supplied as an
 * argument [key].  Pops the value from the stack.
 **)
<*EXTERNAL lua_setfield*>
PROCEDURE SetField(L: State; 
                   idx: Ctypes.int;
                   key: Ctypes.const_char_star);

(**
 * RawSet - Similar to SetTable, but without invoking a
 * metamethod.
 **)
<*EXTERNAL lua_rawset*>
PROCEDURE RawSet(L: State; idx: Ctypes.int);

(**
 * RawSetI - Similar to setfield, but without invoking
 * a metamethod.
 **)
<*EXTERNAL lua_rawseti*>
PROCEDURE RawSetI(L: State; idx, n: Ctypes.int);

(**
 * SetMetaTable - Pops a table from the stack, and sets
 * it as the new metatable for the value at the specified
 * index [idx].
 **)
<*EXTERNAL lua_setmetatable*>
PROCEDURE SetMetaTable(L: State; objIndex: Ctypes.int);

(**
 * SetFEnv - Pops a table from the stack, and sets it as the
 * new environment for the value at the specifed index [idx].
 **)
<*EXTERNAL lua_setfenv*>
PROCEDURE SetFEnv(L: State; idx: Ctypes.int);


(* --- `load' and `call' functions (load and run Lua code) --- *) 

(**
 * Call - Calls a function.  Client first pushes the function
 * to be called, then the arguments (in direct order: first 
 * arg first).  These (function and [nargs] values) are all 
 * popped from the stack during the "call".
 * After the "call", [nresults] results are placed
 * on the stack with the last result on the top.  These will
 * be adjusted as necessary to make it [nresults].
 * If the client specifies MultipleReturnCode for [nresults],
 * then all the results are placed on the stack, no adjustment
 * will be made.
 **)
<*EXTERNAL lua_call*>
PROCEDURE Call(L: State; nargs, nresults: Ctypes.int);

(**
 * ProtectedCall - Calls a function in protected mode.
 * If an error occurs during the call, the error is caught,
 * and a single error message is placed on the stack, and
 * the return value is the error code.  
 * If [errFunc] is non-zero it is the stack index of an 
 * error handler function.
 **)
<*EXTERNAL lua_pcall*>
PROCEDURE ProtectedCall(L: State; 
                nargs, nresults, errFunc: Ctypes.int)
           : Ctypes.int;

(**
 * CPCall - Calls a CallbackFunction in protected mode.
 * [funct] starts with only one element on the stack, the
 * light userdata.  This does not support an error handler,
 * but it will on error place the error message on the stack
 * and return an error code.
 **)
<*EXTERNAL lua_cpcall*>
PROCEDURE CPCall(L: State; 
                 funct: CallbackFunction; 
                 ud: Ctypes.void_star) : Ctypes.int;

(**
 * Load - Loads a Lua chunk.  Upon success, pushes the compiled
 * chunk as a lua function onto the top of the stack.  On failure
 * it places an error message, and returns a non-zero error code.
 **)
<*EXTERNAL lua_load*>
PROCEDURE Load(L: State; 
               reader: M3LuaBase.ReaderFunct;
               data: Ctypes.void_star;
               chunkName: Ctypes.const_char_star) : Ctypes.int;


(**
 * Dump - Dumps a function as a binary chunk.  Pops a function
 * from the top of the stack, and writes the function's byte code
 * to the writer.  Returns the error code of the last call to writer().
 **)
<*EXTERNAL lua_dump*>
PROCEDURE Dump(L: State; 
               writer: M3LuaBase.WriterFunct;
               userdata: Ctypes.const_char_star) : Ctypes.int;



(* --- Co-routine support functions --- *)

(**
 * Yield - Yield execution to a co-routine.  This should
 * only be called as the return expression of a callback function.
 **)
<*EXTERNAL lua_yield*>
PROCEDURE Yield(L: State; nresults: Ctypes.int) : Ctypes.int;


(**
 * Resume - Starts and resumes a coroutine in a given thread.
 * After the client has created a thread, and pushing the
 * main function, it should resume the new thread.  This
 * call will return when the thread is either suspended,
 * or finishs execution.
 * Returns:
 *    - StatusOkay on finished execution, or
 *    - StatusYield if it yielded, and requires more execution.
 *    - Other error code on an error.
 **)
<*EXTERNAL lua_resume*>
PROCEDURE Resume(L: State; narg: Ctypes.int)
           : Ctypes.int;

(**
 * Status - Returns the status of a thread.
 **)
<*EXTERNAL lua_status*>
PROCEDURE Status(L: State)
           : Ctypes.int;


(* --- Garbage collection function and options --- *)

(**
 * GabageCollector - Controls the lua garbage collector.
 * Takes one of the garbage collector commands, 
 * gcStop, ..., etc.
 **)
<*EXTERNAL lua_gc*>
PROCEDURE GarbageCollector(L : State; what, data: Ctypes.int) : Ctypes.int;


(* --- Micellaneous Fucntions --- *)

(**
 * Error - Generate a lua error.  Error hander must place
 * an error message on the stack.  This will never return.
 **)
<*EXTERNAL lua_error*>
PROCEDURE Error(L: State) : Ctypes.int;

(**
 * Next - Pop a key from the stack, push a key/value pair 
 * the table at the specified index [idx] (the "next" pair
 * after the specified ones ordinal positon within the table.
 * Allows one to traverse all the key/value pairs contained
 * in a table.
 **)
<*EXTERNAL lua_next*>
PROCEDURE Next(L: State; idx: Ctypes.int) : Ctypes.int;

(**
 * Concat - Pops [n] elements from the stack, concatenates
 * them, and pushes the result onto the stack.  If [n] is
 * zero, push an empty string onto the stack.
 **)
<*EXTERNAL lua_concat*>
PROCEDURE Concat(L: State; n: Ctypes.int);

(**
 * GetAllocFunc - Return a pointer to the memory allocation
 * function associated with this lua state.  If user data [ud]
 * is not NIL then the pointer to the user data associated with
 * the state is also stored in [ud].
 **)
<*EXTERNAL lua_getallocf*>
PROCEDURE GetAllocFunc(L: State; ud: Ctypes.char_star_star)
(* FIXME: void_star_star    ud: Ctypes.char_star_star) *)
           : M3LuaBase.AllocFunct;

(**
 * SetAllocFunc - Set the memory allocator function associated
 * with the specified state.  
 **)
<*EXTERNAL lua_setallocf*>
PROCEDURE SetAllocFunc(L: State; 
                       allocFunc: M3LuaBase.AllocFunct;
                       ud: Ctypes.void_star);



(* --- Macros, reimplemented as function in the *.m3 file --- *)

(**
 * Pop - simply removes [n] elements from the stack.
 **)
PROCEDURE Pop(L: State; n: INTEGER);

(**
 * Register - Creates a named global variable containing a value pointing
 * to a callback function.
 **)
PROCEDURE Register(L: State; name: Ctypes.const_char_star; cfunc: CallbackFunction);

(**
 * StrLen - Returns the number of characters in the string at the specified
 * position on the stack.
 **)
PROCEDURE StrLen(L: State; idx: INTEGER): INTEGER;


(**
 * IsFunction - Tests if the element at the specified position of the
 * stack is a pointer to a function.
 **)
PROCEDURE IsFunction(L: State; idx: INTEGER): BOOLEAN;

(**
 * IsCallbackFunction - Tests if the element at the specified position of the
 * stack is a pointer to a callback function.
 **)
(* PROCEDURE IsCallbackFunction(L: State; idx: INTEGER): BOOLEAN; *)

(**
 * IsTable - Tests if the element at the specified position of the
 * stack is a table.
 **)
PROCEDURE IsTable(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsLightUserData - Tests if the element at the specified position
 * of the stack is a "light" user data.
 **)
PROCEDURE IsLightUserData(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsNil - Tests if the element at the specified position
 * of the stack is the special value Nil.
 **)
PROCEDURE IsNil(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsBoolean - Tests if the element at the specified position
 * of the stack is a value of type boolean.
 **)
PROCEDURE IsBoolean(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsThread - Tests the element at the specified position
 * of the stack is a "thread".
 **)
PROCEDURE IsThread(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsNone - 
 **)
PROCEDURE IsNone(L: State; idx: INTEGER): BOOLEAN;


(**
 * IsNoneOrNil -
 **)
PROCEDURE IsNoneOrNil(L: State; idx: INTEGER): BOOLEAN;


(**
 * PushLiteral - 
 **)
PROCEDURE PushLiteral(L: State; str: Ctypes.const_char_star);


(**
 * SetGlobal - Pop a value from the stack and set it as the
 * value of the global with the specified string [name].
 **)
PROCEDURE SetGlobal(L: State; name: Ctypes.const_char_star);
(* PROCEDURE GetGlobal(L: State; name: Ctypes.char_star); *)



(**
 * GetRegistry - Pushes the table containing all the global
 * values onto the stack.
 **)
PROCEDURE GetRegistry(L: State);

(**
 * GetGcCount - Determine the number of bytes in use by lua.
 **)
PROCEDURE GetGcCount(L: State): Ctypes.int;


END M3LuaRaw.
