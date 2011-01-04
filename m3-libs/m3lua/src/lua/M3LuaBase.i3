(* --------------------------------------------------------------------
 * FILE:     M3LuaBase.i3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Low level bindings for the Lua scripting language,
 *           here are the base types and constants.
 * VERSION:  0.0.1 (07-Sep-2010) PPE
 * ----------------------------------------------------------------- *)

(* ------------------------------------------------------------
   These Modula-3 bindings closely follow the C-API provided in
   the lua.h header file.  A host program links against the
   lua library, registers any data and functions into a lua
   "state", then may run a "lua" program or script.  the functions
   registered by the host program may effect the host program
   itself.  This is a common method of implementing a scripting
   environment for a host program.

   The major concept is the virtual stack by which the Lua 
   environment does impedence matching to foreign environments 
   such as "C" and with this set of bindings "Modula-3".  
   The two big issues are garbage collection, and that the 
   typeless variables can have values of different types.

   Lua has a small number of primitive types, and one structured
   type (the table).  The Lua library also supports user defined
   data, which is not accessed by the Lua system, but is accessable
   to the host program.  

   The table is very flexible, and can perform as an array, 
   record, hash table, or even modules and objects.  Functions 
   are first class anonymous values, and can be stored in 
   variables or the values of a table entry.  A closure is 
   created to contain the environment (local variables) when 
   a function is created.

   Any of the Lua types can be found on the "stack".  The stack
   can be accessed as a simple array, indexed starting at zero 
   (bottom), and going upwards towards positive indices.  Elements
   can be pushed to or popped from the top of the stack.  The top
   element can be accessed using the index of -1, and further
   negative going downwards towards the bottom of the stack.
 * --------------------------------------------------------- *)


INTERFACE M3LuaBase;

IMPORT Ctypes, Cstddef;


CONST
   VersionStr = "Lua 5.1";
   VersionNum = 501;

   MagicSignature = "\033Lua"; (* mark for precompiled code *)

   MultipleReturnCode = -1;

   (* Pseudo-indices *)
   RegistryIndex    = -10000;
   EnvironmentIndex = -10001;
   GlobalsIndex     = -10002;

CONST
   (* --- Error Codes --- *)
   (* thread status; 0 is okay *)
   StatusOkay         = 0;
   StatusYield        = 1;
   ErrorRun           = 2;
   ErrorSyntax        = 3;
   ErrorMemory        = 4;
   ErrorErr           = 5;
   (* FIXME: use enum instead of const integers *)


TYPE
   (* This is an opaque type, and performs the task of an object in an OPPS language. *)
   State = ADDRESS;

  (**
   * A CallbackFunction is a callback function, that you pass to the 
   * Lua library, and it will call the function when it needs to, and
   * providing the function the current "state".  In the Modula-3 bindings 
   * you may extract the "user data" from the state (see interface M3Lua).
   * The callback function gets its arguments from the state's "stack".
   * The return value is the number of results placed on the stack.
   **) 
   CallbackFunction = PROCEDURE(L: State) : INTEGER;

   (**
    * ReadFunct, WriteFunct.
    * The Lua library does not have builtin functions to read and write
    * from files, since there are devices that don't have file systems.
    * A library used by lua may implement file IO, or any thing else, by 
    * using the following two callback functions.  These callback allow
    * lua to read or write new code.
    * functions that read/write blocks when loading/dumping lua chunks 
    **)
   ReaderFunct = PROCEDURE(L: State; 
                           ud: Ctypes.void_star; 
                           sz: Ctypes.int_star): Ctypes.const_char_star;
   WriterFunct = PROCEDURE(L: State; 
                           buf: Ctypes.const_void_star; 
                           sz: Cstddef.size_t;
                           ud : Ctypes.void_star): Ctypes.signed_int;


   (**
    * AllocFunct - A callback function allowing the host program
    *
    * prototype for memory allocation functions *)
   AllocFunct = PROCEDURE(ud: Ctypes.void_star;
                             ptr: Ctypes.void_star;
                             osize: Cstddef.size_t;
                             nsize: Cstddef.size_t) : Ctypes.void_star;

   (**
    * AtPanicFunct - A callback for when lua panics 
    **)
   AtPanicFunct   = PROCEDURE(L: State; panic: CallbackFunction) : CallbackFunction;

CONST
   (* Minimum number of slots available in a Lua Stack *)
   MinStack = 20;


   (* Basic types (TypeCode) *)
   (* NOTE: Minus 1 can't be in a Modula-3 enumeration. *)
   TypeNone = -1;
   TypeNil  = 0;
   TypeBoolean = 1;
   TypeLightUserData = 2;
   TypeNumber = 3;
   TypeString = 4;
   TypeTable = 5;
   TypeFunction = 6;
   TypeUserData = 7;
   TypeThread = 8;

TYPE
   LuaType = Ctypes.int;

TYPE

   (**
    * Lua uses two types of number.  A generic "Number" is normally
    * a floating point number.  But on small devices without floating
    * point hardware, may be defined as a longint.  A integer type is
    * also provided.
    **)
   Number = Ctypes.double;
   Integer = Ctypes.int;


CONST
   (**
    * Garbage collection function.  The Raw (unsafe) interface
    * has a garbage collection dispatch function, which depending
    * upon the constants below will perform different actions.
    **)
	GcStop         = 0;
	GcRestart      = 1;
	GcCollect      = 2;
        GcCount        = 3;
        GcCountB       = 4;
        GcStep         = 5;  (* perform an incremental step *)
        GcSetPause     = 6;
        GcSetStepMult  = 7;


END M3LuaBase.
