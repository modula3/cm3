(* Copyright (C) 1991, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Fri Jan 14 10:15:13 PST 1994 by kalsow *)
(*      modified on Mon Apr 06 14:13:15 PDT 1992 by muller *)

(* Version 6.2 of TCL *)

(* This file is derived from tcl.h, covered by the following copyright:

 * Copyright 1987, 1990 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *)

UNSAFE INTERFACE TclC;

FROM Ctypes IMPORT char_star, int_star, int, char,
                   double_star, int_star_star, long_star;
IMPORT Word;

CONST
  TCL_OK       = 0;
  TCL_ERROR    = 1;
  TCL_RETURN   = 2;
  TCL_BREAK    = 3;
  TCL_CONTINUE = 4;

  TCL_RESULT_SIZE = 199;

  BRACKET_TERM  = 1;
  DONT_USE_BRACES = 1;
  NO_EVAL       = -1;

VAR (* READONLY *)
  volatile,  static,  dynamic: FreeProc;

CONST
  GLOBAL_ONLY =      1;
  APPEND_VALUE =     2;
  LIST_ELEMENT =     4;
  NO_SPACE =         8;
  TRACE_READS =      16_10;
  TRACE_WRITES =     16_20;
  TRACE_UNSETS =     16_40;
  TRACE_DESTROYED =  16_80;
  INTERP_DESTROYED = 16_100;
  LEAVE_ERR_MSG	=    16_200;

  VARIABLE_UNDEFINED = 8;

TYPE
  FreeProc = PROCEDURE (blockPtr: char_star);

  Interp = RECORD
             result: char_star;
             freeProc: FreeProc;
             errorLine: int; END;

  Interp_star = UNTRACED REF Interp;

  ClientData = Word.T;

  Argv = UNTRACED REF ARRAY [0..255] OF char_star;
  Argv_star = UNTRACED REF Argv;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_AddErrorInfo *>
PROCEDURE AddErrorInfo (interp: Interp_star; message: char_star);

<*EXTERNAL Tcl_SetErrorCode *>
PROCEDURE SetErrorCode (interp: Interp_star;
                        s1, s2, s3, s4, s5: char_star := NIL);

<*EXTERNAL Tcl_UnixError *>
PROCEDURE UnixError (interp: Interp_star): char_star;

(*---------------------------------------------------------------------------*)

TYPE
  CmdBuf = int_star;

<*EXTERNAL Tcl_CreateCmdBuf *>
PROCEDURE CreateCmdBuf (): CmdBuf;

<*EXTERNAL Tcl_DeleteCmdBuf *>
PROCEDURE DeleteCmdBuf (buffer: CmdBuf);

<*EXTERNAL Tcl_AssembleCmd *>
PROCEDURE AssembleCmd (buffer: CmdBuf; string: char_star): char_star;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_Backslash *>
PROCEDURE Backslash (src: char_star; count: int_star): char;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_Concat *>
PROCEDURE Concat (argc: int; argv: Argv): char_star;

(*---------------------------------------------------------------------------*)

TYPE
  CmdProc = PROCEDURE (clientData: ClientData;
                       interp: Interp_star;
                       argc:   int;
                       argv:   Argv): int;

  CmdDeleteProc  = PROCEDURE (clientData: ClientData);

<*EXTERNAL Tcl_CreateCommand *>
PROCEDURE CreateCommand (interp: Interp_star; 
                         cmdName: char_star; 
                         proc: CmdProc;
                         clientData: ClientData;
                         deleteProc: CmdDeleteProc);

<*EXTERNAL Tcl_DeleteCommand *>
PROCEDURE DeleteCommand (interp: Interp_star;
                         cmdName: char_star): int;


(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_CreateInterp *>
PROCEDURE CreateInterp (): Interp_star;

<*EXTERNAL Tcl_DeleteInterp *>
PROCEDURE DeleteInterp (interp: Interp_star);

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_CreatePipeline *>
PROCEDURE CreatePipeline (interp: Interp_star;
                          argc: int;
                          argv: Argv;
                          pidArrayPtr: int_star_star;
                          inPipePtr: char_star;
                          outPipePtr: char_star;
                          errFilePr: char_star): int;

(*---------------------------------------------------------------------------*)

TYPE
  CmdTraceProc = PROCEDURE (clientData: ClientData;
                            interp: Interp_star;
                            level: int;
                            command: char_star;
                            cmdProc: CmdProc;
                            cmdClientData: ClientData;
                            argc: int;
                            argv: Argv);
  Trace = int_star;

<*EXTERNAL Tcl_CreateTrace *>
PROCEDURE CreateTrace (interp: Interp_star;
                       level: int;
                       proc: CmdTraceProc;
                       clientData: ClientData): Trace;

<*EXTERNAL Tcl_DeleteTrace *>
PROCEDURE DeleteTrace (interp: Interp_star; trace: Trace);

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_Eval *>
PROCEDURE Eval (interp: Interp_star;
                cmd: char_star;
                flags: int;
                termPtr: Argv): int;

<*EXTERNAL Tcl_VarEval *>
PROCEDURE VarEval (interp: Interp_star;
                   s1, s2, s3, s4, s5: char_star := NIL): int;

<*EXTERNAL Tcl_EvalFile *>
PROCEDURE EvalFile (interp: Interp_star;
                    fileName: char_star): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_ExprLong *>
PROCEDURE ExprLong (interp: Interp_star; 
                    string: char_star; longPtr: long_star): int;

<*EXTERNAL Tcl_ExprDouble *>
PROCEDURE ExprDouble (interp: Interp_star; 
                      string: char_star; doublePtr: double_star): int;

<*EXTERNAL Tcl_ExprBoolean *>
PROCEDURE ExprBoolean (interp: Interp_star; 
                       string: char_star; booleanPtr: int_star): int;

<*EXTERNAL Tcl_ExprString *>
PROCEDURE ExprString (interp: Interp_star; 
                      string: char_star): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_Fork *>
PROCEDURE Fork (): int;

<*EXTERNAL Tcl_WaitPids *>
PROCEDURE WaitPids (numPids: int; pidPtr, statusPtr: int_star): int;

<*EXTERNAL Tcl_DetachPids *>
PROCEDURE DetachPids (numPids: int; pidPtr: int_star): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_GetInt *>
PROCEDURE GetInt (interp: Interp_star; 
                  string: char_star; intPtr: int_star): int;

<*EXTERNAL Tcl_GetDouble *>
PROCEDURE GetDouble (interp: Interp_star;
		     string: char_star; doublePtr: double_star): int;

<*EXTERNAL Tcl_GetBoolean *>
PROCEDURE GetBoolean (interp: Interp_star; 
		      string: char_star; boolPtr: int_star): int;

(*---------------------------------------------------------------------------*)

TYPE
  HashEntry = RECORD
                nextPtr: HashEntry_star;
                tablePtr: HashTable_star;
                bucketPtr: HashEntry_star_star;
                clientData: ClientData;
                key: Word.T; END;
  HashEntry_star = UNTRACED REF HashEntry;
  HashEntry_star_star = UNTRACED REF HashEntry_star;
  

CONST
  SMALL_HASH_TABLE = 4;

TYPE
  HashTable = RECORD 
                buckets: HashEntry_star_star;
                staticBuckets: ARRAY [0..SMALL_HASH_TABLE-1] OF HashEntry_star;
                numBuckets: int;
                numEntries: int;
                rebuildSize: int;
                downShift: int;
                mask: int;
                keyType: int;
                findProc: PROCEDURE (tablePtr: HashTable_star; 
                                     key: char_star): HashEntry_star;
                createProc: PROCEDURE (tablePtr: HashTable_star; 
                                     key: char_star; 
                                     newPtr: int_star): HashEntry_star; END;

  HashTable_star = UNTRACED REF HashTable;
          
  HashSearch = RECORD
                 tablePtr: HashTable_star;
                 nextIndex: int;
                 nextEntryPtr: HashEntry_star; END;

  HashSearch_star = UNTRACED REF HashSearch;

<*EXTERNAL Tcl_InitHashTable *>
PROCEDURE InitHashTable (tablePtr: HashTable_star; keyType: int);

<*EXTERNAL Tcl_DeleteHashTable *>
PROCEDURE DeleteHashTable (tablePtr: HashTable_star);

<*EXTERNAL Tcl_CreateHashEntry *>
PROCEDURE CreateHashEntry (tablePtr: HashTable_star; 
                           key: char_star; newPtr: int_star): HashEntry_star;

<*EXTERNAL Tcl_DeleteHashEntry *>
PROCEDURE DeleteHashEntry (entryPtr: HashEntry_star);

<*EXTERNAL Tcl_FindHashEntry *>
PROCEDURE FindHashEntry (tablePtr: HashTable_star; 
                         key: char_star): HashEntry_star;

<*EXTERNAL Tcl_GetHashValue *>
PROCEDURE GetHashValue (entryPtr: HashEntry_star): ClientData;

<*EXTERNAL Tcl_SetHashValue *>
PROCEDURE SetHashValue (entryPtr: HashEntry_star; value: ClientData);

<*EXTERNAL Tcl_GetHashKey *>
PROCEDURE GetHashKey (tablePtr: HashTable_star;
                      entryPtr: HashEntry_star): char_star;

<*EXTERNAL Tcl_FirstHashEntry *>
PROCEDURE FirstHashEntry (tablePtr: HashTable_star; 
                          searchPtr: HashSearch_star): HashEntry_star;

<*EXTERNAL Tcl_NextHashEntry *>
PROCEDURE NextHashEntry (searchPtr: HashSearch_star): HashEntry_star;

<*EXTERNAL Tcl_HashStats *>
PROCEDURE HashStats (tablePtr: HashTable_star): char_star;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_InitHistory *>
PROCEDURE InitHistory (interp: Interp_star);

<*EXTERNAL Tcl_RecordAndEval *>
PROCEDURE RecordAndEval (interp: Interp_star; 
                         cmd: char_star; flags: char): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_SetResult *>
PROCEDURE SetResult (interp: Interp_star;
                     string: char_star; freeProc: FreeProc);

<*EXTERNAL Tcl_AppendResult *>
PROCEDURE AppendResult (interp: Interp_star;
			s1, s2, s3, s4, s5: char_star := NIL);

<*EXTERNAL Tcl_AppendElement *>
PROCEDURE AppendElement (interp: Interp_star; string: char_star; noSep: int);

<*EXTERNAL Tcl_ResetResult *>
PROCEDURE ResetResult (interp: Interp_star);

PROCEDURE FreeResult (interp: Interp_star);

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_SetVar *>
PROCEDURE SetVar (interp: Interp_star; 
                  varName, newValue: char_star; flags: int): char_star;

<*EXTERNAL Tcl_SetVar2 *>
PROCEDURE SetVar2 (interp: Interp_star; 
                   name1, name2, newValue: char_star; flags: int): char_star;

<*EXTERNAL Tcl_GetVar *>
PROCEDURE GetVar (interp: Interp_star; 
		  varName: char_star; flags: int): char_star;

<*EXTERNAL Tcl_GetVar2 *>
PROCEDURE GetVar2 (interp: Interp_star; 
		   name1, name2: char_star; flags: int): char_star;

<*EXTERNAL Tcl_UnsetVar *>
PROCEDURE UnsetVar (interp: Interp_star; 
	  	    varName: char_star; flags: int): int;
 
<*EXTERNAL Tcl_UnsetVar2 *>
PROCEDURE UnsetVar2 (interp: Interp_star; 
	  	     name1, name2: char_star; flags: int): int;
 
(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_SplitList *>
PROCEDURE SplitList (interp: Interp_star; 
 		     list: char_star;
		     argcPtr: int_star;
		     argvPtr: Argv_star): int;

<*EXTERNAL Tcl_Merge *>
PROCEDURE Merge (argc: int; argv: Argv): char_star;

<*EXTERNAL Tcl_ScanElement *>
PROCEDURE ScanElement (src: char_star; flagsPtr: int_star): int;

<*EXTERNAL Tcl_ConvertElement *>
PROCEDURE ConvertElement (src, dst: char_star; flags: int): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_StringMatch *>
PROCEDURE StringMatch (string, pattern: char_star): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL Tcl_TildeSubst *>
PROCEDURE TildeSubst (interp: Interp_star; name: char_star): char_star;

(*---------------------------------------------------------------------------*)

TYPE
  VarTraceProc = PROCEDURE (clientData: ClientData;
                            interp: Interp_star;
			    name1, name2: char_star;
                            flags: int): char_star;

<*EXTERNAL Tcl_TraceVar *>
PROCEDURE TraceVar (interp: Interp_star; 
		    varName: char_star;
		    flags: int;
		    proc: VarTraceProc;
		    clientData: ClientData): int;

<*EXTERNAL Tcl_TraceVar2 *>
PROCEDURE TraceVar2 (interp: Interp_star; 
		     name1, name2: char_star;
		     flags: int;
		     proc: VarTraceProc;
		     clientData: ClientData): int;

<*EXTERNAL Tcl_UnTraceVar *>
PROCEDURE UnTraceVar (interp: Interp_star;
                      varName: char_star;
                      flags: int;
                      proc: VarTraceProc;
  		      clientData: ClientData);

<*EXTERNAL Tcl_UnTraceVar2 *>
PROCEDURE UnTraceVar2 (interp: Interp_star;
                       name1, name2: char_star;
                       flags: int;
                       proc: VarTraceProc;
  		       clientData: ClientData);

<*EXTERNAL Tcl_VarTraceInfo *>
PROCEDURE VarTraceInfo (interp: Interp_star; 
			varName: char_star; 
			flags: int;
			proc: VarTraceProc;
			prevClientData: ClientData): int;

<*EXTERNAL Tcl_VarTraceInfo2 *>
PROCEDURE VarTraceInfo2 (interp: Interp_star; 
			 name1, name2: char_star; 
 			 flags: int;
			 proc: VarTraceProc;
			 prevClientData: ClientData): int;

(*---------------------------------------------------------------------------*)

END TclC.
