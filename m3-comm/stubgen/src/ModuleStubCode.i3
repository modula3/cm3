(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon May 17 14:06:28 PDT 1993 by mjordan    *)
(*      modified on Fri Feb  5 14:31:17 PST 1993 by owicki     *)

INTERFACE ModuleStubCode;

IMPORT Atom, Formatter, RefList, AtomRefTbl, StubCode, StubUtils, Type;

PROCEDURE Header(modWr: Formatter.T; 
                 t: Type.Object; 
                 typeName: Atom.T;
                 objName: Type.Qid; 
                 methods: StubCode.MethodList;
                 lastNewMethod: INTEGER;
                 VAR returnCodes: RefList.T;
                 importList: AtomRefTbl.T);

PROCEDURE Surrogates(modWr: Formatter.T; 
                     t: Type.Object;
                     methods: StubCode.MethodList;
                     lastNewMethod: INTEGER) RAISES {StubUtils.Failure};

PROCEDURE Dispatcher(modWr: Formatter.T; 
                     t: Type.Object; 
                     typeName: Atom.T;
                     methods: StubCode.MethodList;
                     returnCodes: RefList.T) RAISES {StubUtils.Failure};

PROCEDURE OwnerStubs(modWr: Formatter.T;
                     t: Type.Object;
                     methods: StubCode.MethodList;
                     lastNewMethod: INTEGER) RAISES {StubUtils.Failure};

END ModuleStubCode.
