(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon May 17 13:27:15 PDT 1993 by mjordan    *)
(*      modified on Fri Dec 18 10:01:04 PST 1992 by owicki     *)
(*      modified on Sat Jun 27 15:46:02 PDT 1992 by muller     *)

INTERFACE IntfStubCode;

IMPORT Atom, Formatter, Type, AtomRefTbl, StubCode;

PROCEDURE Header(t: Type.Object; 
                 intWr: Formatter.T; 
                 typeName: Atom.T;
                 objName: Type.Qid; 
                 methods: StubCode.MethodList;
                 lastNewMethod: INTEGER;
                 imports: AtomRefTbl.T);

END IntfStubCode.
