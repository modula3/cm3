(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon May 17 14:14:57 PDT 1993 by mjordan    *)
(*      modified on Wed Feb 10 09:59:01 PST 1993 by owicki     *)
(*      modified on Sat Jun 27 15:46:02 PDT 1992 by muller     *)

MODULE IntfStubCode;

IMPORT Atom, CodeForType, Formatter, AtomRefTbl, StubCode, 
       StubUtils, Type, Wr;

<* FATAL Wr.Failure *>

PROCEDURE Header(t: Type.Object; 
                 intWr: Formatter.T; 
                 typeName: Atom.T;
                 objName: Type.Qid; 
                 methods: StubCode.MethodList;
                 lastNewMethod: INTEGER;
                 imports: AtomRefTbl.T) = 
  BEGIN
    Formatter.PutText(intWr, "INTERFACE " & StubUtils.FileName(typeName) & 
      ";" & Wr.EOL & Wr.EOL);
    CodeForType.ProduceImports(intWr, objName, imports);
    CodeForType.ImportSuperStubs(intWr, methods, lastNewMethod, typeName);
    Formatter.PutText(intWr, "TYPE "); 
    Formatter.Begin(intWr, 1);
    Formatter.PutText(intWr, "Surrogate_" & Atom.ToText(typeName) & " = " &
         Atom.ToText(objName.intf) & "." & Atom.ToText(objName.item) &
         " OBJECT");
    Formatter.NewLine(intWr, freshLine := FALSE);
    Formatter.Begin(intWr, 2);
    Formatter.PutText(intWr, "OVERRIDES");  
    FOR i := 0 TO LAST(methods^) DO
      Formatter.NewLine(intWr, freshLine := FALSE);
      Formatter.PutText(intWr, Atom.ToText(methods[i].name) & " := ");
      IF methods[i].intf # typeName THEN
        Formatter.PutText(intWr, Atom.ToText(methods[i].intf) & ".");
      END;
      Formatter.PutText(intWr, "Surrogate_" & Atom.ToText(methods[i].name) & ";");
    END;
    Formatter.End(intWr);
    Formatter.NewLine(intWr, freshLine := FALSE);
    Formatter.PutText(intWr, "END;");
    Formatter.NewLine(intWr, freshLine := FALSE);
    Formatter.End(intWr);
    Formatter.NewLine(intWr, freshLine := FALSE);
     (* Output procedure headers for method overrides *)
    FOR i := 0 TO lastNewMethod DO
      CodeForType.ProcHeader(intWr, t,
             "Surrogate_" & Atom.ToText(methods[i].name), methods[i].sig);
      Formatter.PutText(intWr, ";" & Wr.EOL & Wr.EOL);
    END;
     (* Output procedure headers for owner stubs *)
    FOR i := 0 TO lastNewMethod DO
      CodeForType.ProcHeader(intWr, t,
             "Stub_" & Atom.ToText(methods[i].name), 
             StubCode.SigForStub(methods[i].sig));
      Formatter.PutText(intWr, ";" & Wr.EOL & Wr.EOL);
    END;
  END Header;
  
BEGIN
END IntfStubCode.
