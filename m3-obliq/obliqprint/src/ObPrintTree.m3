(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObPrintTree;
IMPORT TextConv, ObCommand, SynWr, Text, Fmt, ObTree, ObLib;

  VAR
    printAlphaDecor: BOOLEAN;
    printVarIndex: BOOLEAN;
    printVariant: BOOLEAN;

  PROCEDURE Setup()  =
  BEGIN

    printVarIndex := FALSE;
    ObCommand.Register(ObTree.doCommandSet,
      NEW(ObCommand.T, name:="ShowVarIndex", sortingName:="ShowVarIndex",
	Exec:=PrintVarIndex));

    printAlphaDecor := FALSE;
    ObCommand.Register(ObTree.doCommandSet,
      NEW(ObCommand.T, name:="ShowVarAlphaDecor",sortingName:="ShowAlphaDecor",
	Exec:=PrintAlphaDecor));

    printVariant := FALSE;
    ObCommand.Register(ObTree.doCommandSet,
      NEW(ObCommand.T, name:="ShowVarVariant", sortingName:="ShowVarVariant",
	Exec:=PrintVariant));

  END Setup;

  PROCEDURE FetchDecoration(name: ObTree.IdeName; env: ObTree.Env): INTEGER  =
  BEGIN
    LOOP
      IF env=NIL THEN RETURN -1 END;
      IF ObTree.SameIdeName(env.name, name) THEN RETURN env.decoration END;
      env := env.rest;
    END;
  END FetchDecoration;

  PROCEDURE PrintDecoration(swr: SynWr.T; decoration: INTEGER)  =
  BEGIN
    SynWr.Text(swr, FmtDecoration(decoration));
  END PrintDecoration;

  PROCEDURE PrintIdeName(swr: SynWr.T; name: ObTree.IdeName; env: ObTree.Env)  =
  BEGIN
    SynWr.Beg(swr);
    SynWr.Text(swr, name.text);
    IF printAlphaDecor THEN
      PrintDecoration(swr, FetchDecoration(name, env));
    END;
    IF printVariant THEN
      IF name.variant#0 THEN
	SynWr.Text(swr, "%" & Fmt.Int(name.variant));
      END;
    END;
    SynWr.End(swr);
  END PrintIdeName;

  PROCEDURE PrintIdePlace(swr: SynWr.T; place: ObTree.IdePlace)  =
  BEGIN
    SynWr.Beg(swr);
    TYPECASE place OF
    | ObTree.IdePlaceLocal(node) =>
        SynWr.Text(swr, "L" & Fmt.Int(node.index));
    | ObTree.IdePlaceGlobal(node) =>
        SynWr.Text(swr, "G" & Fmt.Int(node.index));
    ELSE
      <*ASSERT FALSE*>  (*shouldn't happen*)
    END;
    SynWr.End(swr);
  END PrintIdePlace;

  PROCEDURE PrintIde(swr: SynWr.T; name: ObTree.IdeName; place: ObTree.IdePlace; 
    env: ObTree.Env)  =
  BEGIN
    SynWr.Beg(swr);
    PrintIdeName(swr, name, env);
    IF printVarIndex THEN
      SynWr.Char(swr, '_');
      PrintIdePlace(swr, place);
    END;
    SynWr.End(swr);
  END PrintIde;

  PROCEDURE PrintIdeList(swr: SynWr.T; list: ObTree.IdeList; env: ObTree.Env): ObTree.Env =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    LOOP
      TYPECASE list OF
      | NULL => EXIT;
      | ObTree.IdeList(node) =>
          env := ObTree.NewEnv(node.first, env);
	  SynWr.Text(swr, sep); sep := ",";
          SynWr.FlatBreak(swr);
	  SynWr.Beg(swr, 2);
	    PrintIdeName(swr, node.first, env);
	  SynWr.End(swr);
          list := node.rest;
      END;
    END;
    RETURN env; 
  END PrintIdeList;
  
  PROCEDURE FmtDecoration(decoration: INTEGER): TEXT  =
  VAR res: TEXT;
  BEGIN
    IF decoration<=0 THEN RETURN "" END;
    res := "";
    LOOP
      CASE decoration MOD 4 OF
      | 1 => res := "\'" & res;
      | 2 => res := "\"" & res;
      | 3 => res := "^" & res;
      | 0 => res := "~" & res;
      ELSE <*ASSERT FALSE*> (* can't happen! *)
      END;
      decoration := (decoration-1) DIV 4;
      IF decoration = 0 THEN EXIT END;
    END;
    RETURN res;
  END FmtDecoration;

  PROCEDURE FmtIdeName(name: ObTree.IdeName; env: ObTree.Env): TEXT  =
  VAR text: TEXT;
  BEGIN
    text := name.text;
    IF printAlphaDecor THEN
      text:=text & FmtDecoration(ObTree.FreshDecoration(name, env));
    END;
    IF printVariant THEN
      IF name.variant>0 THEN
	text:=text & "%";
	text := text & Fmt.Int(name.variant);
      END;
    END;
    RETURN text;
  END FmtIdeName;

  PROCEDURE FmtIdePlace(place: ObTree.IdePlace): TEXT  =
  BEGIN
    TYPECASE place OF
    | ObTree.IdePlaceLocal(node) =>
        RETURN "L" & Fmt.Int(node.index);
    | ObTree.IdePlaceGlobal(node) =>
         RETURN "L" & Fmt.Int(node.index);
    ELSE <*ASSERT FALSE*> (* shouldn't happen *)
    END;
  END FmtIdePlace;

  PROCEDURE FmtIde(name: ObTree.IdeName; place: ObTree.IdePlace; env: ObTree.Env): TEXT  =
  VAR text: TEXT;
  BEGIN
    text := FmtIdeName(name, env);
    IF printVarIndex THEN
      text := text & "_" & FmtIdePlace(place);
    END;
    RETURN text;
  END FmtIde;

  PROCEDURE PrintTermBinding(swr: SynWr.T; <*UNUSED*>rec: BOOLEAN; 
                             binding: ObTree.TermBinding; 
    libEnv: ObLib.Env; env: ObTree.Env; depth: INTEGER)  =
  (* -- The env stuff is correct for sequential bindings, not for recursive
     ones *)
  VAR newEnv: ObTree.Env; sep: TEXT;
  BEGIN
    sep := "";
    newEnv := env;
    LOOP
      TYPECASE binding OF
      | NULL => EXIT;
      | ObTree.TermBinding(node) =>
	  SynWr.Text(swr, sep); sep := ", ";
          SynWr.Break(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
	      newEnv := ObTree.NewEnv(node.binder, newEnv);
	      PrintIdeName(swr, node.binder, newEnv);
	      SynWr.Text(swr, " = ");
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.term, libEnv, env, depth-1);
	  SynWr.End(swr);
          binding := node.rest;
      END;
    END;
  END PrintTermBinding;

  PROCEDURE PrintProtected(swr: SynWr.T; protected: BOOLEAN) =
  BEGIN
    IF protected THEN
      SynWr.Break(swr);
      SynWr.Beg(swr, 2);
      SynWr.Text(swr, "protected, "); 
      SynWr.End(swr);
    END;
  END PrintProtected;

  PROCEDURE PrintSerialized(swr: SynWr.T; sync: ObTree.Sync) =
  BEGIN
    CASE sync OF
    | ObTree.Sync.Monitored => 
      SynWr.Break(swr);
      SynWr.Beg(swr, 2);
      SynWr.Text(swr, "serialized, ");
      SynWr.End(swr);
    ELSE
    END;
  END PrintSerialized;

  PROCEDURE PrintObjFields(swr: SynWr.T; fields: ObTree.TermObjFields; 
                           libEnv: ObLib.Env; env: ObTree.Env; 
    depth: INTEGER)  =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    LOOP
      TYPECASE fields OF
      | NULL => EXIT;
      | ObTree.TermObjFields(node) =>
	  SynWr.Text(swr, sep); sep := ", ";
          SynWr.Break(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
	      PrintIdeName(swr, node.label, env);
	      SynWr.Text(swr, " => ");
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.term, libEnv, env, depth-1);
	  SynWr.End(swr);
	  fields := node.rest;
      END;
    END;
  END PrintObjFields;

  PROCEDURE PrintTermList(swr: SynWr.T; list: ObTree.TermList; libEnv: ObLib.Env; env: ObTree.Env;
    depth: INTEGER)  =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    LOOP
      TYPECASE list OF
      | NULL => EXIT;
      | ObTree.TermList(node) =>
	  SynWr.Text(swr, sep); sep := ", ";
          SynWr.Break(swr);
	  SynWr.Beg(swr, 2);
	    PrintTerm(swr, node.first, libEnv, env, depth-1);
	  SynWr.End(swr);
	  list := node.rest;
      END;
    END
  END PrintTermList;

  PROCEDURE PrintCaseList(swr: SynWr.T; list: ObTree.TermCaseList; libEnv: ObLib.Env; env: ObTree.Env;
    depth: INTEGER)  =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    LOOP
      TYPECASE list OF
      | NULL => EXIT;
      | ObTree.TermCaseList(node) =>
	  SynWr.Text(swr, sep); sep := ", ";
          SynWr.Break(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
              IF node.pattern=NIL THEN
                SynWr.Text(swr, "else ");
              ELSE
	        PrintTerm(swr, node.pattern, libEnv, env, depth-1);
                IF node.binder # NIL THEN
	          SynWr.Char(swr, '(');
	          PrintIdeName(swr, node.binder, env);
                  IF node.binderMatch # NIL THEN
                    SynWr.Text(swr, ", ");
                    SynWr.Break(swr);
                    PrintIdeName(swr, node.binderMatch, env);
                  END;
	          SynWr.Char(swr, ')');
                END;
	        SynWr.Text(swr, " => ");
              END;
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, env, depth-1);
	  SynWr.End(swr);
	  list := node.rest;
      END;
    END;
  END PrintCaseList;

  PROCEDURE PrintTryList(swr: SynWr.T; list: ObTree.TermTryList; libEnv: ObLib.Env; env: ObTree.Env;
    depth: INTEGER)  =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    LOOP
      TYPECASE list OF
      | NULL => EXIT;
      | ObTree.TermTryList(node) =>
	  SynWr.Text(swr, sep); sep := ", ";
          SynWr.Break(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
              IF node.exception=NIL THEN
                SynWr.Text(swr, "else ");
              ELSE
	        PrintTerm(swr, node.exception, libEnv, env, depth-1);
	        SynWr.Text(swr, " => ");
              END;
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.recover, libEnv, env, depth-1);
	  SynWr.End(swr);
	  list := node.rest;
      END;
    END;
  END PrintTryList;

  PROCEDURE PrintOk(swr: SynWr.T)  =
  BEGIN
    SynWr.Text(swr, "ok");
  END PrintOk;

  PROCEDURE PrintChar(swr: SynWr.T; char: CHAR)  =
  VAR charsOut: ARRAY[0..3] OF CHAR; avail: INTEGER;
  BEGIN
    avail := TextConv.EncodeChar(char, (*out*)charsOut);
    SynWr.Beg(swr);
    SynWr.Char(swr, '\'');
    FOR i:=0 TO avail-1 DO
      SynWr.Char(swr, charsOut[i]);
    END;
    SynWr.Char(swr, '\'');
    SynWr.End(swr);
  END PrintChar;

  PROCEDURE PrintText(swr: SynWr.T; text: TEXT)  =
  BEGIN
    SynWr.Beg(swr);
    SynWr.Text(swr, TextConv.Encode(text, TRUE));
    SynWr.End(swr);
  END PrintText;

  PROCEDURE PrintBool(swr: SynWr.T; bool: BOOLEAN)  =
  BEGIN
    SynWr.Text(swr, ObTree.FmtBool(bool));
  END PrintBool;

  PROCEDURE PrintInt(swr: SynWr.T; int: INTEGER)  =
  BEGIN
    SynWr.Text(swr, ObTree.FmtInt(int));
  END PrintInt;

  PROCEDURE PrintReal(swr: SynWr.T; real: LONGREAL)  =
  BEGIN
    SynWr.Text(swr, ObTree.FmtReal(real));
  END PrintReal;

  PROCEDURE PrintSignature(swr: SynWr.T; term: ObTree.Term; 
                           <*UNUSED*>libEnv: ObLib.Env;
                      env: ObTree.Env)  =
  VAR newEnv: ObTree.Env;
  BEGIN
    TYPECASE term OF
    | NULL => SynWr.Text(swr, "<nil term>");
    | ObTree.TermFun(node) =>
	  SynWr.Beg(swr);
	    SynWr.Beg(swr, 2);
              SynWr.Text(swr, "proc(");
	      newEnv := PrintIdeList(swr, node.binders, env);
              SynWr.Text(swr, ")...end");
	    SynWr.End(swr);
          SynWr.End(swr);
    | ObTree.TermMeth(node) =>
	  SynWr.Beg(swr);
	    SynWr.Beg(swr, 2);
              IF node.update THEN SynWr.Text(swr, "umeth("); 
              ELSE SynWr.Text(swr, "meth("); END;
	      newEnv := PrintIdeList(swr, node.binders, env);
              SynWr.Text(swr, ")...end");
	    SynWr.End(swr);
          SynWr.End(swr);
    ELSE
	SynWr.Text(swr, "<?>");
    END;

  END PrintSignature;

  PROCEDURE PrintTerm(swr: SynWr.T; term: ObTree.Term; libEnv: ObLib.Env;
                      env: ObTree.Env; depth: INTEGER)  =
  VAR newEnv: ObTree.Env; pkgName: TEXT;
  BEGIN
    TYPECASE term OF
    | NULL => SynWr.Text(swr, "<nil term>");
    | ObTree.TermIde(node) =>
	PrintIde(swr, node.name, node.place, env);
    | ObTree.TermOk => 
        PrintOk(swr);
    | ObTree.TermBool(node) => 
        PrintBool(swr, node.bool);
    | ObTree.TermChar(node) => 
        PrintChar(swr, node.char);
    | ObTree.TermText(node) =>
        PrintText(swr, node.text);
    | ObTree.TermInt(node) =>
        PrintInt(swr, node.int);
    | ObTree.TermReal(node) =>
        PrintReal(swr, node.real);
    | ObTree.TermArray(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr, 1);
          CASE node.semantics OF
          | ObTree.SharingSemantics.Remote =>
            SynWr.Char(swr, '[');
          | ObTree.SharingSemantics.Replicated =>
            SynWr.Text(swr, "replicated [");
          | ObTree.SharingSemantics.Simple =>
            SynWr.Text(swr, "simple [");
          END;
          PrintTermList(swr, node.elems, libEnv, env, depth);
          SynWr.Char(swr, ']');
	SynWr.End(swr);
    | ObTree.TermOption(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "option ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
              PrintTerm(swr, node.tag, libEnv, env, depth-1);
	      SynWr.Text(swr, " => ");
            SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.term, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
	  SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermAlias(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "alias ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      PrintIdeName(swr, node.label, env);
	      SynWr.Text(swr, " of ");
            SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.term, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
	  SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermOp(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        IF (node.argsNo=2) AND
           (ObLib.LookupFixity(node.op.text, libEnv, (*out*)pkgName)=
             ObLib.OpFixity.Infix)
        THEN
	   SynWr.Beg(swr, 2);
             SynWr.Char(swr, '(');
             PrintTerm(swr, node.args.first, libEnv, env, depth-1);
             SynWr.Char(swr, ' ');
           SynWr.Break(swr);
             SynWr.Text(swr, node.op.text);        
             SynWr.Char(swr, ' ');
             PrintTerm(swr, node.args.rest.first, libEnv, env, depth-1);
             SynWr.Char(swr, ')');
           SynWr.End(swr);
        ELSIF (node.argsNo=1) AND
           (ObLib.LookupFixity(node.op.text, libEnv, (*out*)pkgName)=
             ObLib.OpFixity.Prefix)
        THEN
	  SynWr.Beg(swr, 2);
            SynWr.Text(swr, node.op.text);
            SynWr.Char(swr, '(');
	    PrintTermList(swr, node.args, libEnv, env, depth);
            SynWr.Char(swr, ')');
	  SynWr.End(swr);
        ELSIF node.argsNo>=0 THEN
	  SynWr.Beg(swr, 2);
	   SynWr.Beg(swr, 4);
              SynWr.Text(swr, node.pkg.text);
              SynWr.Char(swr, '_');
	    SynWr.Break(swr);
              SynWr.Text(swr, node.op.text);
              SynWr.Char(swr, '(');
	    SynWr.End(swr);
	    PrintTermList(swr, node.args, libEnv, env, depth);
            SynWr.Char(swr, ')');
	  SynWr.End(swr);
        ELSE
	 SynWr.Beg(swr, 2);
            SynWr.Text(swr, node.pkg.text);
            SynWr.Char(swr, '_');
	  SynWr.Break(swr);
            SynWr.Text(swr, node.op.text);
	  SynWr.End(swr);
        END;
    | ObTree.TermFun(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
              SynWr.Text(swr, "proc(");
	      newEnv := PrintIdeList(swr, node.binders, env);
              SynWr.Text(swr, ") ");
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, newEnv, depth-1);
            SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermMeth(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
	  SynWr.Beg(swr, 2);
	    SynWr.Beg(swr, 4);
              IF node.update THEN SynWr.Text(swr, "umeth("); 
              ELSE SynWr.Text(swr, "meth("); END;
	      newEnv := PrintIdeList(swr, node.binders, env);
              SynWr.Text(swr, ") ");
	    SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, newEnv, depth-1);
            SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermObj(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
    	SynWr.Beg(swr, 1); 
    	SynWr.Char(swr, '{');
        PrintProtected(swr, node.protected);
        PrintSerialized(swr, node.sync);
	PrintObjFields(swr, node.fields, libEnv, env, depth);
    	SynWr.Char(swr, '}');
    	SynWr.End(swr);
    | ObTree.TermClone(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "clone(");
	SynWr.Break(swr);
	  PrintTermList(swr, node.objs, libEnv, env, depth);
          SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermNotify(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "notify ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.obj, libEnv, env, depth);
	  SynWr.Char(swr, ' ');
	SynWr.Break(swr);
	  SynWr.Text(swr, " with ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.withObj, libEnv, env, depth);
	SynWr.End(swr);
    | ObTree.TermPickler(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "setpickler(");
	SynWr.Break(swr);
	  PrintTerm(swr, node.obj, libEnv, env, depth);
	  SynWr.Text(swr, ", ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.pklIn, libEnv, env, depth);
	  SynWr.Text(swr, ", ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.pklOut, libEnv, env, depth);
          SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermReplicate(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "replicate(");
	SynWr.Break(swr);
	  PrintTermList(swr, node.args, libEnv, env, depth);
          SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermRemote(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "remote(");
	SynWr.Break(swr);
	  PrintTerm(swr, node.obj, libEnv, env, depth);
	  SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermSimple(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "simple(");
	SynWr.Break(swr);
	  PrintTerm(swr, node.obj, libEnv, env, depth);
	  SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermRedirect(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "redirect ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.obj, libEnv, env, depth);
	  SynWr.Char(swr, ' ');
	SynWr.Break(swr);
	  SynWr.Text(swr, " to ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.toObj, libEnv, env, depth);
	  SynWr.Char(swr, ' ');
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermSelect(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  PrintTerm(swr, node.obj, libEnv, env, depth-1);
	  SynWr.Char(swr, '.');
          (* PrintInt(swr, node.labelIndexHint); *)
	SynWr.Break(swr);
	  PrintIdeName(swr, node.label, env);
	  IF node.args#NIL THEN
	    SynWr.Char(swr, '(');
	    PrintTermList(swr, node.args, libEnv, env, depth);
	    SynWr.Char(swr, ')');
	  END;
	SynWr.End(swr);
    | ObTree.TermUpdate(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  PrintTerm(swr, node.obj, libEnv, env, depth-1);
	SynWr.Break(swr);
	  SynWr.Char(swr, '.');
          (* PrintInt(swr, node.labelIndexHint); *)
	  PrintIdeName(swr, node.label, env);
	  SynWr.Text(swr, " := ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.term, libEnv, env, depth-1);
	SynWr.End(swr);
    | ObTree.TermAppl(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  PrintTerm(swr, node.fun, libEnv, env, depth-1);
	  SynWr.Char(swr, '(');
	  PrintTermList(swr, node.args, libEnv, env, depth);
	  SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermSeq(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  PrintTerm(swr, node.before, libEnv, env, depth-1);
	  SynWr.Text(swr, "; ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.after, libEnv, env, depth-1);
	SynWr.End(swr);
    | ObTree.TermLet(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
          IF node.var THEN 
            SynWr.Text(swr, "var ");
            CASE node.semantics OF
            | ObTree.SharingSemantics.Remote =>
            | ObTree.SharingSemantics.Replicated =>
              SynWr.Text(swr, "replicated ");
            | ObTree.SharingSemantics.Simple =>
              SynWr.Text(swr, "simple ");
            END;
          ELSE 
            SynWr.Text(swr, "let ")
          END;
          PrintTermBinding(swr, node.rec, node.binding, libEnv, env, depth);
	SynWr.End(swr);  
    | ObTree.TermAssign(node) =>  
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;  
	SynWr.Beg(swr, 2);
	  PrintIde(swr, node.name, node.place, env);
	  SynWr.Text(swr, " := ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.val, libEnv, env, depth-1);
	SynWr.End(swr);
    | ObTree.TermIf(node) =>   
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "if ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.test, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "then ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.ifTrue, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
        IF node.ifFalse # NIL THEN
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "else ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.ifFalse, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
        END;
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermCase(node) =>   
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "case ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.option, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "of ");
          SynWr.Break(swr);
            PrintCaseList(swr, node.caseList, libEnv, env, depth);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermLoop(node) =>   
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "loop ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.loop, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermExit =>  
	SynWr.Text(swr, "exit"); 
    | ObTree.TermFor(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
            SynWr.Text(swr, "for ");
          SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      newEnv := ObTree.NewEnv(node.binder, env);
	      PrintIdeName(swr, node.binder, newEnv);
	      SynWr.Text(swr, " = ");
	    SynWr.Break(swr);
	      PrintTerm(swr, node.lb, libEnv, env, depth-1);
              SynWr.Char(swr, ' ');
            SynWr.End(swr);
	  SynWr.Break(swr);
            SynWr.Text(swr, "to ");
	    PrintTerm(swr, node.ub, libEnv, env, depth-1);
            SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
            SynWr.Text(swr, "do ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, env, depth-1);
            SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Text(swr, "end");
	SynWr.End(swr);  
    | ObTree.TermForeach(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
            SynWr.Text(swr, "for ");
          SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      newEnv := ObTree.NewEnv(node.binder, env);
	      PrintIdeName(swr, node.binder, newEnv);
	      SynWr.Text(swr, " in ");
	    SynWr.Break(swr);
	      PrintTerm(swr, node.range, libEnv, env, depth-1);
              SynWr.Char(swr, ' ');
            SynWr.End(swr);
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
            IF node.map THEN SynWr.Text(swr, "map ") ELSE SynWr.Text(swr, "do ") END;
          SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, env, depth-1);
            SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Text(swr, "end");
	SynWr.End(swr);  
    | ObTree.TermException(node) =>  
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "exception");
	  SynWr.Char(swr, '(');
	SynWr.Break(swr);
	  PrintTerm(swr, node.name, libEnv, env, depth-1);
	  SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermRaise(node) =>  
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "raise"); 
	  SynWr.Char(swr, '(');
	SynWr.Break(swr);
	  PrintTerm(swr, node.exception, libEnv, env, depth-1);
	  SynWr.Char(swr, ')');
	SynWr.End(swr);
    | ObTree.TermTry(node) =>   
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "try ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "except ");
          SynWr.Break(swr);
            PrintTryList(swr, node.tryList, libEnv, env, depth);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermTryFinally(node) =>   
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "try ");
          SynWr.Break(swr);
	    PrintTerm(swr, node.body, libEnv, env, depth-1);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "finally ");
          SynWr.Break(swr);
            PrintTerm(swr, node.finally, libEnv, env, depth);
	    SynWr.Char(swr, ' ');
          SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObTree.TermWatch(node) =>
        IF depth<=0 THEN SynWr.Text(swr, "..."); RETURN END;
	SynWr.Beg(swr, 2);
	  SynWr.Text(swr, "watch ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.condition, libEnv, env, depth);
	  SynWr.Char(swr, ' ');
	SynWr.Break(swr);
	  SynWr.Text(swr, " until ");
	SynWr.Break(swr);
	  PrintTerm(swr, node.guard, libEnv, env, depth);
	  SynWr.Char(swr, ' ');
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
     ELSE
	SynWr.Text(swr, "<?>");
    END;
  END PrintTerm; 

  PROCEDURE PrintVarIndex(wr: SynWr.T; self: ObCommand.T; 
                          arg: TEXT; <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF printVarIndex THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr, "Off"); END;
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "On") THEN printVarIndex:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printVarIndex:=FALSE;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintVarIndex;

  PROCEDURE PrintVariant(wr: SynWr.T; self: ObCommand.T; arg: TEXT;
                         <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF printVariant THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr , "Off"); END;
	SynWr.NewLine(wr );	
      ELSIF Text.Equal(arg, "On") THEN printVariant:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printVariant:=FALSE;
      ELSE
	SynWr.Text(wr , "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr );
      END;
    END PrintVariant;

  PROCEDURE PrintAlphaDecor(wr: SynWr.T; self: ObCommand.T; arg: TEXT;
                            <*UNUSED*>data: REFANY:=NIL)  =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF printAlphaDecor THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr, "Off"); END;
	SynWr.NewLine(wr);	
      ELSIF Text.Equal(arg, "On") THEN printAlphaDecor:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printAlphaDecor:=FALSE;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END PrintAlphaDecor;

BEGIN
END ObPrintTree.
