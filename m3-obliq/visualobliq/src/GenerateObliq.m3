(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:45:34 PST 1995 by kalsow *)
(*      modified on Thu Sep  8 09:35:40 PDT 1994 by bharat *)
(*      modified on Mon Nov  1 14:40:22 PST 1993 by mhb    *)
<* PRAGMA LL *>

MODULE GenerateObliq; 

IMPORT Dialog, FormsVBT, FileWr, Rd, NodeVBT, Pathname,
       Rsrc, Text, Thread, Wr;

<* FATAL Rd.Failure, Thread.Alerted, NodeVBT.InvalidObjectName  *>

VAR
  fv                 : Dialog.T;
  sessionTemplate    : TEXT;
  defnTemplate       : TEXT;
  declarationTemplate: TEXT;
  constructorTemplate: TEXT;

PROCEDURE Initialize () =
  <* FATAL Rsrc.NotFound *>
  BEGIN
    WITH t = Rsrc.Open("sessionTEMPLATE.obl", Dialog.rsrcPath) DO
      sessionTemplate := Rd.GetText(t, LAST(CARDINAL));
      Rd.Close(t)
    END;
    WITH t = Rsrc.Open("defnTEMPLATE.obl", Dialog.rsrcPath) DO
      defnTemplate := Rd.GetText(t, LAST(CARDINAL));
      Rd.Close(t)
    END;
    WITH t = Rsrc.Open("constructorTEMPLATE.obl", Dialog.rsrcPath) DO
      constructorTemplate := Rd.GetText(t, LAST(CARDINAL));
      Rd.Close(t)
    END;
    WITH t = Rsrc.Open("declarationTEMPLATE.obl", Dialog.rsrcPath) DO
      declarationTemplate := Rd.GetText(t, LAST(CARDINAL));
      Rd.Close(t)
    END;

    WITH t = Rsrc.Open("callbackTEMPLATE.obl", Dialog.rsrcPath) DO
      callbackTemplate := Rd.GetText(t, LAST(CARDINAL));
      Rd.Close(t)
    END;
  END Initialize;

PROCEDURE GenerateConstructorCode (current: NodeVBT.FormNode): TEXT =
  VAR constructor: TEXT;
      supportCode : TEXT := "";
  BEGIN
    (* embed SupportCode *)

    (* first do  anchored forms *)
    FOR j := 0 TO current.NoOfChildren - 1 DO
      supportCode := supportCode & current.ChildForms[j].SupportCode
    END;
    constructor :=
      NodeVBT.FindAndReplace(
        constructorTemplate, "SupportCode", supportCode & current.SupportCode);

    (* embed object definitions *)
    constructor :=
      NodeVBT.FindAndReplace(
        constructor, "object-definitions", current.generateObjectDefs());

    (* embed callbacks *)
    constructor :=
      NodeVBT.FindAndReplace(
        constructor, "callbacks", current.generateCallbacks());
    (* embed attachments *)
    constructor := NodeVBT.FindAndReplace(constructor, "Attachments",
                                          current.generateAttachments());

    (* embed Initialization Code *)
    constructor :=
      NodeVBT.FindAndReplace(constructor, "InitializationCode",
                             current.generateInitializationCode());

    RETURN NodeVBT.FAndRAll(constructor, "form", current.name);
  END GenerateConstructorCode;

PROCEDURE WriteTextToFile (text: TEXT; filename: TEXT) =
  VAR writer: Wr.T;
  BEGIN
    TRY
      writer := FileWr.Open(filename);
      Wr.PutText(writer, text);
      Wr.Close(writer);
    EXCEPT
    ELSE
      Dialog.message(fv, "Unable to save to " & filename);
    END;
  END WriteTextToFile;

PROCEDURE SlashQuotes( string: TEXT) : TEXT =
 VAR
   first: INTEGER := Text.FindChar(string, '"');
  BEGIN
    IF first = -1 THEN RETURN string; END;
    RETURN Text.Sub(string, 0, first) & "\\\"" &
           SlashQuotes(Text.Sub(string, first + 1));
  END SlashQuotes;

PROCEDURE SlashSlashes( string: TEXT) : TEXT =
 VAR
   first: INTEGER := Text.FindChar(string, '\\');
  BEGIN
    IF first = -1 THEN RETURN string; END;
    RETURN Text.Sub(string, 0, first) & "\\\\" &
           SlashSlashes(Text.Sub(string, first + 1));
  END SlashSlashes;


PROCEDURE GenerateCode (dfv: FormsVBT.T; fullname: TEXT; bundled: BOOLEAN) : TEXT=
  VAR
    declarations                   := "";
    constructors                   := "";
    defn, sessn : TEXT;
    egform                         := "taxform";
    formclass                      := NodeVBT.NameToIndex("form");
    n                              := NodeVBT.NoOfObjects(formclass);
    current     : NodeVBT.FormNode;
    initialFormsToCreate := "";

    progname, destdir : TEXT;
    needToSave : BOOLEAN := TRUE;

  BEGIN

    IF fullname # NIL THEN
      progname := Pathname.Last(fullname);
      destdir := Pathname.Prefix(fullname);
    ELSE
      progname := NodeVBT.defaultName;
      needToSave := FALSE;
    END;

    bundled := bundled OR NOT(needToSave);

    fv := NARROW(dfv, Dialog.T);

    NodeVBT.ComputeAnchoredFormTree();
    (* for each root form in turn *)
    FOR i := 0 TO n - 1 DO
      IF i = 0 THEN
        current := NARROW(NodeVBT.GetFirst(formclass), NodeVBT.FormNode);
      ELSE
        current := NARROW(NodeVBT.GetNext(formclass), NodeVBT.FormNode);
      END;

      IF current.ParentForm = NIL THEN
        egform := current.name;

        initialFormsToCreate := initialFormsToCreate & current.name &
                                    "New(LOCAL);\n";

        (* generate the sx and write to destdir/formname.fv *)

        NodeVBT.ComputeDimensions(current);
        current.DialogSX := current.SXTemplate();
        EVAL current.computeSX(TRUE);



        (* Using declarationTemplate generate declaration code and append
           to declarations *)
        declarations :=
          declarations
            & NodeVBT.FAndRAll(declarationTemplate, "form", current.name);
        
     

        (* Using constructorTemplate generate constructor code and append
           to constructors *)
        constructors := constructors & GenerateConstructorCode(current);

(* == NOT NEEDED == sx-generation
        IF bundled THEN 
          constructors := NodeVBT.FindAndReplace(constructors, "FormSX",
                                                 "\"" & SlashQuotes(SlashSlashes(current.DialogSX)) & "\""); 
        ELSE
          constructors := NodeVBT.FindAndReplace(constructors, "FormSX",
                             "volibLocal.LoadFile(\"" & current.name &".fv\")");                     
    
          WriteTextToFile(current.DialogSX, destdir & "//" & current.name & ".fv");
        END;
*)
      END (* IF *)
    END (* FOR *);
    (* end for *)


    (* replace $declarations$ and $constructors$ in defnTemplate *)
    defn :=
      NodeVBT.FindAndReplace(defnTemplate, "declarations", declarations);
    defn := NodeVBT.FindAndReplace(defn, "constructors", constructors);

    (* replace $GlobalCode$ with code that is visible within all form instances *)
    defn := NodeVBT.FindAndReplace(defn, "GlobalCode", globalCode);
    
    (* replace $name$ and $egform$ in sessionTemplate and write to
       destdir/progname.obl *)
    sessn := NodeVBT.FAndRAll(sessionTemplate, "name", progname);
    sessn := NodeVBT.FAndRAll(sessn, "egform", egform);
    sessn := NodeVBT.FindAndReplace(sessn, "initForms", 
                                    initialFormsToCreate);

    (* put in session constructor code and server side code *)
   
    sessn := NodeVBT.FindAndReplace(sessn, "SessionConstCode", sessionConstructor);
    sessn := NodeVBT.FindAndReplace(sessn, "ServerSideCode", serverSideCode);

    IF bundled THEN
      sessn := NodeVBT.FAndRAll(sessn, "Definition", defn);
    ELSE
      sessn := NodeVBT.FAndRAll(sessn, "Definition", 
                              "load \"" & progname & "DEFN.obl\";"  ); 
      (* write defn to destdir/prognameDEFN.obl *) 
      WriteTextToFile(defn, destdir & "//" & progname & "DEFN.obl");
    END (* IF *);

    (* write sessn to destdir/progname.obl *)  
   IF needToSave THEN
      WITH ssnfnm =  destdir & "//" & progname & ".obl" DO
        WriteTextToFile(sessn, ssnfnm); 
      END;
   END;

  IF needToSave THEN
    RETURN "(* saved *)";
  ELSE
    RETURN sessn;
  END;
  END GenerateCode;

BEGIN
END GenerateObliq.
