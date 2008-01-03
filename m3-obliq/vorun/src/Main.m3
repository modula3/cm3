(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Feb  1 09:52:34 PST 1995 by kalsow  *)
(*      modified on Fri Jul 22 11:03:28 PDT 1994 by bharat  *)

MODULE Main;

IMPORT ObliqOnline;
IMPORT ObLibM3; (* rd,wr,lex,fmt,pickle,process,thread *)
IMPORT ObLibUI; (* color,form *)

IMPORT  FileRd, Params, Rd,
        Rsrc, SynWr, Text, TextRd, TextWr,  Thread, TextVBT, Trestle, VORunBundle,  Wr;
FROM Stdio IMPORT stderr, stdin;

(* EXCEPTION ParamError; *)

<* FATAL Wr.Failure, Thread.Alerted *>

CONST Greetings = "Visual Obliq Interpreter - Version 2.1";
              QueryExecutable = "voquery";

TYPE
  AccessLevel = { Unrestricted, Restricted, Protected };
(* correspondingly on the command line we have <no flag>, -r  and -p *)

VAR 
  interp: ObliqOnline.T;
  rsrcPath : Rsrc.Path;
  obliqFile := "";
  accessLevel := AccessLevel.Unrestricted;
  output := TextWr.New();
  queryExecutable := QueryExecutable;
  code : TEXT;

PROCEDURE Usage() =
  BEGIN
    Wr.PutText(stderr, "Usage: " & Params.Get(0) & " [-p | -r]  [-q<voquery-filename>] [<obliq file>]\n\n");
    Wr.PutText(stderr, "   -p = execute in PROTECTED mode ( no filesystem access or process creation )\n");
    Wr.PutText(stderr, "   -r = execute in RESTRICTED mode ( restricted access to filesystem & process creation )\n");
    Wr.PutText(stderr, "   Default is UNPROTECTED (no restrictions)\n\n");
    Wr.PutText(stderr, "   -q<voquery-filename> specifies the full path of the voquery executable\n");
    Wr.PutText(stderr, "   Default is " & QueryExecutable & "\n\n");
    Wr.PutText(stderr, "   If no file is specified, reads from standard input until EOF\n\n");
  END Usage;

PROCEDURE GetParams() =
  BEGIN

    FOR i:=1 TO Params.Count-1 DO
      WITH arg = Params.Get(i) DO
        (* is it a flag *)
        IF Text.Length(arg) >= 2 AND  Text.GetChar(arg, 0) = '-' THEN
          CASE Text.GetChar(arg,1) OF
              | 'p' => accessLevel := AccessLevel.Protected;
              | 'r'  => accessLevel := AccessLevel.Restricted;
              | 'q' => queryExecutable := Text.Sub(arg, 2);
              | 'h' => Usage(); 
              ELSE
              END (* CASE *);
          ELSE
            obliqFile := arg;
          END
      END (* IF *)
    END (* FOR *); 
(*
    IF Params.Count < 2 OR obliqFile = "" THEN
      RAISE   ParamError;
    END (* IF *);
*)
  
  END GetParams;

PROCEDURE Do (cmd : TEXT)=
  BEGIN
    ObliqOnline.Interact(interp,
      rd := TextRd.New(cmd),
      rdName := obliqFile,
      closeRd := TRUE, 
      generateEOF := TRUE);
  END Do;


PROCEDURE loadObliqRsrc(name: TEXT) =
  BEGIN
     TRY
      WITH  loadedFile = Rsrc.Get(name, rsrcPath) DO
        Do(loadedFile);
      END;
    EXCEPT
      Rsrc.NotFound =>Wr.PutText(stderr, "Error: Cannot find '" & name & "' in resource bundle\n");
    ELSE
      Wr.PutText(stderr,  "Error: while executing '" & name & "'\n");
    END (* TRY *);
  END loadObliqRsrc;

BEGIN

  TRY
    GetParams();
    rsrcPath := Rsrc.BuildPath ("$VORUNPATH", VORunBundle.Get());
    
    ObliqOnline.Setup();
    ObLibM3.PackageSetup();
    ObLibUI.PackageSetup();
        
    interp := ObliqOnline.New(Greetings, SynWr.New(output),  FALSE);
   
    (* Don't load default .obliq - who knows what it contains *)
    (* Instead use bundled files                                                   *)
    
    loadObliqRsrc("templates.obl"); 
    loadObliqRsrc("vowidgets.obl"); 
    loadObliqRsrc("vocheckpt.obl"); 
    loadObliqRsrc("volib.obl"); 



    IF accessLevel = AccessLevel.Restricted THEN
      loadObliqRsrc("vorestrict.obl");
    END (* IF *);

    IF accessLevel = AccessLevel.Restricted OR accessLevel = AccessLevel.Protected THEN
      (* no local processes may be created *)
      Do("let processor = ok;");
      (* no local files may be written or read *)
      Do("let fileSys = ok;");
      Do ("let fileSysReader = ok;");
    END;

    IF Text.Equal(obliqFile,"") THEN (* get from stdin *)
      code := Rd.GetText( stdin, LAST(CARDINAL));
    ELSE
      code :=  Rd.GetText( FileRd.Open(obliqFile), LAST(CARDINAL));
    END;

    IF Text.GetChar(code, 0) = '#' THEN
      (* strip first line *)
      WITH  ix = Text.FindChar(code, '\n') DO
        IF ix # -1 THEN
          code := Text.Sub(code, ix+1);
        END;
      END;
    END;

    Do(code);

    WITH v = TextVBT.New("Delete this window to terminate program...") DO
      Trestle.Install(v);      
      Trestle.AwaitDelete(v);
    END;
    
  EXCEPT
  ELSE
    Wr.PutText(stderr, "Error: Abnormal Termination...\nPost-Mortem:-\n\n" & TextWr.ToText(output));
    Usage();
  END;

END Main.






