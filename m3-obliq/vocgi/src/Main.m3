(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Mon Oct 31 14:46:00 PST 1994 by kalsow  *)
(*      modified on Fri Jul 22 11:03:28 PDT 1994 by bharat  *)

MODULE Main;

(* vocgi - gateway for visual obliq.

   This is responsible for a variety of operations that result in the
   creation of an html page on the fly to support distributed apps on the
   world-wide web.

   KEYWORDS (voapps, vosessions, voget, vojoin, vomembers)

   USAGE:

   .../vocgi/<DIR>?voapps+<HOSTNAME>

         = generates a list of available apps in a certain directory
           with html links. Clicking on the name of an app 
           will cause it to be registered with a network object
           called voWebList@<HOSTNAME> and will import
           the text of the program.
                                             
           Apps are identified by the vobl extension.

           The apps are listed as follows:-

               <app-name>(description)
               --- Link1--   --Link2--

               Link1 = .../vocgi/<FULLPATH>/app.vobl?voget+<HOSTNAME>
               Link2 = <FULLPATH>/app.html
               This is created only if the html file is present

   .../vocgi?vosessions+<HOSTNAME>    

         =  generates a list of ongoing sessions
            registered with vowwwlist@<HOSTNAME> with
            html links. Names of sessions are listed as

               <app-name>@<server-sitename> (description, members)
               --------- Link1------------   --Link2--    -Link3-

               Link1 = .../vocgi?vojoin+<app-name>@<server-sitename>
               Link2 = <FULLPATH>/app.html ( if present )
               Link3 = .../vocgi?vomembers+<app-name>@<server-sitename>

   .../vocgi/<FULLPATH>/app.vobl?voget+<HOSTNAME>
   
         = sends the vobl file over as plaintext and registers the
           server site name with voWebList@<HOSTNAME>

   .../vocgi?vojoin+<app-name>@<server-sitename>
   
         = returns "vojoin <app-name>@<server-sitename>;"

   .../vocgi?vomembers+<app-name>@<server-sitename>

         = queries <app-name>@<server-sitename> for the list
           of participants and then sees if they are alive
           and generates a list of active members.
*)

IMPORT Obliq, ObValue;
IMPORT ObliqOnline;
IMPORT ObLibM3; (* rd,wr,lex,fmt,pickle,process,thread *)

IMPORT Env, File, FileRd, FS, OSError, Params,  Pathname, Rd;
IMPORT SynWr, Text, TextList, TextRd, TextWr,  Thread, Wr;

FROM Stdio IMPORT  stdout;

EXCEPTION InputError(TEXT);

CONST
  Greetings = "";    
  IntroToApps = "<i>This page lists a set of Visual Obliq Applications "
    & "that may be invoked by clicking on corresponding links. Imported "
    & "applications are run within a 'safe' Visual Obliq Interpreter on "
    & "your site. Whenever the imported program tries to access a file "
    & "or spawn a process you will be notified and allowed to abort it, "
    & "unless the action is allowed in your </i>.vorestrict<i> file.</i>"
    & "<P><P>\n";

VAR 
  interp: ObliqOnline.T;
(*  rsrcPath : Rsrc.Path;   *)
  output := TextWr.New();

PROCEDURE Enumerate (fn: TEXT): TextList.T RAISES {OSError.E} =
  VAR
    res, tail, l: TextList.T := NIL;
    t: TEXT;
    i: FS.Iterator;
  BEGIN
    IF fn = NIL THEN fn := ""; END;
    TRY
      i := FS.Iterate(fn);
      WHILE i.next(t) DO
        l := TextList.Cons(t, NIL);
          IF res = NIL THEN
            res := l;
          ELSE
            tail.tail := l;
          END;
          tail := l;
        END;
      FINALLY
        i.close();
      END;
    RETURN res;
  END Enumerate;

PROCEDURE GetEnv(s : TEXT) : TEXT =
  BEGIN
    WITH z = Env.Get(s) DO
      IF s = NIL THEN
        RETURN("");
      ELSE
        RETURN(z);
      END (* IF *)
    END (* WITH *)
  END GetEnv;

VAR
  vopath :=  GetEnv("PATH_INFO"); 
  voclient := GetEnv("REMOTE_HOST"); 

PROCEDURE Do (cmd: TEXT; name : TEXT := "current file")=
  BEGIN
    ObliqOnline.Interact(interp,
      rd := TextRd.New(cmd),
      rdName := name,
      closeRd := TRUE, 
      generateEOF := TRUE);
  END Do;

(*
PROCEDURE LoadObliqRsrc(name: TEXT) =
  BEGIN
    TRY
      WITH  loadedFile = Rsrc.Get(name, rsrcPath) DO
        Do(loadedFile, name);
      END;
    EXCEPT
      Rsrc.NotFound =>Error("Cannot find '", name, "' in resource bundle");
    ELSE
      Error("While executing '", name, "'");
    END (* TRY *);
  END LoadObliqRsrc;
*)

PROCEDURE StartInterpreter() =
  BEGIN

    (* rsrcPath := Rsrc.BuildPath ("$VOCGIPATH", VOCgiBundle.Get()); *)

    ObliqOnline.Setup();
    ObLibM3.PackageSetup();

    interp := ObliqOnline.New(Greetings, SynWr.New(output),  FALSE);
   
    (* Dont load default .obliq - who knows what it contains *)
    (* Instead use bundled files                                                   *)

    Do(";");

  END StartInterpreter; 

PROCEDURE MakeHtmlLink(message, linkto : TEXT) : TEXT =
  BEGIN
    RETURN  "<A HREF=\"" & linkto & "\">" & message &   "</A>";
  END MakeHtmlLink;

PROCEDURE VOApps() RAISES { InputError } =
  BEGIN
    (* open vopath as a directory and list all the vobl files therein *)
    Out ("Content-type: text/html\n\n\n");
    Out ("<TITLE>List of Available Applications</TITLE>\n<UL>\n");
    Out ("<H2>Visual Obliq Applications</H2>\n");
    Out ("(Generated automatically by the gateway, ");
    Out (Params.Get(0), ")<P><P>\n");
    Out (IntroToApps);
    TRY
      WITH enumlist = Enumerate(vopath) DO
        FOR i := 0 TO TextList.Length(enumlist) DO
          WITH 
            current = TextList.Nth(enumlist, i),
            ext = Pathname.LastExt(current),
            registerAt = Params.Get(2)
          DO
            IF Text.Equal(ext, "vobl") THEN
              (* Out ("<LI> ", MakeHtmlLink(Pathname.Base(current)); *)
              (* Out ("file://localhost", vopath, "/" & current)); *)
              Out ("<LI> ");
              Out (MakeHtmlLink(Pathname.Base(current),
                     "http://" & GetEnv("SERVER_NAME") & GetEnv("SCRIPT_NAME")
                     & vopath & "/" & current & "?voget+" & registerAt ));

              (* check if the corresponding file with a .html extension exists
                 this will have the documentation *)
              WITH doc = vopath & "/" & Pathname.Base(current) & ".html" DO
                IF FileIsReadable(doc) THEN
                  Out (" (",
                       MakeHtmlLink("documentation",
                                    "file://localhost" & doc),
                       " ) ");
                END
              END (* WITH *);

              Out ("<P>\n");
             
            END (* IF *)
          END (* WITH *)
        END (* FOR *)
      END
    EXCEPT
    ELSE
      RAISE InputError("Could not open directory: " & vopath);
    END;

  END VOApps;

PROCEDURE FileIsReadable (path: TEXT): BOOLEAN =
  VAR f: File.T;
  BEGIN
    TRY
      f := FS.OpenFileReadonly (path);
      f.close ();
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END FileIsReadable;

PROCEDURE VOGet() =
  <*FATAL OSError.E, Rd.Failure, Wr.Failure, Thread.Alerted*>
  VAR 
    f := FileRd.Open(vopath); 
    (* vopath has the file in question *)   
    oblcode := Rd.GetText(f, LAST(CARDINAL)); 
   
    name := Pathname.LastBase(vopath);
    cmd := "try\n" &
      "     let svr = net_import(\"vossns\", \"" & Params.Get(2) &  "\");\n" &
      "     svr.addNew(\"" & name & "\",\"" & voclient & "\");\n" &
      "except else end;\n"; 
  BEGIN
    StartInterpreter(); 

    (* register the <app>@<voclient>  with the vossns@<Params.Get(2)> *)
    Do( cmd );

    Out ("Content-type: application/x-vobl\n\n\n");
    
    Out (oblcode);
    Wr.Flush(stdout);
  END VOGet;

PROCEDURE VOJoin() =
  BEGIN
    WITH ssn = Params.Get(2),
         host = Params.Get(3) DO
      Out ("Content-type: application/x-vobl\n\n\n");
      Out ("Join(\"" & ssn & "\",\"" & host & "\");\n");
    END (* WITH *)
  END VOJoin;

PROCEDURE VOSessions() =
  VAR
    quotedPrefix := "\"http://" &
                        GetEnv("SERVER_NAME")&GetEnv("SCRIPT_NAME") &
                        vopath & "?vojoin\"";
    script :=    "let result = \n"  &
        "try\n" &
        "     let svr = net_import(\"vossns\", \"" & Params.Get(2) &  "\");\n" &
        "     svr.genListing(" &  quotedPrefix  & ");\n" &
        "except else \"none\" end;\n";
  BEGIN
    StartInterpreter(); 
    Do(script);
    TRY
      WITH
        obval = Obliq.Lookup("result", interp.env),
        result = Obliq.ToText(obval)  DO
        IF Text.Equal(result, "none") THEN
          Error("Couldn't get session listing from vossns@" & Params.Get(2) );
        ELSE
          (* result should contain the complete html document *)
          Out (result);
          Wr.Flush(stdout);
        END (* IF *)
      END (* WITH *);
    EXCEPT
      ObValue.Error(foo) =>Error("POST-MORTEM = Raised Exception: " & foo.msg);
    ELSE
    END (* TRY *)
  END VOSessions;

(*------------------------------------------------ errors & low-level I/O ---*)

PROCEDURE Error (msg: TEXT) =
  BEGIN    
    Out ("Content-type: text/html\n\n\n");
    Out ("<H1>Error in " & Params.Get(0) & "</H1><P>\n");
    Out ("<H2>" & msg & "</H2> <P>\n"); 
  END Error;

PROCEDURE UsageError (msg: TEXT) =
  BEGIN    
    Error (msg);
    Out ("<H1>Usage:</H1> <P>\n<B>", Params.Get(0), "</B>\n");
    Out ("[voapps &lt; HOSTNAME&gt; <P>");
    Out ("| vosessions &lt;HOSTNAME&gt;<P><UL><UL>\n");
    Out ("      | voget &lt;HOSTNAME&gt;<P>");
    Out (" | vojoin &lt;APP-NAME&gt;@&lt;SERVER-SITENAME&gt;<P>\n");
    Out ("      | vomembers &lt;APP-NAME&gt;@&lt;SERVER-SITENAME&gt;] <P>\n");
  END UsageError;

PROCEDURE Out (a, b, c: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (stdout, a); END;
    IF (b # NIL) THEN Wr.PutText (stdout, b); END;
    IF (c # NIL) THEN Wr.PutText (stdout, c); END;
  END Out;

(*---------------------------------------------------------- main program ---*)

BEGIN
  TRY

    IF Params.Count < 2 THEN
      UsageError("No arguments!");
    ELSIF Params.Count <= 4 THEN
      WITH action = Params.Get(1) DO
        IF Text.Equal(action, "voapps")  THEN
          (* open directory and list files with a vobl extension *)
          VOApps();
        ELSIF Text.Equal(action, "vosessions") THEN
          VOSessions();
        ELSIF Text.Equal(action, "voget")  THEN
           VOGet();
        ELSIF Text.Equal(action, "vojoin")  THEN
          VOJoin();
        ELSIF Text.Equal(action, "vomembers ") THEN
          UsageError("Unimplemented");
        ELSE
          UsageError("Unknown command");
        END
      END (* WITH *)
    ELSE
      UsageError("Argument list has too few or too many arguments");
    END (* IF *)

  EXCEPT
  | InputError(t) => Error(t);
  ELSE               Error("Error!");
  END;

END Main.






