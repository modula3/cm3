(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Mar 12 12:17:06 1997
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat May 17 14:45:02 1997
 * Update Count    : 4
 * 
 * $Source: /opt/cvs/cm3/m3-obliq/obliqlibemb/src/ObLoader.m3,v $
 * $Date: 2001-01-24 21:52:38 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1997/05/25 20:40:53  bm
 * small fix.
 *
 * Revision 1.2  1997/03/12 21:34:59  bm
 * Moved sharedobj from coterie
 *
 * 
 * HISTORY
 *)

MODULE ObLoader;

IMPORT Bundle, ObLibOnline, ObValue, Obliq, ObliqParser, SynScan, SynWr, 
       SynParse, TextRd, ObCommand, Text, TextWr, RdCopy, Pathname,
       OSError, FileRd, Rd, Fmt, Wr, Thread, Process, TextSeq;

REVEAL 
  T = Public BRANDED OBJECT
    parser : SynParse.T;
    env    : Obliq.Env := NIL;
    alt    : T := NIL;
  METHODS
    getRd(name: TEXT): Rd.T;
    getText(name: TEXT): TEXT;
  OVERRIDES
    get := Get;
    load := Load;
    help := Help;
  END;

REVEAL 
  BundleT = BundlePublic BRANDED OBJECT
    bundle : Bundle.T;
  OVERRIDES
    init := BundleInit;
    getRd := BundleGetRd;
    getText := BundleGetText;
  END;

REVEAL 
  DirT = DirPublic BRANDED OBJECT
    root : Pathname.Arcs;
  OVERRIDES
    init := DirInit;
    getRd := DirGetRd;
    getText := DirGetText;
  END;

PROCEDURE Get (self : T; qualName : TEXT) : Obliq.Val =
  BEGIN
    ObliqParser.ReadFrom (self.parser, "", TextRd.New (qualName & ";"), TRUE);

    TRY
      RETURN Obliq.EvalPhrase (ObliqParser.ParsePhrase (self.parser), 
                               self.env);
    EXCEPT
    | ObValue.Error (packet) => 
      ObValue.ErrorMsg(SynWr.err, packet);
      Process.Crash("Fatal error attempting to get '" & qualName & 
        "' in ObLoader.Get");
      <*ASSERT FALSE*>
    | ObValue.Exception (packet) => 
      ObValue.ExceptionMsg(SynWr.err, packet);
      Process.Crash("Fatal error attempting to get '" & qualName & 
        "' in ObLoader.Get");
      <*ASSERT FALSE*>
    | ObliqParser.Eof =>
      Process.Crash("Parser.EOF error attempting to get '" & qualName & 
        "' in ObLoader.Get");
      <*ASSERT FALSE*>
    END;
  END Get;

PROCEDURE Load (self : T; name : TEXT) =
  BEGIN
    TRY
      WITH rd = self.getRd(name) DO
        ObliqParser.ReadFrom (self.parser, "", rd, TRUE, TRUE);
      END;
      LOOP
        TRY
          SynScan.FirstPrompt (self.parser.Scanner());
          WITH phrase = ObliqParser.ParsePhrase (self.parser) DO
            EVAL ObliqParser.EvalPhrase (self.parser, phrase, self.env);
          END;
        EXCEPT
        | ObliqParser.Eof => RETURN;
        END;
      END;
    EXCEPT
    | ObValue.Error (packet) => 
      ObValue.ErrorMsg(SynWr.err, packet);
      Process.Crash("Fatal error attempting to get '" & name & 
        "' in ObLoader.Load");
    | ObValue.Exception (packet) => 
      ObValue.ExceptionMsg(SynWr.err, packet);
      Process.Crash("Fatal error attempting to get '" & name & 
        "' in ObLoader.Load");
    END;
  END Load;

PROCEDURE Help (self: T; cmd: ObCommand.T; arg, pkgname, m3name : TEXT) =
  BEGIN
    IF m3name = NIL THEN
      m3name := pkgname;
    END;
    IF Text.Equal (arg, "!") THEN
      SynWr.Text (SynWr.out, 
                  "  " & Fmt.Pad (pkgname, 18, ' ', Fmt.Align.Left) & 
                  "(interface to the " & m3name & " module)\n");
    ELSIF Text.Equal (arg, "?") THEN
      WITH text = self.getText(m3name & ".hlp") DO
        IF text # NIL THEN
          SynWr.Text (SynWr.out, text);
        END;
        SynWr.NewLine (SynWr.out);
      END;
    ELSE
      SynWr.Text(SynWr.out, "Command " & cmd.name & ": bad argument: " & arg);
      SynWr.NewLine (SynWr.out);
    END;
  END Help;

PROCEDURE BundleInit (self : BundleT; bundle: Bundle.T; parent: T; alt: T) : T=
  BEGIN
    self.alt := alt;
    IF parent # NIL THEN
      self.env := parent.env; 
    ELSE
      self.env := Obliq.EmptyEnv ();
    END;
    self.parser := ObliqParser.New (SynWr.out);
    self.bundle := bundle;

    ObLibOnline.RegisterScanner (self.parser.Scanner ());

    RETURN self;
  END BundleInit;

PROCEDURE BundleGetRd (self: BundleT; name : TEXT) : Rd.T =
  BEGIN
    RETURN TextRd.New (self.getText(name));
  END BundleGetRd;

PROCEDURE BundleGetText (self: BundleT; name : TEXT) : TEXT =
  VAR text: TEXT;
  BEGIN
    text := Bundle.Get (self.bundle, name);
    IF text = NIL THEN
      IF self.alt # NIL THEN
        RETURN self.alt.getText (name);
      END;
      Process.Crash("Could not find load file '" & name & "'.");
    END;
    RETURN text;
  END BundleGetText; 

PROCEDURE DirInit (self : DirT; root: TEXT; parent: T; alt: T) : T =
  BEGIN
    self.alt := alt;
    IF parent # NIL THEN
      self.env := parent.env; 
    ELSE
      self.env := Obliq.EmptyEnv ();
    END;
    self.parser := ObliqParser.New (SynWr.out);
    TRY
      self.root   := Pathname.Decompose(root);
    EXCEPT 
    | Pathname.Invalid => 
      Process.Crash("Invalid Pathname '" & root & "'");
    END;
    ObLibOnline.RegisterScanner (self.parser.Scanner ());
    RETURN self;
  END DirInit; 

PROCEDURE DirGetRd (self: DirT; name : TEXT) : Rd.T =
  BEGIN
    self.root.addhi(name);
    TRY
      WITH file = Pathname.Compose(self.root) DO
        EVAL self.root.remhi();
        TRY
          RETURN FileRd.Open(file);
        EXCEPT
        | OSError.E =>
          IF self.alt # NIL THEN
            RETURN self.alt.getRd(name);
          END;
          Process.Crash("Error opening file '" & file & "'");
          RETURN NIL; (* To shut up the compiler *)
        END;
      END;
    EXCEPT
    | Pathname.Invalid => 
      Process.Crash("Invalid filename '" & name & "'");
      RETURN NIL; (* To shut up the compiler *)
    END;
  END DirGetRd;

PROCEDURE DirGetText (self: DirT; name : TEXT) : TEXT =
  BEGIN
    TRY
      WITH rd = self.getRd(name),
           wr = TextWr.New() DO
        EVAL RdCopy.ToWriter(rd, wr);
        RETURN TextWr.ToText(wr);
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted, Wr.Failure =>
      Process.Crash("Error reading from file '" & name & "'");
      RETURN NIL; (* To shut up the compiler *)
    END;
  END DirGetText; 

PROCEDURE NewDirs(roots: TextSeq.T; parent: T := NIL; alt: T := NIL): T =
  VAR root: T := NIL;
  BEGIN
    FOR i := roots.size()-1 TO 0 BY -1 DO
      root := NEW(DirT).init(roots.get(i), parent, alt);
      alt := root;
    END;
    RETURN root;
  END NewDirs;

BEGIN
END ObLoader. 
