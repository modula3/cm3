(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 18:32:00 PDT 1994 by najork                   *)
(*       Created on Tue Jul 26 18:38:38 PDT 1994 by najork                   *)


MODULE ObAux;

IMPORT Bundle, Fmt, ObCommand, ObLib3DBundle, ObValue, Obliq, SynWr, 
       Text, TextWr, Wr;

VAR
  mu  : MUTEX;
  wr  : Wr.T;
  swr : SynWr.T;
  

PROCEDURE ErrorToText (packet : ObValue.ErrorPacket) : TEXT =
  BEGIN
    LOCK mu DO
      Obliq.ReportError (swr, packet);
      RETURN TextWr.ToText (wr);
    END;
  END ErrorToText;


PROCEDURE ExceptionToText (packet: ObValue.ExceptionPacket) : TEXT =
  BEGIN
    LOCK mu DO
      Obliq.ReportException (swr, packet);
      RETURN TextWr.ToText (wr);
    END;
  END ExceptionToText;


PROCEDURE Help (self : ObCommand.T; arg, pkgname, m3name : TEXT) =
  BEGIN
    IF m3name = NIL THEN
      m3name := pkgname;
    END;
    IF Text.Equal (arg, "!") THEN
      SynWr.Text (SynWr.out, 
                  "  " & Fmt.Pad (pkgname, 18, ' ', Fmt.Align.Left) & 
                  "(built-in interface to Anim3D's " & m3name & " module)\n");
    ELSIF Text.Equal (arg, "?") THEN
      SynWr.Text (SynWr.out, Bundle.Get(ObLib3DBundle.Get(), m3name & ".hlp"));
      SynWr.NewLine (SynWr.out);
    ELSE
      SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
      SynWr.NewLine (SynWr.out);
    END;
  END Help;


BEGIN
  mu := NEW (MUTEX);
  wr := TextWr.New ();
  swr := SynWr.New (wr);
END ObAux.
