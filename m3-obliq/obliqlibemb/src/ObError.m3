(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 18:32:00 PDT 1994 by najork                   *)
(*       Created on Tue Jul 26 18:38:38 PDT 1994 by najork                   *)
(* Copied from anim3d.ObAux.m3                                               *)

MODULE ObError;

IMPORT ObValue, Obliq, SynWr, TextWr, Wr, Atom, AtomList;

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

PROCEDURE AtomListToText (al: AtomList.T) : TEXT =
  VAR ret: TEXT;
  BEGIN
    IF al # NIL THEN
      ret := Atom.ToText(al.head);
      al := al.tail;
    ELSE
      ret := "";
    END;
    WHILE al # NIL DO
      ret := ret & ", " & Atom.ToText(al.head);
      al := al.tail;
    END;
    RETURN ret;
  END AtomListToText; 

BEGIN
  mu := NEW (MUTEX);
  wr := TextWr.New ();
  swr := SynWr.New (wr);
END ObError.
