(* $Id$ *)

(* 
   Copyright (c) 2007-2008, 2011 Generation Capital Ltd.  All rights reserved.
   
   Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE HMTime;
IMPORT Date, Word, FinDate;
IMPORT Time, TZ;

EXCEPTION ParseError;

TYPE T = RECORD hour   : [0..23]; 
                minute : [0..59]; 
                second : [0..60]  (* leap second = 60 *)
         END;

CONST First = T {  0,  0,  0 };
CONST Last  = T { 23, 59, 59 }; (* what about the leap second? *)

TYPE F1224 = { F24, F12 };

CONST F1224Names = ARRAY F1224 OF TEXT { "F24", "F12" };

PROCEDURE ParseF1224(t : TEXT) : F1224 RAISES { ParseError };

PROCEDURE Parse(t : TEXT; f1224 := F1224.F24) : T RAISES { ParseError };
  (* parse from HH:MM:SS or HH:MM format; if F1224 = F1224.F12 expect AM or PM to follow *)

PROCEDURE ParseSubsecond(t : TEXT; VAR (*OUT*) sub : LONGREAL) : T 
  RAISES { ParseError };
  (* parse from HH:MM:SS.SSSS... or HH:MM:SS or HH:MM format *)


CONST ParseFIX = Parse;

PROCEDURE Format(t : T) : TEXT;
  (* format in HH:MM:SS (always!) format *)

PROCEDURE FormatHMs(t : T) : TEXT;
  (* format in HH:MM[:SS] format *)

CONST FormatFIX = Format;

PROCEDURE Truncate(READONLY d : Date.T) : T;
  (* extract a T from a Date.T *)

PROCEDURE Assemble(READONLY t : T; 
                   READONLY f : FinDate.T; 
                   prototype : Date.T) : Date.T;
  (* start with prototype and update fields to match t and f given *)

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE SecondInDay(READONLY a : T) : CARDINAL;
  (* which second in the day is it? *)

PROCEDURE FromSeconds(s : CARDINAL) : T RAISES { Overflow };
  (* inverse fxn of above *)

EXCEPTION Overflow(CARDINAL); (* arg is excess. *)

PROCEDURE Advance(READONLY a : T; bySeconds : CARDINAL) : T 
  RAISES { Overflow };

PROCEDURE Today(tz : TZ.T; READONLY hm : T) : Time.T;

CONST Brand = "HMTime";

END HMTime.
