(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: Xrm.i3							*)
(* Last modified on Fri May  7 16:16:27 PDT 1993 by mjordan     *) 
(*      modified on Thu Mar 14 02:34:50 1991 by muller		*)
(*      modified on Wed Apr 25 10:31:12 1990 by jerome		*)



UNSAFE INTERFACE Xrm;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	contains:	/usr/include/X11/Xresource.h		*)
(*==============================================================*)


FROM X		IMPORT	Bool, Enumeration;
FROM Ctypes	IMPORT	const_char_star, char_star, char_star_star, int, 
			int_star, unsigned_int;


(* $XConsortium: Xresource.h,v 1.26 89/12/12 12:12:12 jim Exp $ *)

(***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************)

(* 
    Size of a "regular" List of objects
*)

CONST
  MaxSizeList  = 63;

TYPE Int = int;

(****************************************************************
 ****************************************************************
 ***                                                          ***
 ***                                                          ***
 ***          X Resource Manager Intrinsics                   ***
 ***                                                          ***
 ***                                                          ***
 ****************************************************************
 ****************************************************************)

(****************************************************************
 *
 * Quark Management
 *
 ****************************************************************)

TYPE
  Quark       =  Int;
  QuarkList   =  UNTRACED REF ARRAY [0..MaxSizeList] OF Quark;

CONST
  NULLQUARK   = 0;

TYPE
  String      = char_star;

CONST
  NULLSTRING  = 0;


(* find quark for string, create new quark if none already exists *)
<* EXTERNAL  XrmStringToQuark *>
   PROCEDURE StringToQuark (string: const_char_star): Quark;

(* find string for quark *)
<* EXTERNAL XrmQuarkToString *>
   PROCEDURE QuarkToString (quark: Quark): String;

<* EXTERNAL XrmUniqueQuark *>
   PROCEDURE UniqueQuark (): Quark;


(* ?!?!?! Has to be defined in "Xrm.m3" if needed.
#define XrmStringsEqual(a1, a2) (strcmp(a1, a2) == 0)
?!?!?! *)


(****************************************************************
 *
 * Conversion of Strings to Lists
 *
 ****************************************************************)


TYPE 
  Binding     = Enumeration;
  BindingList = UNTRACED REF ARRAY [0..MaxSizeList] OF Binding;

CONST
  BindTightly	= 0;
  BindLoosely	= 1;

<* EXTERNAL XrmStringToQuarkList *>
   PROCEDURE StringToQuarkList (
		string:               const_char_star;
		quarks_return:        QuarkList);

<* EXTERNAL XrmStringToBindingQuarkList *>
   PROCEDURE StringToBindingQuarkList (
		string:               const_char_star;
		bindings_return:      BindingList;
		quarks_return:        QuarkList);

(****************************************************************
 *
 * Name and Class lists.
 *
 ****************************************************************)

TYPE
  Name      =  Quark;
  NameList  =  QuarkList;

(* ?!?! still not allowed in Modula-3 R^1 ... waiting for R^2.
CONST
  NameToString      =  QuarkToString;
  StringToName      =  StringToQuark;
  StringToNameList  =  StringToQuarkList;
?!?!? *)

<* EXTERNAL XrmQuarkToString *>
   PROCEDURE NameToString (quark: Quark): String;

<* EXTERNAL  XrmStringToQuark *>
   PROCEDURE StringToName (string: const_char_star): Quark;

<* EXTERNAL XrmStringToQuarkList *>
   PROCEDURE StringToNameList (string: const_char_star;
                               quarks_return: QuarkList);

TYPE
  Class              =  Quark;
  ClassList          =  QuarkList;

(* ?!?! still not allowed in Modula-3 R^1 ... waiting for R^2.
CONST
  ClassToString      =  QuarkToString;
  StringToClass      =  StringToQuark;
  StringToClassList  =  StringToQuarkList;
?!?!?! *)

<* EXTERNAL XrmQuarkToString *>
   PROCEDURE ClassToString (quark: Quark): String;

<* EXTERNAL  XrmStringToQuark *>
   PROCEDURE StringToClass (string: const_char_star): Quark;

<* EXTERNAL XrmStringToQuarkList *>
   PROCEDURE StringToClassList (string: const_char_star;
                                quarks_return: QuarkList);

(****************************************************************
 *
 * Resource Representation Types and Values
 *
 ****************************************************************)

TYPE
  Representation      =  Quark;
  RepresentationStar  =  UNTRACED REF Representation;

<* EXTERNAL  XrmStringToQuark *>
   PROCEDURE StringToRepresentation (string: const_char_star): Quark;

<* EXTERNAL XrmQuarkToString *>
   PROCEDURE RepresentationToString (quark: Quark): String;

TYPE
  Value     = RECORD size: unsigned_int; addr: ADDRESS END;
  ValueStar = UNTRACED REF Value;
  ValuePtr  = UNTRACED REF Value;

(****************************************************************
 *
 * Resource Manager Functions
 *
 ****************************************************************)


TYPE
  HashBucket      =  ADDRESS;
  SearchList      =  UNTRACED REF ARRAY [0..MaxSizeList] OF HashBucket;
  Database        =  ADDRESS;
  DatabaseStar    =  ADDRESS;

<* EXTERNAL XrmDestroyDatabase *>
   PROCEDURE DestroyDatabase (database: Database);

<* EXTERNAL XrmQPutResource *>
   PROCEDURE QPutResource (
		database:             DatabaseStar;
		bindings:             BindingList;
		quarks:               QuarkList;
		type:                 Representation;
		value:                ValueStar);

<* EXTERNAL XrmPutResource *>
   PROCEDURE PutResource (
		database:             DatabaseStar;
		specifier:            const_char_star;
		type:                 const_char_star;
		value:                ValueStar);

<* EXTERNAL XrmQPutStringResource *>
   PROCEDURE QPutStringResource (
		database:             DatabaseStar;
		bindings:             BindingList;
		quarks:               QuarkList;
		value:                const_char_star);

<* EXTERNAL XrmPutStringResource *>
   PROCEDURE PutStringResource (
		database:             DatabaseStar;
		specifier:            const_char_star;
		value:                const_char_star);

<* EXTERNAL XrmPutLineResource *>
   PROCEDURE PutLineResource (
		database:             DatabaseStar;
		line:                 const_char_star);

<* EXTERNAL XrmQGetResource *>
   PROCEDURE QGetResource (
		database:             DatabaseStar;
		quark_name:           NameList;
		quark_class:          ClassList;
		quark_type_return:    RepresentationStar;
		value_return:         ValueStar);

<* EXTERNAL XrmGetResource *>
   PROCEDURE GetResource (
		database:             Database;
		str_name:             const_char_star;
		str_class:            const_char_star;
		str_type_return:      char_star_star;
		value_return:         ValueStar): Bool;

<* EXTERNAL XrmQGetSearchList *>
   PROCEDURE QGetSearchList (
		database:             Database;
		names:                NameList;
		classes:              ClassList;
		list_return:          SearchList;
		list_length:          Int):	Bool;

<* EXTERNAL XrmQGetSearchResource *>
   PROCEDURE QGetSearchResource (
		list:                 SearchList;
		name:                 Name;
		class:                Class;
		type_return:          RepresentationStar;
		value_return:         ValueStar): Bool;

(****************************************************************
 *
 * Resource Database Management
 *
 ****************************************************************)

<* EXTERNAL XrmGetFileDatabase *>
   PROCEDURE GetFileDatabase (filename:  const_char_star): Database;

<* EXTERNAL XrmGetStringDatabase *>
   PROCEDURE GetStringDatabase (data: const_char_star): Database;

<* EXTERNAL XrmPutFileDatabase *>
   PROCEDURE PutFileDatabase (database:  Database;
			      filename:  const_char_star);

<* EXTERNAL XrmMergeDatabases *>
   PROCEDURE MergeDatabases (source_db:     Database;
			     target_db:     DatabaseStar);


(****************************************************************
 *
 * Command line option mapping to resource entries
 *
 ****************************************************************)

TYPE
  OptionKind = Enumeration;

CONST
  optionNoArg	   = 0;	(* Value is specified in OptionDescRec.value	    *)
  optionIsArg	   = 1;	(* Value is the option string itself		    *)
  optionStickyArg  = 2;	(* Value is characters immediately following option *)
  optionSepArg	   = 3;	(* Value is next argument in argv		    *)
  optionResArg	   = 4;	(* Resource and value in next argument in argv      *)
  optionSkipArg	   = 5;	(* Ignore this option and the next argument in argv *)
  optionSkipLine   = 6;	(* Ignore this option and the rest of argv	    *)
  optionSkipNArgs  = 7;	(* Ignore this option and the next 
			   OptionDescRes.value arguments in argv *)

TYPE
  OptionDescRec = RECORD
		    optionn: char_star;	 (* Option abbreviation in argv *)
		    specifier: char_star; (* Resource specifier  *)
		    argKind: OptionKind; (* Which style of option it is *)
		    value: ADDRESS;	 (* Value to provide if XrmoptionNoArg *)
		  END;
  OptionDescList    = UNTRACED REF ARRAY [0..MaxSizeList] OF OptionDescRec;

CONST
  voidOptionDescList  =  NIL;


<* EXTERNAL XrmParseCommand *>
   PROCEDURE ParseCommand (
		database:             DatabaseStar;
		table:                OptionDescList;
		table_count:          Int;
		name:                 const_char_star;
		argc_in_out:          int_star;
		argv_in_out:          char_star_star);


END Xrm.


