(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* NetPath.i3                                                *)
(* Last modified on Fri Jan  6 07:57:17 PST 1995 by kalsow   *)
(*      modified on Wed May 26 10:41:48 PDT 1993 by wobber   *)

INTERFACE NetPath;

  (* network representation of pathnames *)

IMPORT TextList, Word;

CONST
  Brand = "NetPath.T";

TYPE
  T = TextList.T;
     (* A "NetPath.T" represents a relative pathname.  All such pathnames
        have component arc paths which are non-empty.  Path arcs have no
        machine-specific separator characters, and require a character
        set and maximum length allowing for architecture independence.
        The value "NIL" is a legal "NetPath.T". *)

  Dir = T;
  PN = RECORD dir: Dir; arc: TEXT; END;
     (* A "PN" names a package.  It consists of a "dir" which
        is interpreted relative to the target package server's
        directory structure, and an "arc" which names an immediate
        child of "dir".  The "arc" field of a "PN" must be non-NIL. *)

  Referent = TEXT;
     (* There are only symbolic links in Unix, therefore we use the
        Unix representation for link referents. *)


EXCEPTION Invalid;

(* for generics *)

PROCEDURE Equal(t1, t2: T) : BOOLEAN;
PROCEDURE Hash(t: T) : Word.T;
PROCEDURE Compare(t1, t2: T) : [-1..1];


(* utilities ... validate type safety *)

PROCEDURE Check(t: T) : BOOLEAN;
PROCEDURE CheckArc(arc: TEXT) : BOOLEAN;


(* conversions *)

PROCEDURE ToText(t: T) : TEXT;
            (* result is a non-NIL relative pathname *)

PROCEDURE FromText(text: TEXT) : T RAISES {Invalid};
            (* expects a relative path name *)
            (* for backward compatibility .. single
               character absolute root names (e.g. "/") will be stripped *)

PROCEDURE ToRelFN(t: T) : TEXT;
            (* result is a non-NIL relative pathname *)

PROCEDURE FromRelFN(text: TEXT) : T RAISES {Invalid};
            (* expects a relative path name *)
            (* for backward compatibility .. single
               character absolute root names (e.g. "/") will be stripped *)

PROCEDURE Parent(t: T) : T;
            (* returns a copy of "t" minus the tail element, or NIL *)


(* package names *)

PROCEDURE EqualPN(pn1, pn2: PN) : BOOLEAN;
PROCEDURE PNToText(pn: PN) : TEXT;
PROCEDURE PNFromText(text: TEXT) : PN RAISES {Invalid};

END NetPath.
