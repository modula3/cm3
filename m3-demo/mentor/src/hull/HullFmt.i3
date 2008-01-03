(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 24 08:59:26 PDT 1994 by najork     *)
(*      modified on Sat Oct 17 13:52:24 PDT 1992 by ramshaw    *)


INTERFACE HullFmt;

IMPORT IntList AS IL;
IMPORT SiteList AS SL;

PROCEDURE IntList (l: IL.T) : TEXT;
PROCEDURE SiteList (l : SL.T) : TEXT;

END HullFmt.

