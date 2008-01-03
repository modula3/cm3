(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: escape character literals *)

MODULE Main;

VAR c: CHAR;

BEGIN

c := '\n';
c := '\t';
c := '\r';
c := '\f';
c := '\\';
c := '\"';
c := '\'';
c := '\000';
c := '\012';

END Main.
