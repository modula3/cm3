(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CHash;

(* Hash table for reserved words, literals, identifiers. *)

TYPE
  Table <: TablePublic;
  TablePublic = OBJECT
  METHODS
    init(size: CARDINAL; idCreator: IdCreator := NIL): Table;
    enter(text: TEXT): Id;
    lookup(text: TEXT; VAR id: Id): BOOLEAN;
    setCreator(idCreator: IdCreator): IdCreator;
    enterCharsWithValue(v: Value; READONLY chars: ARRAY OF CHAR): Id;
    enterTextWithValue(v: Value; text: TEXT): Id;
  END;

  Id <: IdPublic;
  IdPublic = OBJECT
  METHODS
    toText(): TEXT;
  END;

  IdCreator = OBJECT METHODS new(text: TEXT): Id END;

(* When creating a table the client specifies:
\begin{itemize}
\item The table size. It is a bucket and chain style hash table; "size" 
      specifies the number of buckets).
\item An optional "IdCreator" object. If this is supplied its "new"
      method will be called whenever a text "n" is added to the table by
      the "enter(n)" method. The "id" returned by the "new" method (which 
      can be any subtype of "Id") will be the value that is associated with 
      "n". If no creation object is specified, a default subtype of "Id"
      will be associated. A different "id" value must be associated
      with each text that is entered in the table.
\end{itemize}


A call of "lookup(n, id)" returns "TRUE" if "n" has been entered in the
table, and sets "id" to the "Id" value that was associated with "n".
Otherwise it returns "FALSE" and leaves "id" unchanged.

The "setCreator" method changes the "IdCreator" object that is associated
with the table, returning the previous value.

The "toText" method returns the text with which "id" has been associated.

\subsection{Incremental Hashing}

The following routines expose more of the hashing mechanism. They allow a
hash value to be built up incrementally and then specified when a text or
array of characters is entered into the table. They are intended for use by
a lexer which has to build up arrays of characters (e.g. identifiers,
reserved words) incrementally and hence can build up the hash value at the
same time *)

TYPE
  Value <: ValuePublic;
  ValuePublic = OBJECT
  METHODS
    init(): Value;
    reset();
    addCharToValue(ch: CHAR);
  END;

(* A hash value is created by "NEW(Value).init()". The "reset" method
resets the computed value to the initial value. Therefore, a lexer
need only allocate one "Value" object and reuse it many times.
The "addCharToValue" method adds the character "ch" to the hash value. 
Values must be added in sequence. For example the
the hash value for "ab" is constructed by:

|  v := NEW(M3CHash.Value).init();
|  v.addCharToValue('a');
|  v.addCharToValue('b');

The "enterCharsWithValue" and "enterTextWithValue" methods add
"chars" and "text" to the table, using the precomputed hash value "v". *)


END M3CHash.
