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

INTERFACE M3AST_LX;

IMPORT M3AST;

(* This interface defines the types of the lexical entities in the
Modula-3 AST. At this level of the specification, most of the entities
are declared as opaque types, to be given a definition by a particular
implementation of the AST. The exception is source position
information which is defined as an "INTEGER", although the encoding of
the bits is not specified here. *)

TYPE 
  Symbol_rep <: REFANY;       (* identifiers *)
  Literal_rep <: REFANY;      (* all literals *)
  Number_rep <: Literal_rep;  (* numeric literals *)
  Text_rep <: Literal_rep;    (* text and character literals *)
  SrcPos = INTEGER;           (* source positions *)
  Whitespace_rep <: REFANY;   (* whitespace *)
  BadChar_rep <: REFANY;      (* illlegal character *)
  Comment_rep <: REFANY;      (* a comment *)
  Pragma_rep <: REFANY;       (* a pragma *)
  Token_rep <: REFANY;        (* reserved word or symbol *)

(* The AST is capable of recording the complete set of characters that
were present in the original source file, allowing exact reproduction.
The class "SRC_NODE" is defined to denote a node that represents an
entity in the source form of the program.  The {\it source position}
of any "SRC_NODE" is available as an attribute, "lx_srcpos". The
terminal symbols of the Modula-3 grammar are represented by subtypes
of "SRC_NODE", as are whitespace, comments, pragmas, and illegal
characters. 

Nodes that represent non-terminals of the grammer and, therefore have
child nodes, are declared as subtypes of "SRC_NODE_C". This class carries a
sequence attribute, "lx_node_s", that encodes all the terminals and
child nodes, in the order of occurrence. *)

<* PRAGMA FIELDS *>

TYPE
  SRC_NODE <: M3AST.NODE;
  <* FIELDS OF SRC_NODE
       lx_srcpos: SrcPos; *>

  SRC_NODE_C <: SRC_NODE;
  <* FIELDS OF SRC_NODE_T
       lx_node_s: SEQUENCE OF SRC_NODE; *>
       
  Whitespace <: SRC_NODE;
  <* FIELDS OF Whitespace
       lx_whitespace_rep: Whitespace_rep; *>

  Comment <: SRC_NODE;
  <* FIELDS OF Comment
       lx_comment_rep: Comment_rep; *>

  Pragma <: SRC_NODE;
  <* FIELDS OF Pragma
       lx_pragma_rep: Pragma_rep *>

  BadChar <: SRC_NODE;
  <* FIELDS OF BadChar
      lx_badchar_rep: BadChar_rep *>

  Token <: SRC_NODE;
  <* FIELDS OF Token
       lx_token_rep: Token_rep; *>
  

(* Identifiers are represented by many different node types, all of
which are subtypes of the "ID" class. This class carries the lexical
attribute that denotes the name of the identifier. *)

  ID <: SRC_NODE;
  <* FIELDS OF ID
    lx_symrep: M3AST_LX.Symbol_rep; *>

(* All literals are members of the class "LITERAL", which carries the
lexical information. *)

  LITERAL <: SRC_NODE;
  <* FIELDS OF LITERAL
       lx_litrep: Literal_rep; *>

END M3AST_LX.
