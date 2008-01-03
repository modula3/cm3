(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST;

IMPORT AST;

(* This interface and its companions, define an abstract syntax for
the Modula-3 programming language, expressed as a tree of nodes
represented by "OBJECT" types. The specification follows the approach
described in the "AST" interface, and is structured in several layers.
The principal layers are "M3AST_LX", "M3AST_AS" and "M3AST_SM", which
define the lexical, syntactic and semantic layers, respectively.
Within these interfaces the AST nodes are specified as opaque "OBJECT"
types, with attributes defined abstractly using the "FIELDS" pragma.
Additional interfaces define the access method to the attributes. For
example, "M3AST_AS_F" defines the attributes as fields of the object
type.  In the abstract interfaces, the name and type of abstract
attributes are indicated with a "FIELDS" declaration, for example: 

|  <* FIELDS OF Node 
|       attribute_name: Attribute_type; *>

\subsection{Attribute Types}

Attribute types fall into three groups. First, lexical types denoting,
for example, the characters of an identifier or the characters of a
text literal. These correspond to the token grammar of the concrete
syntax.  The lexical attribute types are declared in the interface
"M3AST_LX", are opaque, and are given concrete definitions by a
particular compiler implementation. Thus, the details of the token
grammar are not reflected in the abstract syntax.  The lexical
attribute names are distinguished by a prefix of "lx_".

The second group, are always other node types, that is, subtypes of
"AST.NODE", and are often called the children of the node.  In
general, each non-terminal on the right hand side of a rule in the
grammar is represented by a child node of the corresponding type. An
optional term "X" (denoted by "[X]" in the grammar), is represented by
a child of type of "X_NULL", denoting that "NIL" represents a legal
missing item. A corollary of this is that a value of "NIL" is not
legal for any attribute in a well-formed syntax tree, unless it has
type "X_NULL". Sequences of "X"'s (denoted by "{X}" in the syntax) are
represented as instances of an abstract sequence type "SEQUENCE OF T".
Finally, non-terminals that are defined by more than one rule, for
example, "STM", are defined by object types referred to as classes
(e.g. denoting the {\it class} of statements), with each choice
appearing as a separate subtype. There are no instances of such class
types in a tree instance, only of the subtypes.  To distinguish the
class types, they are named using all upper case leters.

The third group of attributes are those added by layers other than
"M3AST_LX" and "M3AST_AS", and denote the results of processing the
syntactic AST. For example, the "M3AST_SM" layer adds attributes to
denote the results of static semantic analysis. These attributes are
very often node types, but can be any Modula-3 type.

\subsection{Multiple Inheritance}

There are a number of places where it would be convenient to declare a
node as a subtype of more than one parent, that is, use multiple
inheritance. Since this is not possible in Modula-3, a number of
mechanisms are used as workarounds. Sometimes the number of affected
nodes is small enough that it suffices to merely repeat the mutiple
inherited attribute in each node.  The general solution is as follows:
Suppose we wish to declare a new node "A" which inherits from "X" and
"Y". Then choose one, say "X", to be the supertype, and include an
attribute named "vY" of type "Y". The "init" method for such a node
is overridden to build this structure. *)

(*\subsubsection{M3AST.NODE}

This is the interface in which one would define attributes that were common
to all Modula-3 AST nodes. In the current specification, there are no
such attributes, so we equate an "M3AST.NODE" with an "AST.NODE". *)

TYPE 
  NODE = AST.NODE;
  T = AST.T;

END M3AST.

