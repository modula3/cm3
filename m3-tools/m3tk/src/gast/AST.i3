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
(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* The traditional method for specifying the syntax of a programming
language is in some variant of Backus-Naur form, or BNF. In this
method, one specifies the syntax by a grammar consisting of {\it
productions}. Each production defines the syntax of one element of the
language, say, a statement. For example, the Modula-3 "WHILE"
statement is defined as follows:

|  WhileSt = WHILE Expr DO S END.
|  S = [ Stmt { ";" Stmt } [";"] ].
|  Expr = ... .

Names that appear to the left of the "=" sign are called
"non-terminals".  Names, like "WHILE", that only appear on the
right-hand side are called "terminals". A terminal may denote a
language keyword like "WHILE" or an entity that is defined elsewhere,
for example a "TEXT" literal defined by the Modula-3 token grammar.

Such grammars define the {\it external} form of programs, which is
biased towards ease of reading by programmers and parsing by
compilers. As a result they tend to obscure the {\it essential}
structure of the language. For example, what is really important about
a "WHILE" statement is that it contains an "Expr" and an "S"
construct; the keywords "WHILE", "DO" and "END" could be altered
without changing the meaning, as indeed could the order of occurrence
of the constructs. It should be obvious that the details of the
external syntax are of no help in analysing or reasoning on the
essential properties of programs. Therefore, we seek a more abstract
representation of programs that carries only the essential
information.

We will refer to the syntax of the external form of a language as the
{\it concrete} syntax. If we abstract from the representational
details of the concrete syntax, what we are left with is an {\it
abstract} syntax for the language, defined by an {\it abstract
grammar}.

Like the grammar for the concrete syntax, the abstract grammar is also
defined in terms of productions, terminals and non-terminals. To
define the abstract grammar we will use a slightly different syntax
for productions, one that is closer to the syntax of aggregate types
(e.g. "RECORD") in programming languages.  This syntax also
facilitates the mapping of the abstract syntax into Modula-3 data
types.

The abstract grammar defines a finite set of {\it constructs}
using a finite set of {\it productions}, defined according to
the conventions in the Modula-3 report, as follows:

|  Production = Construct "=" Aggregate | Choice .
|  Aggregate = { "[" AttributeName ":" AttributeType ";" "]" } .
|  Choice = Construct { "|" Construct } .
|  AttributeType = Construct | "{" Construct "}" .

A construct that is not defined by a production is a terminal;
otherwise it is a non-terminal. An "AttributeName" is a tag that
distinguishes repeated occurrences of the same construct in a
production. By convention "Choice" constructs are spelled in
upper-case and "Aggregate" constructs begin with an upper-case letter.

The abstract grammar production for a "WhileSt" might be defined as:

|  WhileSt = as_expr: EXPR; as_s: Stmts .
|  Stmts = as_stmt_s: { STM } .
|  STM = WhileSt | ... .

In the same way that a program defined by the concrete grammar has an
associated representation as a {\it parse tree}, so the equivalent
program defined in terms of the abstract grammar has a representation
as an {\it abstract syntax tree}. The leaves of the tree denote
terminal constructs, the interior nodes represent non-terminal
constructs, and the attribute names label the branches. The above
definition of the abstract grammar permits a node to have either a
fixed number of descendent nodes, a variable number, or a combination
of both.  Some abstract grammar definitions restrict a node to have
either a fixed number of descendents or a variable number but not
both.  However, this requires more constructs in the grammar. Indeed
the "WhileSt" production can be rewritten more compactly as follows:

|  WhileSt = as_expr: EXPR; as_stmt_s: { STM } .
|  STM = WhileSt | ... .

The acronym "AST" is widely used in place of abstract syntax tree. An
AST and its associated parse tree will always be similar in form;
however, the AST will usually contain less nodes, due to the reduction
in the number of constructs.

It is important to note that when we talk of a {\it tree}, we are
referring to the {\it abstraction} of a tree. It is not implied
that there is an actual data structure containing records and
pointers. Such a structure is only one possible representation,
although a convenient and common choice.

\subsection{Specifying an AST in Modula-3}

This section describes in general terms how the definition of a
language in terms of an abstract grammar, and its associated AST, is
mapped into an abstraction using the Modula-3 programming language.

There is an an obvious mapping from an "Aggregate" construct into
either a "RECORD" type or an "OBJECT" type. We choose the latter
because it supports abstraction better (the representation of
the components can more easily be hidden from a client) and
because the subtyping of "OBJECT" types directly supports the
"Choice" construct. For example, the production:

|  T = A | B | C.

can be expressed using objects as:

|  TYPE T <: ROOT; A <: T; B <: T; C <: T;

Modula-3 not only supports abstraction directly through interfaces and
opaque object types, it is also augmented with a formal specification
language called Larch/Modula-3, or LM3 for short. Amongst other things
LM3 allows object types to be annotated with "abstract" or
"specification" fields, thus permitting one to reason about these fields
without having to commit to a representation as an actual object
field. We will use specification fields to denote the attributes of an
aggregate production. LM3 also provides the notion of a {\it sequence}
as a primitive, again allowing the representation to be deferred.  We
will use this facility to denote attributes that represent sequences
of nodes.

In this formalism, the "WhileSt" example would be defined as:

|  TYPE WhileSt <: STM;
|  <* FIELDS OF WhileSt
|       as_expr: EXPR;
|       as_stmt_s: SEQUENCE OF STM; *>

This is a simple, clear and concise notation in which to define an AST
for a given programming language. Furthermore, because it is defined
by a formal language, it offers the prospect of that some components
of a programming environment for the given language could be generated
automatically. For example, code could be generated that will
systematically visit every node in an AST. Note that the "type" of an
attribute must either be an object type denoting an abstract grammar
construct, or a sequence of such types. Later we will see how to add
additional attributes that can be any type allowed by the Modula-3
language.

The "WhileSt" node is an example of a situation that occurs frequently
in which all the members of a "Choice" construct in the grammar share
a common attribute. In such cases we can simplify the AST
specification by associating the attribute with the class that denotes
the choice.  The different alternatives then inherit the attribute by
the normal rules of object subtyping.

Ultimately, this abstract definition of an AST node must be given a
concrete representation. There are many possible representation for
such a tree and there are trade-offs to be made between storage
requirements and processing time. It is not the role of this interface
to make those choices, only to provide mechanisms and standards to
support them.

\subsection{Augmenting an AST}

There are many reasons why we might want to augment an AST definition
with additional constructs and attributes. The overriding reason is
that the abstract grammar is unable to define all the constraints on a
legal program.  program. Many of the programs that can be generated
from the grammar are in fact illegal. There is no easy way in such a
context free grammar to express the fact that, for example, the type
of the "EXPR" construct of the "WhileSt" must be "BOOLEAN". Nor that
all identifier uses must be bound to a declaration in scope.  Such
constraints are defined by what is referred to as the "static
semantics" of the language, as opposed to the "dynamic semantics"
which define the meaning of the program when it is executed.

So, in order to analyse programs in a useful way we need, at the very
least, some way to represent the results of the static semantic
analysis.  The obvious way to do this is to augment certain nodes in
an AST with additional attributes that specify this information, for
example the type of expressions.  Choosing which nodes to annotate
and precisely what information to represent is a tricky task and
beyond the scope of this discussion. We can observe, however, that the
formalism that we have chosen is quite capable of specifying these
additional attributes, merely by adding them to the "FIELDS"
declaration. The result is an AST augmented with the additional
attributes. In truth the term augmented AST is somewhat of a misnomer
since there is nothing to prevent the augmented attributes in several
nodes having as value a reference to the same node, thus forming a
directed graph structure. However, it is conventional to continue to
use the term "AST" to refer to the entire structure. Note also that, unlike
the the attributes that denote the {\it pure} abstract syntax tree,
the additional attributes can be of any type that is provided by the
Modula-3 language.

Finally we can observe that practically all analyses of an AST will
generate additional information that is of potential use by other
tools.  It is generally very much easier to attach this information
directly to the AST than to create a separate data structure.

\subsection{AST Layers}

The previous section suggested augmenting the AST with attributes that
capture the result of an analysis by a given tool. In a rich
programming environment, consisting of many tools, many additional
sets of attributes might be defined. If these attributes are all
declared in the same interface, the result will be overwhelming. In
addition, the scope for separate development will be greatly reduced.
Previous AST designs, for example, DIANA, have specified the
attributes for a fixed set of tools, thus placing additional tools at
a disadvantage. One solution is to define a {\it property set}
attribute on each node, that is capable of storing many attributes of
different types, but this has the disadvantages of storage and access
time overheads plus a more complex programming interface.

The basic idea is to define each set of nodes and attributes independently,
and then define an AST as a combination of some or all of these
sets. We refer to each set as an AST {\em layer} or, occasionally,
a {\em view}. 

\subsection{AST Layers in Modula-3}

This section describes how the notion of separate layers of an AST
is mapped into the facilities available in Modula-3. The solution
makes extensive use of {\it interfaces} and {\it partial revelations}. 

An AST for a specific language is specified as a set of interfaces,
which share the naming convention "LLAST", where "LL" is a
language-specific prefix, e.g. "M3", for the Modula-3 AST. Each AST
layer is defined in an interface named "LLAST_VV", where "VV" is a tag
denoting the layer, for example, "AS" for the syntactic layer. Each
interface defines the attributes available on a given node using the
"FIELDS OF" mechanism introduced previously. In order to quickly
relate an attribute to the layer in which is declared, attribute names
are given a prefix of the form "vv_", for example, "as_exp".

All AST nodes are declared to be subtypes of the "AST.NODE" class,
defined by this interface. An implementation of the interface will
define a set of standard methods that support common actions, such as
systematically visiting nodes in the AST.  This interface does not
prescribe a set of methods; this is left to other interfaces.

Non-terminals that are defined as a set of alternatives using the
"Choice" construct in the abstract grammar are named using all upper
case characters. There are never any instances of these nodes in an
AST.

Sequence attributes are named with a suffix "_s", to distinguish them
from other attributes.

Because of the prevalence of prefixes and suffixes in attribute names,
the standard Modula-3 capitalisation rules for multiword identifiers
are modified. Instead of "WhileSt", we write "While_st", and an
attribute of such a type in the "AS" layer would be named
"as_while_st", rather than "as_whileSt".

Using these conventions, interfaces are constructed as follows. The 
"WhileSt" grammar fragment is used as an example.

|  INTERFACE LLAST_AS;
|  IMPORT AST;
|  TYPE
|    STM <: AST.NODE; EXPR <: AST.NODE;
|    WhileSt <: STM;
|    <* FIELDS OF WhileSt
|         as_expr: EXPR;
|         as_stmt_s: SEQUENCE OF STM; *>
|

Now consider a {\em static semantic} layer for the above grammer, in which
all "EXPR" nodes are given a "type", denoted by the class "LTYPE".

|  INTERFACE LLAST_SM;
|  IMPORT AST, LLAST_AS;
|  TYPE
|    LTYPE <: AST.NODE;
|
|  <* FIELDS OF LLAST_AS.EXPR
|       sm_ltype: LTYPE; *>


\subsubsection{Defining the concrete representation}

In practice there are two ways in which to represent the abstract
attributes defined in the "FIELDS" specifications, either directly as
fields of the object type, or indirectly through methods that set or
get the value. The convention is to reveal these access methods in
interfaces called "LLAST_VV_F" and "LLAST_VV_M", respectively. In
either case this interface will contain, for each node to be
attributed, an object type with the same name as the abstract type,
plus a partial revelation that the abstract type is a subtype of this
new object type.


|  INTERFACE LLAST_AS_F;
|  IMPORT AST, LLAST_AS;
|  TYPE
|    While_st = LLAST_AS.STMT OBJECT
|      as_expr: LLAST_AS.EXPR := NIL;
|      as_stmt_s := REF ARRAY OF LLAST_AS.STMT := NIL;
|    END;

|  REVEAL
|    LLAST_AS.While_st <: While_st;

|  TYPE EXPR = LLAST_AS.EXPR; STMT = LLAST_AS.STMT;
|  END LLAST_AS_F.

When the "LLAST_AS_F" interface is imported, it allows access to
"as_expr" and "as_stmt_s", and the "REVEAL" statement tells the
compiler that the actual node will have all of these fields, and
possibly some more. It is conventional to "pass through" the node and
class types that are not revealed, for example "EXPR" in the above, to
ensure that the next layer can access all the node names by referrring
to this layer alone.

The static semantic attributes can be given a similar concrete
representation as follows:

|  INTERFACE LLAST_SM_F;
|  IMPORT AST, LLAST_AS, LLAST_SM;
|  IMPORT LLAST_AS_F AS Previous_View;

|  TYPE 
|    EXPR = Previous_View.EXPR OBJECT
|      sm_ltype: LLAST_SM.LTYPE;
|    END;

|  REVEAL 
|    LLAST_AS.EXPR <: EXPR;
|  TYPE
|    While_st = Previous_View.While_st;
|    STM = Previous_View.STM;
|    LTYPE = LLAST_SM.LTYPE;
|  END LLAST_SM_F.

Notice that in this layer the supertype is inherited from the previous
view, in this case, "LLAST_AS_F".

Owing to the constraints of Modula-3 object subtyping, it is necessary
to know the name of the layer that last added attributes to the node.
If you get this wrong you will get an error message complaining about
incompatible revelations at some point. However, whether this occurs
at compile, link, or run-time depends on the Modula-3 implementation.

Ultimately, there must be an interface or module that chooses which layers
will actually exist in a given program, by making a concrete revelation
containing the corresponding declaration in the lowest layer that
is to be included. This is conventionally named "LLAST_all". E.g.

|  INTERFACE LLAST_all;
|  IMPORT LLAST_AS, LLAST_SM;
|  IMPORT LLAST_SM_F AS Last_View;

|  REVEAL LLAST_AS.STMT = Last_View.STMT BRANDED OBJECT END;
|  REVEAL LLAST_AS.EXPR = Last_View.EXPR BRANDED OBJECT END;
|  REVEAL LLAST_AS.While_st = Last_View.While_st
|                                        BRANDED OBJECT END;
|  REVEAL LLAST_SM.LTYPE = Last_View.LTYPE 
|                                        BRANDED OBJECT END;
|  END LLAST_all;


At first sight, this may all seem rather complicated.  However,
AST specifications for real languages and tools are inherently complex
and there is much value to be gained in separating the specification
into more manageable pieces. For example, the syntactic, semantic and
code-generator attributes can be specified independently.  To
understand the syntactic specification, there is no need to see or
understand the other two. Secondly, it is possible to replace a layer
without affecting any of its ancestors, or add a completely new layer,
for example to support a new programming environment tool. The key
point to note is that although there may be many layers of a node,
there is only one actual node type. Whenever an instance of a node is
created it has the sum total of all the attributes that were specified
in the contributing layers. So, although a parser might be separately
compiled against the syntactic layer, it need only be relinked to
incorporate a new tool with its own layer. This greatly facilitates
the extensibility of the environment. It is perhaps unfortunate that
Modula-3 does not support multiple inheritance, which would avoid the
nuisance of the layer linearisation.  *)

(* \subsection{Basic Definitions} *)

INTERFACE AST;

TYPE 
  NODE <: ROOT;
  T = NODE;

(* An "AST.T" is denoted by the node that defines the root of the tree. 
A particular implementation of this interface will typically define
a standard set of methods on an "AST.NODE". *)

END AST.
