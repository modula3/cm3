INTERFACE M3Error;

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

IMPORT M3AST_LX, M3AST_AS;
IMPORT M3CSrcPos;

(* This interface supports the reporting and subsequent display
   of errors. Errors can be reported with reference to source
   position, for parsers, or by association with a node in the
   AST. Since any given node does not have a back pointer to
   the root, this interface defines the notion of the "current"
   unit, which must be set explicitly by the client before
   calling most procedures. *)

PROCEDURE SetCu(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* Since AST nodes may not contain a reference to the root, this
procedure must be called to establish the unit, before any calls to
the report procedures occur. 
*)


TYPE
  ERROR_NODE = M3AST_AS.SRC_NODE;

(* The following procedures report errors.  Errors are typically associated
   with AST nodes, with an implied unit and source position, and usually
   include identifier names.  Consequently, 'message' may contain 'Fmt' %s 
   specifiers. The method of error presentation is implementation dependent.*)

PROCEDURE Report(n: ERROR_NODE; message: TEXT) RAISES {}; 
(* Reports an error associated with node 'n'. *)

PROCEDURE ReportWithId(n: ERROR_NODE; message: TEXT;
    id1, id2, id3, id4: M3AST_LX.Symbol_rep := NIL) RAISES {};
(* Reports an error associated with node 'n', with optional arguments
denoting identifier names that correspond with %s specifiers in 'message'. *)

PROCEDURE ReportAtPos(pos: M3CSrcPos.T; message: TEXT) RAISES {};
(* If no node is available, e.g. some syntax errors, this associates
an error with source position 'pos'. *)

PROCEDURE Warn(n: ERROR_NODE; message: TEXT) RAISES {};
(* As 'Report', but only a warning. *)

PROCEDURE WarnWithId(n: ERROR_NODE; message: TEXT; 
    id1, id2, id3, id4: M3AST_LX.Symbol_rep := NIL) RAISES {};
(* As 'Report', but only a warning, with arguments as per WarnWithId. *)

PROCEDURE SuppressWarnings(b := TRUE) RAISES {};
(* Switch off/on the interpretation of warning messages. *)

(* These procedures control when errors are presented and allow a client
   to be notified when an error is reported by one of the above procs. *)

PROCEDURE Show(n: ERROR_NODE; forget := TRUE) RAISES {};
(* Show (in some implementation dependent way) any errors on node 'n',
in the current unit. The error is forgotten unless 'forget = FALSE'. *)

PROCEDURE ShowAll(n: ERROR_NODE; forget := TRUE) RAISES {};
(* Calls 'Show' for the entire subtree rooted at 'n', in the current
   unit. *)

TYPE
  Notification <: Notification_public;
  Notification_public = OBJECT
  METHODS
    notify(cu: M3AST_AS.Compilation_Unit; isError: BOOLEAN);
    (* "notify" will be called at least once for each unit in which 
       an error is found. The "cu" argument will only be non-NIL if the 
       error is not in the current unit, e.g. clashing declarations in 
       imported interfaces. The second arg is FALSE if only a warning 
       (i.e. from Warn(...))
    *)
  END;

PROCEDURE AddNotification(n: Notification);

PROCEDURE RemoveNotification(n: Notification);

END M3Error.
