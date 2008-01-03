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


INTERFACE M3CUnit;

IMPORT Rd, M3FindFile, OSError;
IMPORT M3AST_AS, M3AST_FE;


TYPE
  (* for convenience we alias the types that this interface uses from 
     M3AST_FE. *)

  Uid = M3AST_FE.Unit_uid;
  State = M3AST_FE.Unit_state;
  Status = M3AST_FE.Unit_status;
  Type = M3AST_FE.Unit_type;

  TypeSet = SET OF Type;

CONST
  Errors = M3AST_FE.Unit_Errors;
  AllPhases = M3AST_FE.Unit_AllPhases;
  AllTypes = TypeSet{FIRST(Type)..LAST(Type)};
  Interfaces = TypeSet{Type.Interface,
                       Type.Interface_gen_def,
                       Type.Interface_gen_ins};
  Modules = TypeSet{Type.Module,
                    Type.Module_gen_def,
                    Type.Module_gen_ins};


TYPE
  Form = {Source, Ast, DontCare};

PROCEDURE FindUnit(
    f: M3FindFile.T;
    name: TEXT;
    unitType: Type;
    VAR (*inout*) uf: Form;
    VAR (*out*) id: Uid)
    : Rd.T
    RAISES {OSError.E};
(* Given a name and "unitType", indicating whether an interface or a module is
required, "FindSource" attempts to open a stream to an file containing a
representation of a compilation unit. The representation can be requested as:
i) A source file; in this case "id" is set to be a unique id for the source
and the returned stream can be used to parse the source.
ii) A pickled graph; in this case "id" is set to a unique id for the pickle
and the returned stream should be used to "unpickle" the graph.
iii) Dontcare.  Either will do.

If "FindUnit" fails to find a file containing the requested
compilation unit, it returns "NIL".  If an errors occurs in opening a
found file, "OSError.E" is raised, as per "FileRd.Open". *)

PROCEDURE FindStandard(
    f: M3FindFile.T;
    VAR (*inout*) uf: Form;
    VAR (*out*) id: Uid): Rd.T RAISES {M3FindFile.Failed, OSError.E};
(* This procedure is a special case of "FindUnit". It attempts to open a
stream to the representation of the standard interface.  *)


PROCEDURE TextName(id: Uid): TEXT;
(* Given a "Uid" this procedure returns a text, suitable for use
in error messages *)

PROCEDURE TypeName(ut: Type): TEXT;
(* Returns "interface" or "module" based on "ut". *)

PROCEDURE ToType(u: M3AST_AS.UNIT): Type;
(* u # NIL, maps from ISTYPE(u, ..) to "Type". *)

PROCEDURE ToGenIns(
    cu: M3AST_AS.Compilation_Unit;
    VAR (*inout*) ut: Type): M3AST_AS.Compilation_Unit;
(* If "cu" is a generic instantiation, return the instantiated unit,
   (i.e. cu.as_root.sm_ins_comp_unit). The value of "ut" is also updated.
*)

PROCEDURE Equal(id1, id2: Uid): BOOLEAN;
(* TRUE if "id1" and "id2" represent the same unit *)

<*INLINE*> PROCEDURE InclState(VAR status: Status; state: State);
(* status := status + Status{state}; *)

<*INLINE*> PROCEDURE ExclState(VAR status: Status; state: State);
(* status := status - Status{state}; *)

END M3CUnit.
