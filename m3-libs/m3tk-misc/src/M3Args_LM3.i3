(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3Args_LM3;

(* \subsection{LM3 formal specification} *)

<* TRAITS M3Args(TEXT FOR S), Text(TEXT FOR S) *>

(* The following two types are used only within the formal specification
but must be declared at the M3 level because of current limitations in LM3.
*)

TYPE Key <: REFANY;
TYPE RefTextArray = REF ARRAY OF TEXT;

(* The set of registered tools is modelled as the set "toolSet", which
is a global specification variable. The "keySet" specificationvariable
contains the complete set of keys registered with all tools.  This is
needed to specify the behaviour if the implementation does not permit
duplicate keys, which is given by the specification variable
"noDuplicates".  In addition the {\it master} tool is denoted by
"masterTool". *)

<* 
  TYPE 
    ToolSet = ASET OF T;
    KeySet = ASET OF Key;

  VAR 
    toolSet: ToolSet;
    masterTool: T;
    currentTool: T;
    noDuplicates: BOOLEAN;
*>

(* A tool is modelled with the following specification fields. *)

<* FIELDS OF T
  name: TEXT;
    (* The name of the tool. *)
  keySet: KeySet;
    (* The set of keys that have been registered. *)
  nameToKeyMap: MAP TEXT TO Key;
    (* A map from key names to keys. *)
  flagMap: MAP TEXT TO BOOLEAN;
    (* A map from "Flag" key names to key values. *)
  stringMap: MAP TEXT TO TEXT;
    (* A map from "String" key names to key values. *)
  stringListMap: MAP TEXT TO RefTextArray;
    (* A map from "StringList" key names to key values. *) *>

(* The specifications fields of a "Key" correspond directly to the
arguments to the various "Register" calls. *)

<* FIELDS OF Key
  name: TEXT;
  usage: TEXT;
  shared: BOOLEAN;
  opt: Opt; *>

<* PROCEDURE New(toolName, toolDescription, version, master)
  GLOBALS toolSet, masterTool
  REQUIRES
    ((toolName # NIL \and Length(toolName) > 0) \and
    (toolDescription # NIL \and Length(toolDescription) > 0) \and
    (version # NIL \and Length(version) > 0))
  ENSURES (RESULT \in toolSet) \and FRESH(RESULT) \and
    (master \implies (masterTool = RESULT)) \and
    RESULT.keySet = {}  \and RESULT.flagMap = {} \and
    RESULT.stringMap = {} \and RESULT.stringListMap = {} \and
    RESULT.nameToKeyMap = {} \and RESULT.name = toolName
  EXCEPT
    | EXISTS t: T; (t \in toolSet) \and (t.name = toolName) =>
        RAISEVAL = CHECKEDRTE

PROCEDURE SetMaster(t)
  GLOBALS masterTool
  REQUIRES t # NIL
  ENSURES (masterTool' = t) \and (RESULT = masterTool)

PROCEDURE RegisterFlag(t, argName, usage, shared)
  GLOBALS noDuplicates, toolSet
  REQUIRES
    (t # NIL) \and IsWF_Keyword(argName) \and
    ((usage # NIL) \and (Length(usage) >= 0))
  ENSURES
    EXISTS k: Key; (k \in t.keySet) \and (k.name = argName) \and
                   (k.shared = shared) \and (k.usage = usage) \and
                   t.nameToKeyMap = bind(t.nameToKeyMap, argName, k)

  EXCEPT 
  | defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE
  | noDuplicates \and 
      (FORALL tx: T; (tx \in toolSet) \and 
          (EXISTS k: Key (k \in tx.keySet) \and (k.name = argName) \and
          \not(k.shared \and shared))) =>
      RAISEVAL = CHECKEDRTE            

PROCEDURE RegisterString(t, argName, usage, opt, shared)
  GLOBALS noDuplicates, toolSet
  REQUIRES ((t # NIL) \and IsWF_Keyword(argName)) \and
              ((usage # NIL) \and (Length(usage) >= 0)) 
  ENSURES
    EXISTS k: Key; (k \in t.keySet) \and (k.name = argName) \and
                  (k.shared = shared)

  EXCEPT 
  | defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

  | noDuplicates \and 
      (FORALL tx: T (tx \in toolSet) \and
          (EXISTS k: Key (k \in tx.keySet) \and (k.name = argName) \and
          \not(k.shared \and shared))) =>
      RAISEVAL = CHECKEDRTE 

PROCEDURE RegisterStringList(t, argName, usage, opt, shared)
  GLOBALS noDuplicates, toolSet
  REQUIRES ((t # NIL) \and IsWF_Keyword(argName)) \and
           ((usage # NIL) \and (Length(usage) >= 0)) 
  ENSURES (EXISTS k: Key; (k \in t.keySet) \and (k.name = argName) \and
                          (k.shared = shared))

  EXCEPT  
  | defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

  | noDuplicates \and 
        (FORALL tx: T; (tx \in toolSet) \and
        (EXISTS k: Key (k \in tx.keySet) \and (k.name = argName) \and
        \not(k.shared \and shared))) =>
      RAISEVAL = CHECKEDRTE 

PROCEDURE Find(t)
  GLOBALS toolSet, currentTool
  REQUIRES t \in toolSet
  ENSURES (currentTool = t)

  EXCEPT
  | EXISTS k: Key; (k \in t.keySet) \and (k.opt = Opt.Required) \and
                    \not(defined(t.flagMap, k.name)) =>
      RESULT = FALSE

PROCEDURE GetFlag(t, argName)
  GLOBALS toolSet
  REQUIRES (t \in toolSet)
  ENSURES 
    IF defined(t.flagMap, argName) THEN RESULT = t.flagMap[argName]
    ELSE RESULT = FALSE

  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

PROCEDURE GetString(t, argName)
  GLOBALS toolSet
  REQUIRES (t \in toolSet)
  ENSURES 
    IF defined(t.stringMap, argName) THEN RESULT = t.stringMap[argName]
    ELSE RESULT = NIL

  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
       RAISEVAL = CHECKEDRTE

PROCEDURE GetStringList(t, argName)
  GLOBALS toolSet
  REQUIRES (t \in toolSet)
  ENSURES 
    IF defined(t.stringListMap, argName) THEN 
        RESULT = t.stringListMap[argName]
    ELSE RESULT = NIL

  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

PROCEDURE SetFlag(t, argName, f)
  GLOBALS toolSet, noDuplicates
  REQUIRES (t \in toolSet) 
  ENSURES t.flagMap' = bind(t.flagMap, argName, f) \and
      (IF noDuplicates \and (t.nameToKeyMap[argName]).shared THEN
         (FORALL tx: T; 
            (tx \in toolSet) \and defined(tx.flagMap, argName) \implies
            tx.flagMap' = bind(tx.flagMap, argName, f))
       ELSE TRUE)
  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

PROCEDURE SetString(t, argName, val)
  GLOBALS toolSet
  REQUIRES (t \in toolSet)
  ENSURES 
    IF val = NIL THEN t.stringMap = delete(t.stringMap, argName)
    ELSE t.stringMap' = bind(t.stringMap, argName, val)
  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
      RAISEVAL = CHECKEDRTE

PROCEDURE SetStringList(t, argName, sl)
  GLOBALS toolSet
  REQUIRES (t \in toolSet)
  ENSURES 
    IF sl = NIL THEN t.stringListMap = delete(t.stringListMap, argName)
    ELSE t.stringListMap' = bind(t.stringListMap, argName, sl)
  EXCEPT 
  | \not defined(t.nameToKeyMap, argName) =>
    RAISEVAL = CHECKEDRTE
*>

END M3Args_LM3.

