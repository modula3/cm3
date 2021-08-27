(* Copyright (C) 2021 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

(*  
  A Json.T represents a JSON node which can be any of the json types
  or the root of a json tree. This implementation distinguishes
  between integer nodes and floating point ones. The Json standard
  treats them all as numbers.
*)

INTERFACE Json;

IMPORT Rd;

TYPE
  NodeKind = {nkObject, nkArray, nkNull, nkBool, nkInt, nkFloat, nkText, nkRoot};
  KindNames = ARRAY NodeKind OF TEXT;

CONST 
  NK = KindNames{"Object","Array","Null","Bool","Int","Float","Text","Root"};

EXCEPTION E;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    name() : TEXT;
    value() : TEXT;
    kind() : NodeKind;
    format() : TEXT;
    find(path : TEXT) : T;
    addText(name,value : TEXT) : T;
    addInt(name : TEXT; value : INTEGER) : T;
    addFloat(name : TEXT; value : LONGREAL) : T;
    addBool(name : TEXT; value : BOOLEAN) : T;
    addNull(name : TEXT) : T;
    addObj(name : TEXT; obj : T := NIL) : T;
    addArr(name : TEXT; arr : T := NIL) : T;
    updateText(name,value : TEXT);
    updateInt(name : TEXT; value : INTEGER);
    updateBool(name : TEXT; value : BOOLEAN);
    updateFloat(name : TEXT; value : LONGREAL);
    updateArrObj(name : TEXT; value : T);
    delete(name : TEXT) : T;
    iterate() : Iterator;
    clear();
    root() : T;
  END;

  Iterator = OBJECT METHODS
    init() : Iterator;
    next(VAR name : TEXT; VAR value : T) : BOOLEAN;
  END;

  PROCEDURE ParseFile(f : TEXT) : T RAISES{E};
  PROCEDURE ParseStream(rd : Rd.T) : T RAISES{E};
  PROCEDURE ParseBuf(buf : TEXT) : T RAISES{E};

END Json.

(*

  The call to "json.name" returns the name of the node. eg for a text
  node "addr": "main street" would return "addr".

  The call to "json.value" returns the value of the node except
  for the structured nodes array and object in which case the values "_Arr_"
  or "_Obj" is returned. 
  For a text node "addr": "main street" would return "main street".
  For an int node eg "number": 1234 the text value "1234" would be returned.
  Other values are.
  "realval": 12.3e2    return "12.3e2"
  "boolval": true      return "true"
  "nullval": null      return "null"

  The call "json.kind" returns the type of the node.

  The call "json.format" returns the textual representation of the
  json tree. eg.
  "{
     "name": "sam",
     "obj": {
        "elt1": "hydrogen",
        "elt2": "oxygen"a},
     "arr1": [red,yellow,blue]
   }"

  The call "json.find" returns the node which matches the path. A path is
  defined as a sequence of names separated by "/" much like a unix directory
  path. eg the path "/obj/elt2" would return the text node for "elt2" in
  the previous example. 

  The call "json.addText(name,value)" adds a text node with the value "value"
  the node can only be added to a structured node array or object.

  Similarly the other calls except addObj and addArr add the appropriate
  typed node with a conversion of the value argument to a text. eg
  "json.addFloat("realnum",23.2e2)" adds a float node with the value "23.2e2"
  
  The call "json.addObj(name,obj := NIL) adds an object. If obj is NIL the
  node added to json is empty otherwise the obj becomes a child of json.

  The call "json.addArr(name,arr := NIL) adds an array. If arr is NIL the
  node added to json is empty otherwise the arr becomes a child of json.

  The calls updateText, updateInt, updateBool, updateFloat update the node
  whose name is name with the new value value.

  The call "json.updateArrObj(name, value)" updates the node json with
  name "name" with the new value "value", overwriting the old value.

  The call "json.delete(name)" deletes the child node named "name" of json.

  The call "json.iterate" returns a new iterator of the node json.
  The iterator calls "next(name,value)" return the next name and value
  until it returns false.

  The call "json.clear" deletes all children of json.

  The call "json.root" returns the root node of the tree of nodes.
  
  The procedure ParseFile(f) parses the input file "f" and returns a 
  root node of a tree of json nodes if successful. Otherwise it raises 
  an exception.

  The procedure ParseBuf(buf) parses the input text buffer "buf" and
  returns the root node of a json tree, otherwise it raises an 
  exception.

  The procedure ParseStream(rd) parses the input stream "rd" and returns
  the root node of a json tree else it raises an exception.
*)

