INTERFACE Json;

IMPORT Rd;

TYPE
  NodeKind = {nkObject, nkArray, nkNull, nkBool, nkInt, nkFloat, nkText, nkRoot};

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
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
    addArr(name : TEXT) : T;
    updateText(name,value : TEXT);
    updateInt(name : TEXT; value : INTEGER);
    updateBool(name : TEXT; value : BOOLEAN);
    updateFloat(name : TEXT; value : LONGREAL);
    delete(name : TEXT) : T;
    clear();
    root() : T;
  END;

  PROCEDURE ParseFile(f : TEXT) : T;
  PROCEDURE ParseStream(rd : Rd.T) : T;
  PROCEDURE ParseBuf(buf : TEXT) : T;

(*
  maybe need a rethink. The T above at the moment represents a
  json tree as built for a certain parse. It has embedded in the
  root node. The Node is a seperate thing - data struct containting
  all the children.

  We could also define it as a Node a la pascal and parseFile returns a 
  node which is the root. So the client keeps the root safe.
  Because it is arbitrary node its methods operate as if its any node
  in particular find returns a T which update operates on.

  eg root := parseFile("file");
  txt := root.format();
  node := root.find("/path/to/obj");
  txt := node.format();
  n4 := node.delete("address");
  n3 := node.add("name","value");
  n2 := node.find("/abc/def");

*)

END Json.

