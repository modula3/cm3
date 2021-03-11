(* $Id$ *)

GENERIC INTERFACE Trie(Key, Value);

(* 
   Key.T must be a type that can be used as an array index, as in
   ARRAY Key.T OF ... 
   Key.Brand and Value.Brand must be TEXT constants

   Value.T can be any type that isn't an open array.

   The methods work as in Table(K,Value) where K.T is of type ARRAY OF Key.T, 
   where Key.Equal is implemented by the built-in =.
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    get(READONLY k : ARRAY OF Key.T) : Value.T;

    put(READONLY k : ARRAY OF Key.T; READONLY v : Value.T) : Value.T;
    
    init(READONLY defValue : Value.T) : T;

    iterate() : Iterator;
  END;

  Iterator <: PublicIterator;

  PublicIterator = OBJECT METHODS
    next(VAR v : Value.T) : BOOLEAN;
  END;

CONST Brand = Key.Brand & Value.Brand & "Trie";

END Trie.
