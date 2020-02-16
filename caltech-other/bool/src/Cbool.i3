(* $Id$ *)

INTERFACE Cbool;
FROM Ctypes IMPORT void_star;
IMPORT Word;

(* C declarations *)

TYPE
  T = void_star;
  t = void_star;

  bool_var_t = Word.T;

  bool_var_t_star = UNTRACED REF bool_var_t;

  bool_list_t = RECORD
    v : bool_var_t_star;
    n : CARDINAL;
  END;

(* normal stuff *)

<*EXTERNAL bool_print*>
PROCEDURE print(b : t);

<*EXTERNAL null_ptr*>
PROCEDURE nullPtr() : t;

<*EXTERNAL "bool_init"*>    
PROCEDURE init  () : T;

<*EXTERNAL "bool_newvar"*>  
PROCEDURE newvar  (B : T) : t;

<*EXTERNAL "bool_and"*>  
PROCEDURE and  (B : T; a , b : t) : t;

<*EXTERNAL "bool_or"*>  
PROCEDURE or  (B : T; a , b : t) : t;

<*EXTERNAL "bool_xor"*>  
PROCEDURE xor  (B : T; a , b : t) : t;

<*EXTERNAL "bool_copy"*>  
PROCEDURE copy  (B : T; a : t) : t;

<*EXTERNAL "bool_not"*>  
PROCEDURE not  (B : T; a : t) : t;

<*EXTERNAL "bool_implies"*>  
PROCEDURE implies  (B : T; a , b : t) : t;

<*EXTERNAL "bool_maketrue"*>  
PROCEDURE maketrue  (B : T; a , b : t) : t;

<*EXTERNAL "bool_makefalse"*>  
PROCEDURE makefalse  (B : T; a , b : t) : t;

<*EXTERNAL "bool_free"*>
PROCEDURE free  (B : T; a : t);

<*EXTERNAL "bool_gc"*>
PROCEDURE gc (B : T);

<*EXTERNAL "bool_false"*>
PROCEDURE false(B : T) : t;

<*EXTERNAL "bool_true"*>
PROCEDURE true(B : T) : t;

<*EXTERNAL "bool_getid"*>
PROCEDURE getid(b : t) : INTEGER;

<*EXTERNAL bool_refs*>
PROCEDURE refs(B : T; b : t) : INTEGER;

END Cbool.









