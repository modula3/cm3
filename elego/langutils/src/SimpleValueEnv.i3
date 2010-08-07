(*---------------------------------------------------------------------------*)
INTERFACE SimpleValueEnv;

IMPORT Rd, TextList, TextSeq, TextTextTbl, Wr;
IMPORT MsgIF;

EXCEPTION E(TEXT);

TYPE
  Type = {None, Int, Nat, Text, List, Seq, Ref};

  IntObj = OBJECT
    val : INTEGER;
  METHODS
    init(v : INTEGER) : IntObj := IntObjInit;
  END;

  T <: Public;
  (*
    A SimpleTextEnv.T is a mapping from names to values. Values can
    be of Type.Int (INTEGER), Type.Nat (CARDINAL), Type.Text (TEXT), 
    Type.List (TextList.T), Type.Seq (TextSeq.T), and Type.Ref (REFANY).
    Each name can be bound to at most one value. An undefined name is
    of Type.None.

    Several types of values can be converted at lookup time, but the
    internally stored representation is only changed when a new
    binding is defined. All impossible conversions are fatal and cause
    a checked runtime error. This is done on purpose to facilitate the
    use of these value environments and avoid exceptions. If you don't
    want automatic conversions or program aborts, use the type()
    method.

    Environments may be hierarchically structured into a tree, so
    any T object can keep track of its parent environment. Lookups
    of values are recursively by default; definitions and deletions
    of bindings are not.

    The procedures in this module are thread-safe, i.e. they may be
    called by multiple concurrent threads without data corruption.
  *)

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(parent : T := NIL; msgif : MsgIF.T := NIL) : T;
    (* initialize an empty environment *)

    (*-----------------------------------------------------------------------*)
    parent() : T;
    (* Returns the parent environment, if defined, else NIL. *)

    (*-----------------------------------------------------------------------*)
    copy(env : T; deep := TRUE; rec := TRUE);
    (* copy all values from `env' overwriting existing bindings *)

    (*-----------------------------------------------------------------------*)
    defined(name : TEXT; rec := TRUE) : BOOLEAN;
    (* <=> `name' is bound to a value. If `rec' is TRUE, do recursive
       lookups in all parent environments, if needed.
       The type of `name' is `None' iff it is not defined. *)

    (*-----------------------------------------------------------------------*)
    type(name : TEXT; rec := TRUE) : Type;
    (* <=> the type of `name'. If `rec' is TRUE, do recursive
       lookups in all parent environments, if needed.
       The type of `name' is `None' iff it is not defined. *)

    (*-----------------------------------------------------------------------*)
    intVal(name : TEXT; rec := TRUE) : INTEGER;
    natVal(name : TEXT; rec := TRUE) : CARDINAL;
    refVal(name : TEXT; rec := TRUE) : REFANY;
    textVal(name : TEXT; rec := TRUE) : TEXT;
    listVal(name : TEXT; rec := TRUE) : TextList.T;
    seqVal(name : TEXT; rec := TRUE) : TextSeq.T;
    (* These methods look up a binding in the environment and return
       the value associated with `name', if needed converted to the
       appropriate type. 

       `revVal' returns an `IntObj' for integer and cardinal values.

       'textVal' returns decimal denotations for Int and Nat, and
       element lists separated by ' ' for List and Seq. Unknown Ref
       values cannot be converted to Text, List, Seq, Int, or Nat.

       `seqVal' and `listVal' return lists and sequences with one
       element for types Nat, Int, and Text. Ref cannot be converted 
       to List and Seq.

       `intVal' and `natVal' return the value of decimal denotations
       of Text, and the length of List and Seq. 

       List and Seq can be converted into each other.

       All other conversions are illegal and cause a checked runtime
       error. Use the `defined' and `type' methods to check in advance.
    *)

    (*-----------------------------------------------------------------------*)
    substTextVal(name : TEXT; rec := TRUE) : TEXT;
    (* Get a text value from the environment and substitute all 
       variable references.

       The value is the mapping from the environment
       after substitution of all process environment variables, 
       internal environment variables, and the special variables
       {HOME} and {USER}. 
       
       The procedures TextUtils.SubstEnvVars() and 
       SubstituteVariables() are used to perform the actual
       substitutions, which means that environment variables of the
       form $name and ${name} are recognized and internal variable
       names of the form {:name}, {!name}, and {?name}.
    *)

    (*-----------------------------------------------------------------------*)
    textValOrNil(name : TEXT; rec := TRUE) : TEXT;
    (* Like textVal, but returns NIL if no value is found. *)

    (*-----------------------------------------------------------------------*)
    substTextValOrNil(name : TEXT; rec := TRUE) : TEXT;
    (* Like substTextVal, but returns NIL if no value is found. *)

    (*-----------------------------------------------------------------------*)
    setIntVal(name : TEXT; val : INTEGER);
    setNatVal(name : TEXT; val : CARDINAL);
    setRefVal(name : TEXT; val : REFANY);
    setTextVal(name : TEXT; val : TEXT);
    setListVal(name : TEXT; val : TextList.T);
    setSeqVal(name : TEXT; val : TextSeq.T);
    (* Define a new binding of `name' to the given value: (name, val),
       which replaces any existing binding. *)
       
    (*-----------------------------------------------------------------------*)
    delVal(name : TEXT);
    (* Delete the binding of `name' in this environment. Do nothing
       if no binding exists. *)

    (*-----------------------------------------------------------------------*)
    setFromTextTextTbl(tbl : TextTextTbl.T) : T;
    (* Define text bindings in this environment by converting `tbl'
       appropriately. *)

    (*-----------------------------------------------------------------------*)
    toTextTextTbl(plain := TRUE; rec := TRUE) : TextTextTbl.T;
    (* Convert this environment to a TextTextTbl.T, ignoring all bindings
       with incompatible values. If `plain', then all variable references
       are substituted in the resulting mapping. *)

    (*-----------------------------------------------------------------------*)
    toText(rec := TRUE; plain := TRUE) : TEXT;
    (* Convert the bindings to a readable representation (if possible)
       for debugging purposes. *)

    (*-----------------------------------------------------------------------*)
    keys(rec := TRUE) : TextSeq.T;
    (* Returns all binding names as a sequence. *)

    (*-----------------------------------------------------------------------*)
    keyList(rec := TRUE) : TextList.T;
    (* Returns all binding names as a list. *)
  END;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
PROCEDURE SubstituteVariables(t : TEXT; parameters : T) : TEXT
  RAISES {Error};
  (* Substitute all variable references of the form {!name},
     {:name}, and {?name} in the text `t' with the values bound in 
     `parameters'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE TextSeq1(t : TEXT) : TextSeq.T;
  (* Return a text sequence of length 1. *)

(*---------------------------------------------------------------------------*)
PROCEDURE IntObjInit(self : IntObj; v : INTEGER) : IntObj;
  (* Initalize an integer object with value `v'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ListToSeq(l : TextList.T) : TextSeq.T;
  (* Convert a TextList.T to a TextSeq.T. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SeqToList(s : TextSeq.T) : TextList.T;
  (* Convert a TextSeq.T to a TextList.T. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ReadTextList(rd : Rd.T) : TextList.T RAISES {E};
  (* Read a text list denotation, which is either "NIL" or a comma-separated
     list of double-quoted encoded strings (non-printable characters
     appropriately escaped). *)

(*---------------------------------------------------------------------------*)
PROCEDURE ReadTextSeq(rd : Rd.T) : TextSeq.T RAISES {E};
  (* Read a text sequence denotation, which is either "EMPTY"
     or a comma-separated list of double-quoted encoded strings
     (non-printable characters appropriately escaped). *)

(*---------------------------------------------------------------------------*)
PROCEDURE Write(wr : Wr.T; env : T; keys : TextList.T := NIL; rec := TRUE)
  RAISES {E};
  (* Write environment `env' to writer `T'. Use only the elements in `keys'
     if the list is not NIL. Do recursive lookups if `rec' is TRUE. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Read(rd : Rd.T; VAR env : T) RAISES {E};
  (* Read bindings for the environment `env' from reader `rd' until
     EOF is encountered or an environment separator is found. The 
     bindings must have been written with Write (see above). *)

(*---------------------------------------------------------------------------*)
PROCEDURE WriteFile(fn : TEXT; env : T; keys : TextList.T := NIL; rec := TRUE)
  RAISES {E};
  (* As Write, but creates a file. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ReadFile(fn : TEXT; VAR env : T) RAISES {E};
  (* As Read, but reads directly from a file until EOF. *)

END SimpleValueEnv.
