(*--------------------------------------------------------------------------*)
INTERFACE APN;

(* This is an extension of the Pathname interface in libm3, written by
   members of DEC SRC. It exports an abstract data type and is intended
   to be more efficient for use in Tables and other hash structures, as
   hash-values are cached within the APN.T object *)

IMPORT Pathname, Word;

CONST Brand = "APNModule 0.1";

(*--------------------------------------------------------------------------*)
TYPE Type = {Posix, Win, Native, Default, Unknown};

(*--------------------------------------------------------------------------*)
TYPE
  T <: Default;
  Default = OBJECT
  METHODS
    init(pn : TEXT; type := Type.Default) : T;
    denotation(type := Type.Default) : Pathname.T;
    isValid() : BOOLEAN;
    isAbsolute() : BOOLEAN;
  END;

(*--------------------------------------------------------------------------*)
(* EXCEPTION Invalid; FIXME: seems to be unused. Just remove it? *)

(*--------------------------------------------------------------------------*)
PROCEDURE New(pn : TEXT; type := Type.Default) : T;
(* Return a newly allocated APN.T object with contents `pn' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Equal(p, q : T) : BOOLEAN;
(* returns true if p and q are textually equal *)

(*--------------------------------------------------------------------------*)
PROCEDURE Hash(pn : T) : Word.T;
(* hash function for pathnames *)

(*--------------------------------------------------------------------------*)
PROCEDURE Valid(pn: T): BOOLEAN;
(* Return "TRUE" iff "pn" conforms to the pathname syntax of this
   operating system. *)

(* When a pathname with invalid syntax is passed to a procedure in
   this interface not declared as raising the exception "Invalid",
   the result is undefined, but safe. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Absolute(pn: T): BOOLEAN;
(* Return "TRUE" iff "pn" is an absolute pathname.  Equivalent to
   "Decompose(pn).getlo() # NIL", but faster. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Prefix(pn: T): T;
(* Return a pathname equal to "pn" up to, but not including, the final
   arc name.  If "pn" consists only of a root directory name,
   "Prefix(pn)" returns "pn". *)

(*--------------------------------------------------------------------------*)
PROCEDURE Last(pn: T): T;
(* Return the final arc name in "pn".  If "pn" consists only of a root
   directory name, "Last(pn)" returns the empty string.  *)

(*--------------------------------------------------------------------------*)
PROCEDURE Base(pn: T): T;
(* Return a pathname equal to "pn" except with "Last(pn)" replaced by
   its base. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Join(pn, base: T; ext: TEXT): T;
PROCEDURE JoinS(pn:T; base, ext: TEXT): T;
(* Return a pathname formed by prepending "pn" to "base" (if "pn" is
   not "NIL") and appending "ext" to "base" (if "ext" is not "NIL").
   More precisely, this is equivalent to the following, in which "a"
   is a local variable of type "Arcs": *)
(*
| IF pn = NIL THEN a := NIL
| ELSE
|   IF Absolute(base) THEN `Cause checked runtime error` END;
|   a := Decompose(pn)
| END;
| IF ext # NIL THEN base := base & "." & ext END;
| RETURN Compose(
|   TextSeq.Cat(a, TextSeq.Sub(Decompose(base), 1)))
*)

(* The value returned by "Join" will be a valid pathname only if the
   "base" and "ext" conform to the syntax of the particular operating
   system, as specified at the end of this section. *)

(*--------------------------------------------------------------------------*)
PROCEDURE LastBase(pn: T): T;
(* Return the base of the final arc name of "pn".  It is a checked
   runtime error if "pn" is empty or consists only of a root directory
   name. *)

(*--------------------------------------------------------------------------*)
PROCEDURE LastExt(pn: T): TEXT;
(* Return the extension of the last arc name of "pn".  It is a checked
   runtime error if "pn" is empty or consists only of a root directory
   name. *)

(*--------------------------------------------------------------------------*)
PROCEDURE ReplaceExt(pn: T; ext: TEXT): T;
(* Return a pathname equal to "pn" except with the extension of the
   final arc name replaced with "ext", which must be non-"NIL". *)

(*--------------------------------------------------------------------------*)
PROCEDURE Denotation(pn : T; type := Type.Default) : Pathname.T;

(*--------------------------------------------------------------------------*)
VAR caseSensitivePathnames : BOOLEAN;
(* This value will be set to true on POSIX and FALSE on WIN32 systems
   by default. It may be changed to influence the pathname comparisons
   (Equal) and the internal representation (all lowercase if false).
 *)
END APN.

