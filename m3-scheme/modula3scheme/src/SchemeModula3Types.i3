(* $Id$ *)

INTERFACE SchemeModula3Types;
IMPORT SchemeObject, Scheme, SchemePrimitive;

(* convert to and from Modula-3 base types *)

PROCEDURE ToScheme_MUTEX(m : MUTEX) : SchemeObject.T;
  (* returns itself *)

PROCEDURE ToScheme_TEXT(t : TEXT) : SchemeObject.T;
  (* returns SchemeString.T *)

PROCEDURE ToScheme_UNTRACED_ROOT(u : UNTRACED ROOT) : SchemeObject.T;
TYPE UntracedRoot = REF RECORD u : UNTRACED ROOT END;

PROCEDURE ToScheme_ROOT(r : ROOT)          : SchemeObject.T;
  (* returns itself *)

PROCEDURE ToScheme_ADDRESS(a : ADDRESS) : SchemeObject.T;
TYPE Address = REF RECORD a : ADDRESS END;

PROCEDURE ToScheme_REFANY(r : REFANY) : SchemeObject.T;
  (* returns itself *)

PROCEDURE ToScheme_EXTENDED(x : EXTENDED) : SchemeObject.T;
  (* returns a SchemeLongReal.T *)

PROCEDURE ToScheme_REAL(r : REAL) : SchemeObject.T;
  (* returns a SchemeLongReal.T *)

PROCEDURE ToScheme_LONGREAL(l : LONGREAL) : SchemeObject.T;
  (* returns a SchemeLongReal.T *)

PROCEDURE ToScheme_CHAR(c : CHAR) : SchemeObject.T;
  (* returns a SchemeChar.T *)

PROCEDURE ToScheme_BOOLEAN(b : BOOLEAN) : SchemeObject.T;
  (* returns a SchemeBoolean.T *)

PROCEDURE ToScheme_CARDINAL(c : CARDINAL) : SchemeObject.T;
  (* returns a SchemeLongReal.T *)

PROCEDURE ToScheme_INTEGER(i : INTEGER) : SchemeObject.T;
  (* returns a SchemeLongReal.T *)

(**********************************************************************)

PROCEDURE ToModula_MUTEX(m : SchemeObject.T) : MUTEX RAISES { Scheme.E };

PROCEDURE ToModula_TEXT(t : SchemeObject.T) : TEXT RAISES { Scheme.E };

PROCEDURE ToModula_UNTRACED_ROOT(u : SchemeObject.T) : UNTRACED ROOT RAISES { Scheme.E };

PROCEDURE ToModula_ROOT(r : SchemeObject.T) : ROOT RAISES { Scheme.E };

PROCEDURE ToModula_ADDRESS(a : SchemeObject.T) : ADDRESS RAISES { Scheme.E };

PROCEDURE ToModula_REFANY(r : SchemeObject.T) : REFANY RAISES { Scheme.E };

PROCEDURE ToModula_EXTENDED(x : SchemeObject.T) : EXTENDED RAISES { Scheme.E };

PROCEDURE ToModula_REAL(r : SchemeObject.T) : REAL RAISES { Scheme.E };

PROCEDURE ToModula_LONGREAL(l : SchemeObject.T) : LONGREAL RAISES { Scheme.E };

PROCEDURE ToModula_CHAR(c : SchemeObject.T) : CHAR RAISES { Scheme.E };

PROCEDURE ToModula_BOOLEAN(b : SchemeObject.T) : BOOLEAN RAISES { Scheme.E };

PROCEDURE ToModula_CARDINAL(c : SchemeObject.T) : CARDINAL RAISES { Scheme.E };

PROCEDURE ToModula_INTEGER(i : SchemeObject.T) : INTEGER RAISES { Scheme.E };

CONST Brand = "SchemeModula3Types";

TYPE 
  Type = { T_LONGINT, T_WIDECHAR,
         
           T_MUTEX , T_TEXT ,
           T_UNTRACED_ROOT , T_ROOT , T_ADDRESS ,
           T_REFANY , T_EXTENDED , T_REAL ,
           T_LONGREAL , T_CHAR , T_BOOLEAN ,
           T_CARDINAL , T_INTEGER,
           T_NULL};

CONST Name = ARRAY Type OF TEXT { "LONGINT", "WIDECHAR",
                                  
                                  "MUTEX" , "TEXT" ,
                                  "UNTRACEDROOT" , "ROOT" , "ADDRESS" ,
                                  "REFANY" , "EXTENDED" , "REAL" ,
                                  "LONGREAL" , "CHAR" , "BOOLEAN" ,
                                  "CARDINAL" , "INTEGER",
                                  "NULL" };

TYPE 
  Mode = { Reference, Concrete }; 
  (* is the input/return value of the routine the actual value or 
     a reference to it -- used to support language extensions *)

PROCEDURE ToModula_LONGINT(c : SchemeObject.T) : REFANY RAISES { Scheme.E };

PROCEDURE ToModula_WIDECHAR(i : SchemeObject.T) : REFANY RAISES { Scheme.E };

PROCEDURE ToScheme_LONGINT(i : REFANY) : SchemeObject.T;
  (* checked runtime error if it's not a REF LONGINT *)

PROCEDURE ToScheme_WIDECHAR(i : REFANY) : SchemeObject.T;
  (* checked runtime error if it's not a REF WIDECHAR *)
  
CONST
  ProcMode = ARRAY Type OF Mode {
  Mode.Reference, Mode.Reference,

  Mode.Concrete, ..
  };

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;
  (* extend with primitives:

     (scheme-modula-conversion-mode <type-symbol-name>)
     ;; returns 'Reference or 'Concrete 
  *)
  

END SchemeModula3Types.
