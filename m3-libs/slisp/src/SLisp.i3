(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul 23 07:52:16 PDT 1993 by steveg                   *)
(*      modified on Tue May 18 18:28:14 PDT 1993 by muller                   *)

INTERFACE SLisp;

IMPORT Atom, RefList, Text, Rd, Sx, Wr;

EXCEPTION Error;

(* Builtin values in SLisp 

nil                     FALSE (empty list - "()")
t                       TRUE (any non-"nil" value is TRUE)
stdout                  a Writer for output

  Builtin operators in SLisp

(abort)                 raises SLisp.Error.

(and exps...)           Returns "t" if every "exp" evaluates to non-"nil" and
                        "nil" if any "exp" evaluates to "nil".  Only evaluates
                        "exps" until the first "exp" evaluates to "nil".

(append lists...)       Returns the list containing the elements of the list
                        resulting from evaluating "list1" followed
                        by the elements of "list2", "list3", ...

(apply symbol list)     Returns the list containing the elements resulting from
                        applying the function named "symbol" to each element in
                        the list resulting from evaluating "list".
                        Equivalent to:
                          (list (symbol (car list)) (symbol (cadr list)) ... )

(backtrace)             prints a stack dump to "stdout".

(cond ((e1 forms...)    Returns the result of evaluating the last form for the
       (e2 forms...)    first "ei" which evaluates to non-"nil" or "nil" if
       ...              every "ei" evaluates to "nil".
       (en forms...))   Evaluates the "ei" until one evaluates to non-"nil"
                        and then evaluates the forms associated with that "ei".
                        
(car list)              Returns the first element in the list which results
                        from evaluating "list".

(cdr list)              Returns the tail of the list which results
                        from evaluating "list".

(caar list)             "caar", "cadr", "cdar", "cddr" are shorthands for combinations
(cadr list)             of "car" and "cdr" where an "a" represents a "car" and "d"
(cdar list)             represents a "cdr".  i.e. (caar list) is equivalent to
(cddr list)             (car (car list)).

(concat str1 str2)      Returns the textual concatenation of the strings resulting from 
                        evaluating "str1" and "str2".

(cons exp list)         Returns the list whose first element is the result
                        of evaluating "exp" and whose tail is the result of
                        evaluating "list".

(cos exp)               Returns the cosine of the float value resulting from 
                        evaluating "exp".

(defun name             adds a function named "name" to the SLisp environment
       (formals .. )    which takes 0 or more arguments. 
       forms)

                        When the function is called SLisp creates a name scope with
                        each variable in "formals" bound to the value resulting from
                        evaluating the corresponding actual argument.  SLisp
                        then evaluates "forms".  The function returns
                        the value of the last form evaluated.

                        THE_REST - the last formal in the function definition may be
                                preceded by "THE_REST".  This causes any remaining 
                                actuals to be combined in a list and 
                                bound to the last formal.

                        NO_EVAL, _EVAL - a sequence of formals may be between a 
                                NO_EVAL/_EVAL pair.  When the function is 
                                called, the corresponding actuals for the formals
                                are NOT evaluated before being bound.

(defmacro name          adds a macro named "name" to the SLisp environment
          (formals ...) which takes 0 or more arguments.  
          forms)
                        
                        When the macro is invoked it does NOT evaluate its actuals 
                        before binding them to the formals.  The macro "forms" are then 
                        evaluated to produce a new form which is evaluated in turn 
                        and its result is returned.

(eq exp1 exp2)          Returns "t" if the values resulting from evaluating
                        "exp1" and "exp2" are equal and "nil" otherwise.
                        Equality of strings is done textually.  Equality of
                        lists is not supported and equality of everything else
                        is by value.

(eval exp)              Evaluates "exp" and then returns the result of evaluating
                        the previous result.

(float exp)             Returns the result of evaluating "exp" and converting
                        it to a float.

(floatp exp)            Returns "t" if the result of evaluating "exp" is a 
                        float and returns "nil" otherwise.

(ge exp1 exp2)          Returns "t" if the value resulting from evaluating
                        "exp1" is greater than or equal to the value from "exp2"
                        and "nil" otherwise.
                        Comparison of strings is done textually.  Comparison of
                        lists is not supported and comparison of everything else
                        is by value.

(get_prop exp)          Returns any property set with "setprop" on the value
                        resulting from evaluating "exp".
        
(gt exp1 exp2)          Returns "t" if the value resulting from evaluating
                        "exp1" is greater than the value from "exp2"
                        and "nil" otherwise.
                        Comparison of strings is done textually.  Comparison of
                        lists is not supported and comparison of everything else
                        is by value.
        
(if exp then-form       Returns the result of evaluating "then-form" if
        else-forms...)  "exp" is non-"nil" and returns the result of evaluating
                        "else-forms..." otherwise.

(integerp exp)          Returns "t" if the result of evaluating "exp" is a 
                        integer and returns "nil" otherwise.
                        
(intern string)         Returns a symbol with name "string". 
                        Evaluates "string" and creates a global variable with
                        that name and a value of "nil".

(le exp1 exp2)          Returns "t" if the value resulting from evaluating
                        "exp1" is less than or equal to the value from "exp2"
                        and "nil" otherwise.
                        Comparison of strings is done textually.  Comparison of
                        lists is not supported and comparison of everything else
                        is by value.

(length exp)            Returns the length of the value resulting from evaluating
                        "exp".  If the value is a list, its length is then
                        number of elements in the list.  If it is a string, its
                        length is the number of characters.  Otherwise, its 
                        length is 1.
        
(let ((sym1 exp1)       Creates a new name scope, with names "sym1", "sym2", ...
      (sym2 exp2)       bound to the value of evaluating "exp1", "exp2", ...
      ...)              "forms" are evaluated and the result is the value
  forms...)             of the last form.  Note: the names "sym1" .. "sym(i-1)"
                        are NOT available when evaluating "exp(i)".

(letstar ((sym1 exp1)   Creates a new name scope as in "let", except that the
         (sym2 exp2)    values of "sym1", "sym2", ... "sym(i-1)" are available
          ...)          when evaluating "exp(i)".
  forms...)      

(list exps...)          Returns the list whose elements are the values resulting from
                        evaluating "exps".

(listp exp)             Returns "t" if the result of evaluating "exp" is a 
                        integer and returns "nil" otherwise.

(load filename)         Evaluates "filename" to a string.  The string is used
                        as the name of a file which is opened and read.
                        The SLisp code in the file is evaluated.  The result
                        is equivalent to textually inserting the contents of the
                        file at this point.

(lt exp1 exp2)          Returns "t" if the value resulting from evaluating
                        "exp1" is less than or equal to the value from "exp2"
                        and "nil" otherwise.
                        Comparison of strings is done textually.  Comparison of
                        lists is not supported and comparison of everything else
                        is by value.

        
(ne exp1 exp2)          Returns "nil" if the values resulting from evaluating
                        "exp1" and "exp2" are equal and "t" otherwise.
                        Equality of strings is done textually.  Equality of
                        lists is not supported and equality of everything else
                        is by value.

(not exp)               Returns "t" if the value of "exp" is "nil", otherwise
                        returns "nil".

(or exps...)            Returns "nil" if every "exp" evaluates to "nil" and
                        "t" if any "exp" evaluates to non-"nil".  Only evaluates
                        "exps" until the first "exp" evaluates to non-"nil".

(print exps...)         Prints the values from evaluating "exps" to "stdout".  The result
                        is the value of the last expression.

(printname symbol)      Returns the string name of "symbol".

(progn forms...)        Evaluates "forms", and returns the result of evaluating the
                        last form.

(quote exp)             Returns "exp" un-evaluated.

(random)                Returns a pseudo-random float between 0.0 and 1.0 (inclusive).

(randomgen exp)         Returns a pseudo-random number generator produced by
                        NEW(Random.Default).init(exp # NIL)

(randomnext exp)        Returns a pseudo-random float between 0.0 and 1.0 (inclusive).
                        "exp" must evaluate to a random number generator returned
                        by a call on "randomgen".

(round exp)             result is the value of evaluating "exp" and rounding
                        it to an integer.

(set_prop exp value)    Evaluates "value" and sets it as the property on the value
                        resulting from evaluating "exp".  "value" can be retrieved
                        from "exp" by "getprop".
        
(setq symbol exp)       Evaluates "exp" and binds the result to "symbol", 
                        returns the result.  If "symbol" is not defined, it 
                        is created as a global.

(sqrt exp)              Returns the aquare root of the float value resulting from 
                        evaluating "exp".

(sin exp)               Returns the sine of the float value resulting from 
                        evaluating "exp".

(stringp exp)           Returns "t" if the result of evaluating "exp" is a 
                        string and returns "nil" otherwise.

(symbolp exp)           Returns "t" if the result of evaluating "exp" is a 
                        string and returns "nil" otherwise.

(truncate exp)          Returns the result of evaluating "exp" and truncating
                        it to an integer.

(while exp forms...)    Evaluates "exp" and if it is true
                        evaluates "forms".  Result is the value of the last
                        evaluated form.

(+ exps...)             Returns the result of adding together the result 
                        of evalating each "exp".  The values of "exps" should
                        be integers or floats.  If any "exp" is a float, the
                        result is a float, otherwise an integer.

(- exp1 exps...)        Returns the result of subtracting the result 
                        of evalating each "exp(i)" from "exp1".  If there
                        is only one "exp" then the result is equivalent to
                        "(- 0 exp1)".  The values of  "exps" should
                        be integers or floats.  If any "exp" is a float, the
                        result is a float, otherwise an integer.

( * exps...)            Returns the result of multiplying together the result 
                        of evalating each "exp".  The values of "exps" should
                        be integers or floats.  If any "exp" is a float, the
                        result is a float, otherwise an integer.

(- exp1 exps...)        Returns the result of succesively dividing the result 
                        of evalating each "exp(i)" into "exp1".  If there
                        is only one "exp" then the result is equivalent to
                        "(/ 1 exp1)".  The values of  "exps" should
                        be integers or floats.  If any "exp" is a float, the
                        result is a float, otherwise an integer.
*)


TYPE
  Sexp = REFANY;
    (* it is really on of the following: *)
    Symbol  = Atom.T;
    List    = RefList.T;
    Integer = REF INTEGER;
    Float   = REF REAL;
    String  = Text.T;
    Reader  = Rd.T;
    Writer  = Wr.T;

  Builtin = OBJECT
              name: Text.T;
              minArgs := 0;
              maxArgs := LAST (INTEGER);
            METHODS
              apply (interp: T; args: List): Sexp RAISES {Error}; END;
            
  T <: PublicT;
  PublicT = OBJECT METHODS
         new (): T RAISES {Error};
         init(): T;
         (* initializess the interpreter to the initial state.  
            can also be called to reset an existing interpreter. *)
         error (msg: Text.T := ""): Sexp RAISES {Error};

         load (name: Text.T): Sexp RAISES {Error};

         defineVar (name: Text.T; val: Sexp);
         defineFun (cl: Builtin);

         checkSymbol (e: Sexp): Symbol RAISES {Error};
         checkList (e: Sexp): List RAISES {Error};
         checkInt (e: Sexp): INTEGER RAISES {Error};
         checkFloat (e: Sexp): REAL RAISES {Error};
         checkString (e: Sexp): String RAISES {Error};

         eval (e: Sexp): Sexp RAISES {Error};
           evalSymbol (e: Sexp): Symbol RAISES {Error};
           evalList (e: Sexp): List RAISES {Error};
           evalInt (e: Sexp): INTEGER RAISES {Error};
           evalFloat (e: Sexp): REAL RAISES {Error};
           evalString (e: Sexp): String RAISES {Error};

         sEval (t: Text.T): Text.T RAISES {Error};
         varEval (name: Text.T): Sexp RAISES {Error};  END;

PROCEDURE Read (rd: Reader): Sexp 
  RAISES {Sx.ReadError, Rd.EndOfFile};
PROCEDURE Write (wr: Writer; s: Sexp);

END SLisp.
