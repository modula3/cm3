(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep 23 08:57:35 PDT 1993 by mhb      *)
(*      modified on Fri Jul 31 15:26:08 PDT 1992 by sclafani *)

INTERFACE DataView;

(*
    A DataView.T provides simple display of variables.
    It is intended for - but not limited to - use with Zeus and the 
    Modula-3 compiler's TRACE pragma.

    The compiler will generate calls to user-specified trace routines
    with the initial value of a <*TRACE*>d variable and at every point
    where it may be modified.  From the compiler release notice:

    The modified pieces of M3 grammar:

        VariableDecl = IdList (":" Type & ":=" Expr) V_Trace.
        Formal       = [Mode] IdList (":" Type & ":=" ConstExpr) V_Trace.
        ForSt        = FOR Id V_Trace ":=" Expr TO Expr [BY Expr] DO S END.
        Handler      = QualId {"," QualId} ["(" Id V_Trace ")"] "=>" S.
        TCase        = Type {"," Type} ["(" Id V_Trace ")"] "=>" S.
        Binding      = Id V_Trace "=" Expr.
        V_Trace      = [ "<*" TRACE  Expr "*>" ].

    The 'Expr' specified in a V_Trace must evaluate to a procedure
    of two arguments.  The first argument is the name of the traced
    variable, a TEXT.  The second argument is the traced variable.
    Note that any of the formal passing modes may be used with the
    second argument.

    For example:

          MODULE M;
          VAR x: Foo <*TRACE MyTrace.FooChanged*>;

    will cause

          MyTrace.FooChanged ("M.x", x)

    to be generated after each statement that modifies x.
    Variable aliasing is not tracked, so

          WITH  alias = x DO  INC(alias) END

    will not generate any tracing.
    
    Here is an example of a traced Zeus algorithm procedure:    

        PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
          VAR
            B: INTEGER   <* TRACE alg.varView.setIntegerL *>;
            N: INTEGER   <* TRACE alg.varView.setIntegerL *>;
            wt: REAL     <* TRACE alg.varView.setReal *>;
            bin: INTEGER <* TRACE alg.varView.setInteger *>;
            bins: Bins;
          BEGIN
            LOCK VBT.mu DO
              N := FormsVBT.GetInteger(alg.data, "N");
              B := FormsVBT.GetInteger(alg.data, "B");
            END;
            BinpackIE.Setup(alg, B, N);
            bins := NEW(Bins, B);
            FOR b := 0 TO B-1 DO bins[b] := 0.0 END;
            FOR i := 1 TO N DO 
              wt := Random.Real();
              BinpackIE.NewWeight (alg, wt);
              bin := 0; 
              WHILE (bin < B) AND (bins[bin] + wt > 1.0) DO INC(bin) END;
              IF bin = B THEN 
                BinpackIE.Ignore(alg);
              ELSE  
                bins[bin] := bins[bin] + wt;
                BinpackIE.Pack(alg, bin, bins[bin]) 
              END
            END
          END Run;

    The fv expression that is made into a DataView.T must contain
    components with names corresponding to traced variable names.
    Here is one possible form for the example above:

          (Rim
            (Pen 10)
            (HBox
              (VBox
                (Text RightAlign "# of bins: ")
                (Text RightAlign "# of items: ")
                (Text RightAlign "current weight: ")
                (Text RightAlign "probing bin #: "))
              (VBox
                (TextArea ReadOnly %B)
                (TextArea ReadOnly %N)
                (TextArea ReadOnly %wt)
                (TextArea ReadOnly %bin))))
    
    If using a DataView within Zeus, all you need to do is to 
    set the "varRsrc" field on the Algorithm.T to name a resource 
    containing a valid FormsVBT s-expression.

*)

IMPORT FormsVBT;

(* Basic types.  If you need something that isn't here, 
   don't hesitate to add it. *)

TYPE
  Integer = INTEGER;
  Boolean = BOOLEAN;
  Char = CHAR;
  Real = REAL;
  LongReal = LONGREAL;
  Text = TEXT;

  IntegerArray = ARRAY OF Integer;
  CharArray = ARRAY OF Char;
  BooleanArray = ARRAY OF Boolean;
  RealArray = ARRAY OF Real;
  LongRealArray = ARRAY OF LongReal;
  TextArray = ARRAY OF Text;

  IntegerArray2 = ARRAY OF IntegerArray;
  CharArray2 = ARRAY OF CharArray;
  BooleanArray2 = ARRAY OF BooleanArray;
  RealArray2 = ARRAY OF RealArray;
  LongRealArray2 = ARRAY OF LongRealArray;
  TextArray2 = ARRAY OF TextArray;

  IntegerPair = RECORD a, b: INTEGER END;
  IntegerPairArray = ARRAY OF IntegerPair;

  IntegerTriple = RECORD a, b, c: INTEGER END;
  IntegerTripleArray = ARRAY OF IntegerTriple;

  IntegerList = REF RECORD
                      i   : INTEGER;
                      next: IntegerList
                    END;
  IntegerListArray = ARRAY OF IntegerList;

  RealList = REF RECORD
                   r   : REAL;
                   next: RealList
                 END;
  RealListArray = ARRAY OF RealList;

  IntegerPairList = REF RECORD
                          a, b: INTEGER;
                          next: IntegerPairList
                        END;
  IntegerPairListArray = ARRAY OF IntegerPairList;

  IntegerTree = REF RECORD
                      i   : INTEGER;
                      l, r: IntegerTree
                    END;

TYPE
  T <: Public;
  Public = FormsVBT.T OBJECT
    METHODS

      (* TRACE methods with LL < VBT.mu *)
      setInteger  (var: TEXT; val: Integer);
      setBoolean  (var: TEXT; val: Boolean);
      setChar     (var: TEXT; val: Char);
      setReal     (var: TEXT; val: Real);
      setLongReal (var: TEXT; val: LongReal);
      setText     (var: TEXT; val: Text);

      setIntegerArray  (var: TEXT; READONLY val: IntegerArray);
      setBooleanArray  (var: TEXT; READONLY val: BooleanArray);
      setCharArray     (var: TEXT; READONLY val: CharArray);
      setRealArray     (var: TEXT; READONLY val: RealArray);
      setLongRealArray (var: TEXT; READONLY val: LongRealArray);
      setTextArray     (var: TEXT; READONLY val: TextArray);

      setIntegerArray2  (var: TEXT; READONLY val: IntegerArray2);
      setBooleanArray2  (var: TEXT; READONLY val: BooleanArray2);
      setCharArray2     (var: TEXT; READONLY val: CharArray2);
      setRealArray2     (var: TEXT; READONLY val: RealArray2);
      setLongRealArray2 (var: TEXT; READONLY val: LongRealArray2);
      setTextArray2     (var: TEXT; READONLY val: TextArray2);

      setIntegerPair      (var: TEXT; READONLY val: IntegerPair);
      setIntegerPairArray (var: TEXT; READONLY val: IntegerPairArray);

      setIntegerTriple      (var: TEXT; READONLY val: IntegerTriple);
      setIntegerTripleArray (var: TEXT; READONLY val: IntegerTripleArray);

      setIntegerList      (var: TEXT; val: IntegerList);
      setIntegerListArray (var: TEXT; READONLY val: IntegerListArray);

      setRealList      (var: TEXT; val: RealList);
      setRealListArray (var: TEXT; READONLY val: RealListArray);

      setIntegerPairList      (var: TEXT; val: IntegerPairList);
      setIntegerPairListArray (var: TEXT; READONLY val: IntegerPairListArray);

      setIntegerTree (var: TEXT; val: IntegerTree);


      (* TRACE methods with LL = VBT.mu *)
      setIntegerL  (var: TEXT; val: Integer);
      setBooleanL  (var: TEXT; val: Boolean);
      setCharL     (var: TEXT; val: Char);
      setRealL     (var: TEXT; val: Real);
      setLongRealL (var: TEXT; val: LongReal);
      setTextL     (var: TEXT; val: Text);

      setIntegerArrayL  (var: TEXT; READONLY val: IntegerArray);
      setBooleanArrayL  (var: TEXT; READONLY val: BooleanArray);
      setCharArrayL     (var: TEXT; READONLY val: CharArray);
      setRealArrayL     (var: TEXT; READONLY val: RealArray);
      setLongRealArrayL (var: TEXT; READONLY val: LongRealArray);
      setTextArrayL     (var: TEXT; READONLY val: TextArray);

      setIntegerArray2L  (var: TEXT; READONLY val: IntegerArray2);
      setBooleanArray2L  (var: TEXT; READONLY val: BooleanArray2);
      setCharArray2L     (var: TEXT; READONLY val: CharArray2);
      setRealArray2L     (var: TEXT; READONLY val: RealArray2);
      setLongRealArray2L (var: TEXT; READONLY val: LongRealArray2);
      setTextArray2L     (var: TEXT; READONLY val: TextArray2);

      setIntegerPairL      (var: TEXT; READONLY val: IntegerPair);
      setIntegerPairArrayL (var: TEXT; READONLY val: IntegerPairArray);

      setIntegerTripleL      (var: TEXT; READONLY val: IntegerTriple);
      setIntegerTripleArrayL (var: TEXT; READONLY val: IntegerTripleArray);

      setIntegerListL      (var: TEXT; val: IntegerList);
      setIntegerListArrayL (var: TEXT; READONLY val: IntegerListArray);

      setRealListL      (var: TEXT; val: RealList);
      setRealListArrayL (var: TEXT; READONLY val: RealListArray);

      setIntegerPairListL      (var: TEXT; val: IntegerPairList);
      setIntegerPairListArrayL (var: TEXT; READONLY val: IntegerPairListArray);

      setIntegerTreeL (var: TEXT; val: IntegerTree);
    END;

END DataView.
