MODULE Main;
IMPORT CompiledTest, Scheme, SchemeM3, SchemeNavigatorEnvironment;
IMPORT Pathname, Wr, Stdio, SchemeUtils, Text, Fmt, Time;
IMPORT Thread;
<*FATAL Thread.Alerted, Wr.Failure*>

VAR pass, fail, total : INTEGER := 0;

PROCEDURE T(scm: Scheme.T; expr, expected: TEXT) RAISES {Scheme.E} =
  VAR got := SchemeUtils.StringifyT(scm.loadEvalText(expr));
  BEGIN
    INC(total);
    IF Text.Equal(got, expected) THEN
      INC(pass);
      Wr.PutText(Stdio.stdout, "  PASS: " & expr & "\n")
    ELSE
      INC(fail);
      Wr.PutText(Stdio.stdout,
        "  FAIL: " & expr & " => " & got & " (expected " & expected & ")\n")
    END;
    Wr.Flush(Stdio.stdout)
  END T;

PROCEDURE Section(name: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, "\n=== " & name & " ===\n");
    Wr.Flush(Stdio.stdout)
  END Section;

PROCEDURE PrintRow(label: TEXT; compiled, interpreted: LONGREAL) =
  VAR speedup: TEXT;
  BEGIN
    IF compiled > 0.001D0 THEN
      speedup := Fmt.LongReal(interpreted / compiled, Fmt.Style.Fix, 1) & "x"
    ELSE
      speedup := ">1000x"
    END;
    Wr.PutText(Stdio.stdout,
      Fmt.Pad(label, 28)
      & Fmt.Pad(Fmt.LongReal(compiled, Fmt.Style.Fix, 2) & " ms", 14, align := Fmt.Align.Right)
      & Fmt.Pad(Fmt.LongReal(interpreted, Fmt.Style.Fix, 2) & " ms", 17, align := Fmt.Align.Right)
      & "   " & speedup & "\n");
    Wr.Flush(Stdio.stdout)
  END PrintRow;

BEGIN
  TRY
    WITH arr = ARRAY OF Pathname.T {"require"},
         scm = NEW(SchemeM3.T) DO
      EVAL scm.init(arr, globalEnv := NEW(SchemeNavigatorEnvironment.T).initEmpty());
      CompiledTest.Install(scm);
      Wr.PutText(Stdio.stdout, "CompiledTest.Install succeeded.\n");
      Wr.Flush(Stdio.stdout);

      Section("Arithmetic");
      T(scm, "(factorial 0)", "1");
      T(scm, "(factorial 1)", "1");
      T(scm, "(factorial 5)", "120");
      T(scm, "(factorial 10)", "3628800");
      T(scm, "(factorial 12)", "479001600");
      T(scm, "(fibonacci 0)", "0");
      T(scm, "(fibonacci 1)", "1");
      T(scm, "(fibonacci 2)", "1");
      T(scm, "(fibonacci 10)", "55");
      T(scm, "(fibonacci 15)", "610");
      T(scm, "(square 0)", "0");
      T(scm, "(square 5)", "25");
      T(scm, "(square -3)", "9");
      T(scm, "(square 12)", "144");
      T(scm, "(cube 0)", "0");
      T(scm, "(cube 2)", "8");
      T(scm, "(cube 3)", "27");
      T(scm, "(cube -2)", "-8");
      T(scm, "(abs-val 5)", "5");
      T(scm, "(abs-val -5)", "5");
      T(scm, "(abs-val 0)", "0");
      T(scm, "(max-of-two 3 7)", "7");
      T(scm, "(max-of-two 10 2)", "10");
      T(scm, "(max-of-two 5 5)", "5");
      T(scm, "(min-of-two 3 7)", "3");
      T(scm, "(min-of-two 10 2)", "2");
      T(scm, "(min-of-two 5 5)", "5");
      T(scm, "(power 2 0)", "1");
      T(scm, "(power 2 10)", "1024");
      T(scm, "(power 3 5)", "243");
      T(scm, "(power 10 3)", "1000");
      T(scm, "(sum-range 0 0)", "0");
      T(scm, "(sum-range 10 0)", "55");
      T(scm, "(sum-range 100 0)", "5050");
      T(scm, "(even? 0)", "#t");
      T(scm, "(even? 2)", "#t");
      T(scm, "(even? 7)", "#f");
      T(scm, "(even? 100)", "#t");
      T(scm, "(odd? 1)", "#t");
      T(scm, "(odd? 3)", "#t");
      T(scm, "(odd? 4)", "#f");
      T(scm, "(odd? 0)", "#f");
      T(scm, "(gcd 12 8)", "4");
      T(scm, "(gcd 100 75)", "25");
      T(scm, "(gcd 17 13)", "1");
      T(scm, "(gcd 0 5)", "5");
      T(scm, "(negate 5)", "-5");
      T(scm, "(negate -3)", "3");
      T(scm, "(negate 0)", "0");
      T(scm, "(double 7)", "14");
      T(scm, "(double 0)", "0");
      T(scm, "(double -3)", "-6");
      T(scm, "(triple 4)", "12");
      T(scm, "(triple 0)", "0");
      T(scm, "(add3 1 2 3)", "6");
      T(scm, "(add3 10 20 30)", "60");
      T(scm, "(add4 1 2 3 4)", "10");
      T(scm, "(add4 10 20 30 40)", "100");

      Section("List Operations");
      T(scm, "(my-length '())", "0");
      T(scm, "(my-length '(a))", "1");
      T(scm, "(my-length '(a b c))", "3");
      T(scm, "(my-length '(1 2 3 4 5))", "5");
      T(scm, "(sum-list '())", "0");
      T(scm, "(sum-list '(1 2 3))", "6");
      T(scm, "(sum-list '(10 20 30 40))", "100");
      T(scm, "(product-list '())", "1");
      T(scm, "(product-list '(1 2 3 4 5))", "120");
      T(scm, "(product-list '(2 3 7))", "42");
      T(scm, "(my-last '(1))", "1");
      T(scm, "(my-last '(1 2 3))", "3");
      T(scm, "(my-last '(a b c d))", "d");
      T(scm, "(my-nth '(a b c d) 0)", "a");
      T(scm, "(my-nth '(a b c d) 2)", "c");
      T(scm, "(my-nth '(a b c d) 3)", "d");
      T(scm, "(my-append '() '())", "()");
      T(scm, "(my-append '(1 2) '(3 4))", "(1 2 3 4)");
      T(scm, "(my-append '() '(x y))", "(x y)");
      T(scm, "(my-append '(a) '())", "(a)");
      T(scm, "(my-reverse-acc '() '())", "()");
      T(scm, "(my-reverse-acc '(1 2 3) '())", "(3 2 1)");
      T(scm, "(my-reverse-acc '(a) '())", "(a)");
      T(scm, "(my-reverse-acc '(1 2 3 4 5) '())", "(5 4 3 2 1)");
      T(scm, "(my-map square '())", "()");
      T(scm, "(my-map square '(1 2 3 4))", "(1 4 9 16)");
      T(scm, "(my-map double '(5 10 15))", "(10 20 30)");
      T(scm, "(my-filter even? '())", "()");
      T(scm, "(my-filter even? '(1 2 3 4 5 6))", "(2 4 6)");
      T(scm, "(my-filter odd? '(1 2 3 4 5))", "(1 3 5)");
      T(scm, "(my-member 3 '(1 2 3 4))", "#t");
      T(scm, "(my-member 5 '(1 2 3 4))", "#f");
      T(scm, "(my-member 1 '(1))", "#t");
      T(scm, "(my-member 1 '())", "#f");
      T(scm, "(count-if even? '(1 2 3 4 5 6))", "3");
      T(scm, "(count-if odd? '(1 2 3 4 5))", "3");
      T(scm, "(count-if even? '())", "0");
      T(scm, "(take '(1 2 3 4 5) 0)", "()");
      T(scm, "(take '(1 2 3 4 5) 3)", "(1 2 3)");
      T(scm, "(take '(a b) 2)", "(a b)");
      T(scm, "(drop '(1 2 3 4 5) 0)", "(1 2 3 4 5)");
      T(scm, "(drop '(1 2 3 4 5) 3)", "(4 5)");
      T(scm, "(drop '(a b) 2)", "()");
      T(scm, "(zip '(1 2 3) '(a b c))", "((1 . a) (2 . b) (3 . c))");
      T(scm, "(zip '() '())", "()");
      T(scm, "(zip '(1) '(a))", "((1 . a))");
      T(scm, "(flatten '())", "()");
      T(scm, "(flatten '(1 2 3))", "(1 2 3)");
      T(scm, "(flatten '((1 2) (3 4)))", "(1 2 3 4)");
      T(scm, "(flatten '((1 (2 3)) (4)))", "(1 2 3 4)");
      T(scm, "(list-ref-safe '(a b c) 0 'none)", "a");
      T(scm, "(list-ref-safe '(a b c) 2 'none)", "c");
      T(scm, "(list-ref-safe '(a b c) 5 'none)", "none");
      T(scm, "(list-ref-safe '() 0 'none)", "none");
      T(scm, "(iota-acc 0 '())", "()");
      T(scm, "(iota-acc 5 '())", "(1 2 3 4 5)");
      T(scm, "(iota-acc 3 '())", "(1 2 3)");
      T(scm, "(repeat-val 'x 0)", "()");
      T(scm, "(repeat-val 'x 3)", "(x x x)");
      T(scm, "(repeat-val 0 4)", "(0 0 0 0)");

      Section("Cond");
      T(scm, "(sign -10)", "-1");
      T(scm, "(sign 0)", "0");
      T(scm, "(sign 42)", "1");
      T(scm, "(classify -5)", "negative");
      T(scm, "(classify 0)", "zero");
      T(scm, "(classify 7)", "small");
      T(scm, "(classify 50)", "medium");
      T(scm, "(classify 200)", "large");
      T(scm, "(fizzbuzz 1)", "1");
      T(scm, "(fizzbuzz 3)", "fizz");
      T(scm, "(fizzbuzz 5)", "buzz");
      T(scm, "(fizzbuzz 15)", "fizzbuzz");
      T(scm, "(fizzbuzz 30)", "fizzbuzz");
      T(scm, "(fizzbuzz 7)", "7");
      T(scm, "(letter-grade 95)", "A");
      T(scm, "(letter-grade 85)", "B");
      T(scm, "(letter-grade 75)", "C");
      T(scm, "(letter-grade 65)", "D");
      T(scm, "(letter-grade 55)", "F");

      Section("Begin");
      T(scm, "(begin-test 10)", "13");
      T(scm, "(begin-test 0)", "3");
      T(scm, "(begin-with-side-effect 5)", "10");
      T(scm, "(begin-with-side-effect 0)", "0");

      Section("Let / Let*");
      T(scm, "(hypotenuse 3 4)", "25");
      T(scm, "(hypotenuse 0 5)", "25");
      T(scm, "(hypotenuse 5 12)", "169");
      T(scm, "(circle-area 1)", "3");
      T(scm, "(circle-area 10)", "314");
      T(scm, "(let-nested 5)", "18");
      T(scm, "(let-nested 0)", "3");
      T(scm, "(let-nested 10)", "33");
      T(scm, "(let-star-chain 5)", "18");
      T(scm, "(let-star-chain 0)", "3");
      T(scm, "(let-star-chain 10)", "33");
      T(scm, "(let-star-deps 5 3)", "16");
      T(scm, "(let-star-deps 10 10)", "0");
      T(scm, "(let-star-deps 7 2)", "45");
      T(scm, "(swap-via-let 1 2)", "2001");
      T(scm, "(swap-via-let 5 9)", "9005");
      T(scm, "(multi-let 10)", "36");
      T(scm, "(multi-let 0)", "6");

      Section("And / Or / Not");
      T(scm, "(both-positive? 1 1)", "#t");
      T(scm, "(both-positive? -1 1)", "#f");
      T(scm, "(both-positive? 1 -1)", "#f");
      T(scm, "(both-positive? 0 1)", "#f");
      T(scm, "(either-zero? 0 5)", "#t");
      T(scm, "(either-zero? 5 0)", "#t");
      T(scm, "(either-zero? 1 2)", "#f");
      T(scm, "(either-zero? 0 0)", "#t");
      T(scm, "(in-range? 5 1 10)", "#t");
      T(scm, "(in-range? 0 1 10)", "#f");
      T(scm, "(in-range? 11 1 10)", "#f");
      T(scm, "(in-range? 1 1 10)", "#t");
      T(scm, "(in-range? 10 1 10)", "#t");
      T(scm, "(out-of-range? 0 1 10)", "#t");
      T(scm, "(out-of-range? 11 1 10)", "#t");
      T(scm, "(out-of-range? 5 1 10)", "#f");
      T(scm, "(three-and 1 2 3)", "3");
      T(scm, "(three-and 1 #f 3)", "#f");
      T(scm, "(three-and #f 2 3)", "#f");
      T(scm, "(three-or #f #f 3)", "3");
      T(scm, "(three-or #f 2 3)", "2");
      T(scm, "(three-or 1 2 3)", "1");
      T(scm, "(three-or #f #f #f)", "#f");
      T(scm, "(not-zero? 5)", "#t");
      T(scm, "(not-zero? 0)", "#f");
      T(scm, "(not-zero? -1)", "#t");
      T(scm, "(xor-bool #t #f)", "#t");
      T(scm, "(xor-bool #f #t)", "#t");
      T(scm, "(xor-bool #t #t)", "#f");
      T(scm, "(xor-bool #f #f)", "#f");

      Section("Set! (Mutation)");
      T(scm, "(set-param 5)", "15");
      T(scm, "(set-param 0)", "10");
      T(scm, "(set-param -10)", "0");
      T(scm, "(set-in-let 5)", "105");
      T(scm, "(set-in-let 0)", "100");
      T(scm, "(set-in-let 42)", "142");
      T(scm, "(set-accumulate 5)", "11");
      T(scm, "(set-accumulate 10)", "21");
      T(scm, "(set-accumulate 0)", "1");

      Section("Self-Tail-Call Optimization");
      T(scm, "(loop-sum 0 0)", "0");
      T(scm, "(loop-sum 10 0)", "55");
      T(scm, "(loop-sum 100 0)", "5050");
      T(scm, "(loop-sum 1000 0)", "500500");
      T(scm, "(count-down 0)", "0");
      T(scm, "(count-down 100)", "0");
      T(scm, "(tail-factorial 0 1)", "1");
      T(scm, "(tail-factorial 5 1)", "120");
      T(scm, "(tail-factorial 10 1)", "3628800");
      T(scm, "(tail-fib-iter 0 0 1)", "0");
      T(scm, "(tail-fib-iter 1 0 1)", "1");
      T(scm, "(tail-fib-iter 10 0 1)", "55");
      T(scm, "(tail-fib-iter 20 0 1)", "6765");
      T(scm, "(tail-length '() 0)", "0");
      T(scm, "(tail-length '(a b c) 0)", "3");
      T(scm, "(tail-sum-list '() 0)", "0");
      T(scm, "(tail-sum-list '(1 2 3) 0)", "6");
      T(scm, "(tail-last '(1))", "1");
      T(scm, "(tail-last '(1 2 3))", "3");
      T(scm, "(tail-min '(3) 3)", "3");
      T(scm, "(tail-min '(5 2 8 1 9) 5)", "1");
      T(scm, "(tail-max '(3) 3)", "3");
      T(scm, "(tail-max '(5 2 8 1 9) 5)", "9");
      T(scm, "(repeat-string 0 \"\")", "");
      T(scm, "(repeat-string 3 \"\")", "xxx");

      Section("Constants");
      T(scm, "(return-true)", "#t");
      T(scm, "(return-false)", "#f");
      T(scm, "(return-zero)", "0");
      T(scm, "(return-one)", "1");
      T(scm, "(return-string)", "hello");
      T(scm, "(return-empty-list)", "()");
      T(scm, "(return-symbol)", "foo");
      T(scm, "(return-quoted-list)", "(1 2 3)");
      T(scm, "(return-char)", "A");
      T(scm, "(identity 42)", "42");
      T(scm, "(identity 'hello)", "hello");
      T(scm, "(identity #t)", "#t");
      T(scm, "(constant-42)", "42");
      T(scm, "(return-pi-approx)", "3141593");
      T(scm, "(return-negative)", "-7");

      Section("Recursive Algorithms");
      T(scm, "(ackermann 0 0)", "1");
      T(scm, "(ackermann 0 5)", "6");
      T(scm, "(ackermann 1 0)", "2");
      T(scm, "(ackermann 1 5)", "7");
      T(scm, "(ackermann 2 3)", "9");
      T(scm, "(ackermann 3 2)", "29");
      T(scm, "(ackermann 3 3)", "61");
      T(scm, "(hanoi-moves 0)", "0");
      T(scm, "(hanoi-moves 1)", "1");
      T(scm, "(hanoi-moves 3)", "7");
      T(scm, "(hanoi-moves 5)", "31");
      T(scm, "(hanoi-moves 10)", "1023");
      T(scm, "(depth '())", "0");
      T(scm, "(depth '(1 2 3))", "1");
      T(scm, "(depth '((1) 2 3))", "2");
      T(scm, "(depth '(((1)) 2))", "3");
      T(scm, "(tree-sum '(1 2 3))", "6");
      T(scm, "(tree-sum '((1 2) (3 4)))", "10");
      T(scm, "(tree-sum '(((1) 2) 3))", "6");
      T(scm, "(tree-count '())", "0");
      T(scm, "(tree-count '(1 2 3))", "3");
      T(scm, "(tree-count '((1 2) (3)))", "3");
      T(scm, "(collatz-steps 1 0)", "0");
      T(scm, "(collatz-steps 2 0)", "1");
      T(scm, "(collatz-steps 6 0)", "8");
      T(scm, "(collatz-steps 27 0)", "111");
      T(scm, "(digit-sum 0)", "0");
      T(scm, "(digit-sum 5)", "5");
      T(scm, "(digit-sum 123)", "6");
      T(scm, "(digit-sum 9999)", "36");
      T(scm, "(num-digits 0)", "1");
      T(scm, "(num-digits 9)", "1");
      T(scm, "(num-digits 10)", "2");
      T(scm, "(num-digits 999)", "3");
      T(scm, "(num-digits 10000)", "5");
      T(scm, "(is-palindrome-helper '(1 2 1) '(1 2 1))", "#t");
      T(scm, "(is-palindrome-helper '(1 2 3) '(3 2 1))", "#f");
      T(scm, "(is-palindrome-helper '() '())", "#t");
      T(scm, "(list-equal? '(1 2 3) '(1 2 3))", "#t");
      T(scm, "(list-equal? '(1 2 3) '(1 2 4))", "#f");
      T(scm, "(list-equal? '(1 2) '(1 2 3))", "#f");
      T(scm, "(list-equal? '() '())", "#t");

      Section("Higher-Order (free variables)");
      T(scm, "(apply-twice square 2)", "16");
      T(scm, "(apply-twice double 3)", "12");
      T(scm, "(compose-apply square double 3)", "36");
      T(scm, "(compose-apply double square 3)", "18");

      Section("Mixed Constructs");
      T(scm, "(complex-cond-let -3)", "9");
      T(scm, "(complex-cond-let 0)", "0");
      T(scm, "(complex-cond-let 5)", "36");
      T(scm, "(nested-if-let 10 3)", "14");
      T(scm, "(nested-if-let 3 10)", "49");
      T(scm, "(nested-if-let 5 5)", "0");
      T(scm, "(and-or-mix 1 1 1)", "1");
      T(scm, "(and-or-mix 1 -1 -1)", "0");
      T(scm, "(and-or-mix -1 1 1)", "0");
      T(scm, "(and-or-mix 1 -1 1)", "1");
      T(scm, "(multi-begin-let 10)", "36");
      T(scm, "(multi-begin-let 0)", "6");
      T(scm, "(cond-with-begin -5)", "5");
      T(scm, "(cond-with-begin 0)", "0");
      T(scm, "(cond-with-begin 10)", "10");
      T(scm, "(if-chain 200)", "big");
      T(scm, "(if-chain 75)", "medium");
      T(scm, "(if-chain 25)", "small");
      T(scm, "(if-chain 5)", "tiny");

      Section("Edge Cases");
      T(scm, "(zero-args)", "42");
      T(scm, "(single-arg 99)", "100");
      T(scm, "(five-args 1 2 3 4 5)", "15");
      T(scm, "(five-args 10 20 30 40 50)", "150");
      T(scm, "(deeply-nested 5)", "deep");
      T(scm, "(deeply-nested 4)", "4");
      T(scm, "(deeply-nested 3)", "3");
      T(scm, "(deeply-nested 0)", "0");
      T(scm, "(many-lets 10)", "20");
      T(scm, "(many-lets 0)", "10");
      T(scm, "(let-shadow 5)", "30");
      T(scm, "(let-shadow 0)", "20");
      T(scm, "(let-shadow 10)", "40");
      T(scm, "(cond-many 1)", "one");
      T(scm, "(cond-many 3)", "three");
      T(scm, "(cond-many 5)", "other");
      T(scm, "(boolean-identity #t)", "#t");
      T(scm, "(boolean-identity #f)", "#f");

      Section("Cond =>");
      T(scm, "(cond-arrow-test 'a)", "1");
      T(scm, "(cond-arrow-test 'b)", "2");
      T(scm, "(cond-arrow-test 'c)", "3");
      T(scm, "(cond-arrow-test 'd)", "#f");

      Section("Letrec");
      T(scm, "(letrec-simple 5)", "13");
      T(scm, "(letrec-simple 0)", "3");
      T(scm, "(letrec-simple 10)", "23");
      T(scm, "(letrec-even-odd 0)", "2");
      T(scm, "(letrec-even-odd 5)", "1");
      T(scm, "(letrec-even-odd -3)", "-1");

      Section("Case");
      T(scm, "(day-type 'monday)", "weekday");
      T(scm, "(day-type 'friday)", "weekday");
      T(scm, "(day-type 'saturday)", "weekend");
      T(scm, "(day-type 'sunday)", "weekend");
      T(scm, "(day-type 'holiday)", "unknown");
      T(scm, "(case-number 1)", "one");
      T(scm, "(case-number 2)", "two-or-three");
      T(scm, "(case-number 3)", "two-or-three");
      T(scm, "(case-number 5)", "four-to-six");
      T(scm, "(case-number 7)", "other");
      T(scm, "(case-symbol 'red)", "1");
      T(scm, "(case-symbol 'green)", "2");
      T(scm, "(case-symbol 'blue)", "3");
      T(scm, "(case-symbol 'yellow)", "0");

      Section("Do Loops");
      T(scm, "(do-sum-to 0)", "0");
      T(scm, "(do-sum-to 10)", "55");
      T(scm, "(do-sum-to 100)", "5050");
      T(scm, "(do-countdown 5)", "0");
      T(scm, "(do-countdown 0)", "0");
      T(scm, "(do-list-reverse '())", "()");
      T(scm, "(do-list-reverse '(1 2 3))", "(3 2 1)");
      T(scm, "(do-list-reverse '(a b c d))", "(d c b a)");
      T(scm, "(do-factorial 0)", "1");
      T(scm, "(do-factorial 5)", "120");
      T(scm, "(do-factorial 10)", "3628800");

      Section("Rest Parameters");
      T(scm, "(rest-sum)", "0");
      T(scm, "(rest-sum 1 2 3)", "6");
      T(scm, "(rest-sum 10 20 30 40)", "100");
      T(scm, "(rest-first-or-default 99)", "99");
      T(scm, "(rest-first-or-default 99 42)", "42");
      T(scm, "(rest-first-or-default 99 1 2 3)", "1");
      T(scm, "(rest-count)", "0");
      T(scm, "(rest-count 'a)", "1");
      T(scm, "(rest-count 'a 'b 'c)", "3");

      Section("Named Let");
      T(scm, "(named-let-sum 0)", "0");
      T(scm, "(named-let-sum 10)", "55");
      T(scm, "(named-let-sum 100)", "5050");
      T(scm, "(named-let-length '())", "0");
      T(scm, "(named-let-length '(a b c))", "3");
      T(scm, "(named-let-length '(1 2 3 4 5))", "5");
      T(scm, "(named-let-reverse '())", "()");
      T(scm, "(named-let-reverse '(1 2 3))", "(3 2 1)");
      T(scm, "(named-let-iota 0)", "()");
      T(scm, "(named-let-iota 5)", "(1 2 3 4 5)");
      T(scm, "(named-let-find 3 '(1 2 3 4))", "#t");
      T(scm, "(named-let-find 5 '(1 2 3 4))", "#f");
      T(scm, "(named-let-find 1 '())", "#f");
      T(scm, "(named-let-map-square '())", "()");
      T(scm, "(named-let-map-square '(1 2 3))", "(1 4 9)");
      T(scm, "(named-let-map-square '(2 5))", "(4 25)");
      T(scm, "(named-let-nontail 0)", "0");
      T(scm, "(named-let-nontail 10)", "55");
      T(scm, "(named-let-nontail 100)", "5050");

      Section("Lambda");
      T(scm, "(lambda-callback '())", "()");
      T(scm, "(lambda-callback '(1 2 3))", "(2 3 4)");
      T(scm, "(lambda-callback '(10 20 30))", "(11 21 31)");
      T(scm, "(lambda-capture-param 10 '(1 2 3))", "(11 12 13)");
      T(scm, "(lambda-capture-param 0 '(5 10))", "(5 10)");
      T(scm, "(lambda-capture-param 100 '(1))", "(101)");
      T(scm, "(lambda-capture-let '(1 2 3))", "(101 102 103)");
      T(scm, "(lambda-capture-let '())", "()");
      T(scm, "(lambda-capture-binding '(1 2 3 4))", "(1 4 9 16)");
      T(scm, "(lambda-capture-binding '())", "()");
      T(scm, "(lambda-2param '(1 2 3))", "(2 4 6)");
      T(scm, "(lambda-rest-test)", "40");
      T(scm, "(lambda-in-cond #t '(1 2 3))", "(2 3 4)");
      T(scm, "(lambda-in-cond #f '(1 2 3))", "(2 4 6)");
      T(scm, "(lambda-no-capture)", "42");
      T(scm, "(lambda-let-bound 5)", "40");
      T(scm, "(lambda-let-bound 0)", "30");
      T(scm, "(internal-define-simple 5)", "15");
      T(scm, "(internal-define-simple 0)", "10");
      T(scm, "(lambda-map-sum 0)", "0");
      T(scm, "(lambda-map-sum 3)", "9");

      Section("Mutual Recursion");
      T(scm, "(mut-f 0)", "1");
      T(scm, "(mut-f 1)", "1");
      T(scm, "(mut-f 5)", "8");
      T(scm, "(mut-f 10)", "89");
      T(scm, "(mut-g 0)", "1");
      T(scm, "(mut-g 1)", "1");
      T(scm, "(mut-g 5)", "8");
      T(scm, "(mut-g 10)", "89");

      (* ======== Redefinition Semantics ======== *)
      Section("Redefinition Semantics");

      (* Baseline: compiled chain redef-outer -> redef-middle -> redef-inner *)
      (* redef-outer(5) = (redef-middle(5)) * 3 = (redef-inner(5) + 1) * 3
                        = (50 + 1) * 3 = 153 *)
      T(scm, "(redef-inner 5)", "50");
      T(scm, "(redef-middle 5)", "51");
      T(scm, "(redef-outer 5)", "153");

      (* Redefine the innermost function to multiply by 100 *)
      EVAL scm.loadEvalText(
        "(define (redef-inner x) (* x 100))");

      (* Compiled redef-middle and redef-outer should see the new redef-inner
         because they look up redef-inner through the environment binding. *)
      T(scm, "(redef-inner 5)", "500");
      T(scm, "(redef-middle 5)", "501");
      T(scm, "(redef-outer 5)", "1503");

      (* Redefine the middle function *)
      EVAL scm.loadEvalText(
        "(define (redef-middle x) (- (redef-inner x)))");

      (* Compiled redef-outer should see new redef-middle *)
      T(scm, "(redef-middle 5)", "-500");
      T(scm, "(redef-outer 5)", "-1500");

      (* Restore originals for subsequent runs *)
      EVAL scm.loadEvalText(
        "(define (redef-inner x) (* x 10))");
      EVAL scm.loadEvalText(
        "(define (redef-middle x) (+ (redef-inner x) 1))");
      T(scm, "(redef-outer 5)", "153");

      (* Summary *)
      Wr.PutText(Stdio.stdout, "\n========================================\n");
      Wr.PutText(Stdio.stdout, "Total: " & Fmt.Int(total)
        & "  Pass: " & Fmt.Int(pass)
        & "  Fail: " & Fmt.Int(fail) & "\n");
      Wr.Flush(Stdio.stdout);

      (* ======== Performance Benchmarks ======== *)
      Section("Performance: Compiled vs Interpreted");
      VAR t0 : Time.T;
          cFib25, iFib25, cFib30, iFib30 : LONGREAL;
          cLoop, iLoop, cAck, iAck : LONGREAL;
          cLam, hLam, iLam : LONGREAL;
      BEGIN
        (* -- Benchmark compiled procedures (already installed) -- *)
        Wr.PutText(Stdio.stdout, "Timing compiled...\n");
        Wr.Flush(Stdio.stdout);

        t0 := Time.Now();
        EVAL scm.loadEvalText("(fibonacci 25)");
        cFib25 := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(fibonacci 30)");
        cFib30 := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(loop-sum 10000 0)");
        cLoop := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(ackermann 3 4)");
        cAck := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(lambda-map-sum 10000)");
        cLam := (Time.Now() - t0) * 1.0D3;

        (* -- Half-compiled: lambda-map-sum interpreted, my-map still compiled -- *)
        Wr.PutText(Stdio.stdout, "Timing half-compiled (pre-lambda scenario)...\n");
        Wr.Flush(Stdio.stdout);
        EVAL scm.loadEvalText(
          "(define (lambda-map-sum n) (let loop ((i n) (acc 0)) (if (= i 0) acc (loop (- i 1) (+ acc (car (my-map (lambda (x) (+ x i)) '(1))))))))");
        t0 := Time.Now();
        EVAL scm.loadEvalText("(lambda-map-sum 10000)");
        hLam := (Time.Now() - t0) * 1.0D3;

        (* -- Redefine all as interpreted -- *)
        Wr.PutText(Stdio.stdout, "Redefining as interpreted...\n");
        Wr.Flush(Stdio.stdout);

        EVAL scm.loadEvalText(
          "(define (fibonacci n) (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))");
        EVAL scm.loadEvalText(
          "(define (loop-sum n acc) (if (= n 0) acc (loop-sum (- n 1) (+ acc n))))");
        EVAL scm.loadEvalText(
          "(define (ackermann m n) (cond ((= m 0) (+ n 1)) ((= n 0) (ackermann (- m 1) 1)) (else (ackermann (- m 1) (ackermann m (- n 1))))))");
        EVAL scm.loadEvalText(
          "(define (my-map f lst) (if (null? lst) '() (cons (f (car lst)) (my-map f (cdr lst)))))");

        (* -- Benchmark interpreted procedures -- *)
        Wr.PutText(Stdio.stdout, "Timing interpreted...\n");
        Wr.Flush(Stdio.stdout);

        t0 := Time.Now();
        EVAL scm.loadEvalText("(fibonacci 25)");
        iFib25 := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(fibonacci 30)");
        iFib30 := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(loop-sum 10000 0)");
        iLoop := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(ackermann 3 4)");
        iAck := (Time.Now() - t0) * 1.0D3;

        t0 := Time.Now();
        EVAL scm.loadEvalText("(lambda-map-sum 10000)");
        iLam := (Time.Now() - t0) * 1.0D3;

        (* -- Results -- *)
        Wr.PutText(Stdio.stdout, "\n");
        Wr.PutText(Stdio.stdout,
          Fmt.Pad("Benchmark", 28)
          & Fmt.Pad("Compiled", 14, align := Fmt.Align.Right)
          & Fmt.Pad("Interpreted", 17, align := Fmt.Align.Right)
          & "   Speedup\n");
        Wr.PutText(Stdio.stdout,
          "-------------------------------------------------------------------\n");
        PrintRow("(fibonacci 25)", cFib25, iFib25);
        PrintRow("(fibonacci 30)", cFib30, iFib30);
        PrintRow("(loop-sum 10000 0)", cLoop, iLoop);
        PrintRow("(ackermann 3 4)", cAck, iAck);
        PrintRow("(lambda-map-sum 10000)", cLam, iLam);
        Wr.PutText(Stdio.stdout, "\n");
        Wr.PutText(Stdio.stdout,
          "Lambda breakdown (lambda-map-sum 10000):\n");
        Wr.PutText(Stdio.stdout,
          "  Fully compiled:   " & Fmt.LongReal(cLam, Fmt.Style.Fix, 2) & " ms\n");
        Wr.PutText(Stdio.stdout,
          "  Half (pre-lambda): " & Fmt.LongReal(hLam, Fmt.Style.Fix, 2) & " ms\n");
        Wr.PutText(Stdio.stdout,
          "  Fully interpreted: " & Fmt.LongReal(iLam, Fmt.Style.Fix, 2) & " ms\n");
        Wr.Flush(Stdio.stdout);
      END;

      (* ======== Mutual Recursion: Three-Way Benchmark ======== *)
      Section("Mutual Recursion: Direct Calls vs General Dispatch vs Interpreted");
      Wr.PutText(Stdio.stdout,
        "mut-f and mut-g are mutually recursive (cannot use LOOP optimization).\n");
      Wr.PutText(Stdio.stdout,
        "Three modes: compiled+direct, compiled+general-dispatch, interpreted.\n\n");
      Wr.Flush(Stdio.stdout);
      VAR t0m : Time.T;
          dMut25, gMut25, iMut25 : LONGREAL;
          dMut28, gMut28, iMut28 : LONGREAL;
      BEGIN
        (* -- Mode 1: Compiled with direct calls (as-is from Install) -- *)
        Wr.PutText(Stdio.stdout, "1. Compiled + direct calls...\n");
        Wr.Flush(Stdio.stdout);

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 25)");
        dMut25 := (Time.Now() - t0m) * 1.0D3;

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 28)");
        dMut28 := (Time.Now() - t0m) * 1.0D3;

        (* -- Mode 2: Compiled bodies, but defeat direct-call pointer check.
              Wrap each binding in a forwarding lambda so binding.get() != direct_pN.
              Compiled bodies still run, but every cross-call goes through
              NARROW + virtual dispatch + thin interpreted wrapper. -- *)
        Wr.PutText(Stdio.stdout, "2. Compiled + general dispatch (wrapper)...\n");
        Wr.Flush(Stdio.stdout);

        EVAL scm.loadEvalText(
          "(let ((of mut-f) (og mut-g))"
          & " (set! mut-f (lambda (n) (of n)))"
          & " (set! mut-g (lambda (n) (og n))))");

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 25)");
        gMut25 := (Time.Now() - t0m) * 1.0D3;

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 28)");
        gMut28 := (Time.Now() - t0m) * 1.0D3;

        (* -- Mode 3: Fully interpreted -- *)
        Wr.PutText(Stdio.stdout, "3. Fully interpreted...\n");
        Wr.Flush(Stdio.stdout);

        EVAL scm.loadEvalText(
          "(define (mut-f n)"
          & " (if (< n 2) 1 (+ (mut-g (- n 1)) (mut-g (- n 2)))))");
        EVAL scm.loadEvalText(
          "(define (mut-g n)"
          & " (if (< n 2) 1 (+ (mut-f (- n 1)) (mut-f (- n 2)))))");

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 25)");
        iMut25 := (Time.Now() - t0m) * 1.0D3;

        t0m := Time.Now();
        EVAL scm.loadEvalText("(mut-f 28)");
        iMut28 := (Time.Now() - t0m) * 1.0D3;

        (* -- Results table -- *)
        Wr.PutText(Stdio.stdout, "\n");
        Wr.PutText(Stdio.stdout,
          Fmt.Pad("Benchmark", 20)
          & Fmt.Pad("Direct", 12, align := Fmt.Align.Right)
          & Fmt.Pad("General", 12, align := Fmt.Align.Right)
          & Fmt.Pad("Interp", 12, align := Fmt.Align.Right)
          & Fmt.Pad("D/G", 8, align := Fmt.Align.Right)
          & Fmt.Pad("D/I", 8, align := Fmt.Align.Right)
          & "\n");
        Wr.PutText(Stdio.stdout,
          "------------------------------------------------------------------------\n");

        VAR dg25, di25, dg28, di28: TEXT;
        BEGIN
          IF dMut25 > 0.001D0 THEN
            dg25 := Fmt.LongReal(gMut25/dMut25, Fmt.Style.Fix, 1) & "x"
          ELSE dg25 := "n/a" END;
          IF dMut25 > 0.001D0 THEN
            di25 := Fmt.LongReal(iMut25/dMut25, Fmt.Style.Fix, 1) & "x"
          ELSE di25 := "n/a" END;
          IF dMut28 > 0.001D0 THEN
            dg28 := Fmt.LongReal(gMut28/dMut28, Fmt.Style.Fix, 1) & "x"
          ELSE dg28 := "n/a" END;
          IF dMut28 > 0.001D0 THEN
            di28 := Fmt.LongReal(iMut28/dMut28, Fmt.Style.Fix, 1) & "x"
          ELSE di28 := "n/a" END;

          Wr.PutText(Stdio.stdout,
            Fmt.Pad("(mut-f 25)", 20)
            & Fmt.Pad(Fmt.LongReal(dMut25, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(Fmt.LongReal(gMut25, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(Fmt.LongReal(iMut25, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(dg25, 8, align := Fmt.Align.Right)
            & Fmt.Pad(di25, 8, align := Fmt.Align.Right)
            & "\n");
          Wr.PutText(Stdio.stdout,
            Fmt.Pad("(mut-f 28)", 20)
            & Fmt.Pad(Fmt.LongReal(dMut28, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(Fmt.LongReal(gMut28, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(Fmt.LongReal(iMut28, Fmt.Style.Fix, 1) & " ms", 12, align := Fmt.Align.Right)
            & Fmt.Pad(dg28, 8, align := Fmt.Align.Right)
            & Fmt.Pad(di28, 8, align := Fmt.Align.Right)
            & "\n");
        END;

        Wr.PutText(Stdio.stdout, "\n");
        Wr.PutText(Stdio.stdout,
          "D/G = general dispatch slowdown vs direct calls\n");
        Wr.PutText(Stdio.stdout,
          "D/I = interpreted slowdown vs direct calls\n");
        Wr.PutText(Stdio.stdout,
          "Note: general dispatch mode uses thin forwarding lambdas\n");
        Wr.PutText(Stdio.stdout,
          "      that defeat the pointer check but add ~1 interp call/crossing.\n");
        Wr.Flush(Stdio.stdout);
      END
    END
  EXCEPT Scheme.E(err) =>
    Wr.PutText(Stdio.stderr, "Scheme.E: " & err & "\n");
    Wr.Flush(Stdio.stderr)
  END
END Main.
