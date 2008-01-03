/* Last modified on Mon Jun 29 11:51:06 PDT 1992 by heydon                   */

/* This prolog module defines those predicates and functions built-in to the
   Juno machine (i.e., those predicates and functions for which there are byte
   codes. Built-in predicates are declared with the unary predicate pred/1,
   and built-in functions are declared with the unary predicate func/1. Even
   those these predicates are used differently in the file "scope.pl", they
   are defined there to have binary and ternary arity, respectively.
*/

pred(real).
pred(int).
pred(text).
pred(pair).
pred(=).
pred(<).
pred(<=).

func(+).
func(-).
func(*).
func(/).
func(div).
func(mod).
func(neg).
func(abs).
func(floor).
func(ceiling).
func(round).
func(max).
func(min).
func(sin).
func(cos).
func(atan).
func(ln).
func(exp).
func(car).
func(cdr).
func(cons).
func(list).
