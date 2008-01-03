/* Last modified on Fri Jun 26 13:51:05 PDT 1992 by heydon                   */

/* This file defines predicates and functions for the top-level scope. It
   includes the built-in predicates like CONG and the built-in functions like
   CAR.

   To define a new predicate PREDICATE Foo(x,y,..,z) IS P, or to define a new
   function FUNCTION Foo(x,y,..,z) = w IS P, you write:

     pred([foo,X,Y,..,Z], P).       (* predicate definition *)
     func([foo,X,Y,..,Z], W, P).    (* function definition *)

   Where P is the "prologized" version of the predicate P, and where the
   predicate/function arguments are capitalized in P. For example:

     FUNCTION sqrt(x) = y IS x = y * y END

   is written as:

     func([sqrt,X], Y, [=,X,[*,Y,Y]]).

   If your predicate/function introduces new variables, then you should
   create them with calls to "gensym" as the body to the rule, as in:

     pred([pos,X], [e,:,[Y],[=,X,[*,Y,Y]]]) :- gensym(v, Y).
*/

/* Built-In Predicates ===================================================== */

/* CONG */
pred([cong,S,T], [=,[length2_,S],[length2_,T]]).

/* PARA */
pred([para,S,T],
  [e,[S1x,S1y,S2x,S2y,Sdx,Sdy,T1x,T1y,T2x,T2y,Tdx,Tdy],:,
    [and,[=,S,[cons,[cons,S1x,S1y],[cons,S2x,S2y]]],
     [and,[=,T,[cons,[cons,T1x,T1y],[cons,T2x,T2y]]],
      [and,[=,Sdx,[-,S2x,S1x]],
       [and,[=,Sdy,[-,S2y,S1y]],
        [and,[=,Tdx,[-,T2x,T1x]],
         [and,[=,Tdy,[-,T2y,T1y]],
          [=,[*,Sdx,Tdy],[*,Sdy,Tdx]]]]]]]]]).

/* HOR */
pred([hor,S],
  [e,[S1x,S1y,S2x,S2y],:,
    [and,[=,S,[cons,[cons,S1x,S1y],[cons,S2x,S2y]]],
     [and,[real,S1x],
      [and,[real,S1y],
       [and,[real,S2x],
        [and,[real,S2y],
         [=,S1y,S2y]]]]]]]).

/* VER */
pred([ver,S],
  [e,[S1x,S1y,S2x,S2y],:,
    [and,[=,S,[cons,[cons,S1x,S1y],[cons,S2x,S2y]]],
     [and,[real,S1x],
      [and,[real,S1y],
       [and,[real,S2x],
        [and,[real,S2y],
         [=,S1x,S2x]]]]]]]).

/* Built-In Functions ====================================================== */

/* Sqr */
func([sqr_,A], X, [=,[*,A,A],X]).

/* Dist2 */
func([dist2_,P,Q], R,
  [e,[Px,Py,Qx,Qy,Dx,Dy],:,
    [and,[=,P,[cons,Px,Py]],
     [and,[=,Q,[cons,Qx,Qy]],
      [and,[=,Dx,[-,Qx,Px]],
       [and,[=,Dy,[-,Qy,Py]],
        [=,R,[[sqr_,Dx],+,[sqr_,Dy]]]]]]]]) :-
  gensym(v,Px), gensym(v,Py),
  gensym(v,Qx), gensym(v,Qy),
  gensym(v,Dx), gensym(v,Dy).

/* Length2 */
func([length2_,S], Len,
  [e,[P1,P2],:,
    [and,[=,S,[cons,P1,P2]],
     [=,[dist2_,P1,P2],Len]]]) :-
  gensym(v,P1), gensym(v,P2).

/* REL */
func([rel,P,S], R,
  [e,[Px,Py,Rx,Ry,S1x,S1y,S2x,S2y,Dx,Dy],:,
    [and,[=,P,[cons,Px,Py]],
     [and,[=,R,[cons,Rx,Ry]],
      [and,[=,S,[cons,[cons,S1x,S1y],[cons,S2x,S2y]]],
       [and,[=,Dx,[-,S2x,S1x]],
        [and,[=,Dy,[-,S2y,S1y]],
         [and,[=,Rx,[+,S1x,[-,[*,Px,Dx],[*,Py,Dy]]]],
          [=,Ry,[+,S1y,[-,[*,Px,Dy],[*,Py,Dx]]]]]]]]]]]).

/* List introduction */
func([list,Y], X, [=,X,[cons,Y,nil]]).
func([list,Head|Tail], X, [e,[Y],:,[and,[=,X,[cons,Head,Y]],Res]]) :-
  gensym(v, Y),
  func([list|Tail], Y, Res).

/* CAR, CDR */
func([car,Y], X, [e,[Z],:,[=,Y,[cons,X,Z]]]) :- gensym(v,Z).
func([cdr,Y], X, [e,[Z],:,[=,Y,[cons,Z,X]]]) :- gensym(v,Z).

/* -, / */
func([-,Y,Z], X, [=,Y,[+,X,Z]]).
func([/,Y,Z], X, [and,[e,[W],:,[=,1,[*,W,Z]]],[=,Y,[*,X,Z]]]) :- gensym(v,W).

/* Unary - */
func([neg,Y], X, [=,0,[+,X,Y]]).

/* User-Defined Predicates and Functions =================================== */

/* PREDICATE is_sqr(x) */
pred([is_sqr,X], [e,[Y],:,[=,X,[*,Y,Y]]]) :- gensym(v, Y).

/* PREDICATE neq_zero(x) -- true iff x # 0 */
pred([neq_zero,X], [e,[Y],:,[=,1,[*,X,Y]]]) :- gensym(v, Y).

/* PREDICATE pos(x) -- true iff x >= 0 */
pred([pos,X], [e,[Y],:,[=,X,[*,Y,Y]]]) :- gensym(v, Y).

/* PREDICATE neg(x) -- true iff x < 0 */
pred([neg,X], [and,[neq_zero,X],[pos,[-,X]]]).

/* FUNCTION sqrt(x) = y */
func([sqrt,X], Y, [=,X,[*,Y,Y]]).

/* FUNCTION hypot(x,y) = z */
func([hypot,X,Y], Z, [and,[pos,Z],[=,Z,[sqrt,[+,[*,X,X],[*,Y,Y]]]]]).
