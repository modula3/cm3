/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */
/*                                                                           */
/* Last modified on Tue Aug 25 13:11:19 PDT 1992 by heydon                   */

/* This is a prolog program that defines the Juno compilation equations.

   These rules assume certain notational conventions for representing Juno
   programs. In general, Juno constructs are represented as Prolog lists,
   where keywords are spelled with lower-case letters (so they will be Prolog
   atoms). For example, to represent the Juno command "do SKIP; x := 2 od", we
   write: [do, [skip,;,[[x],:=,[2]]], od]. Notice how we use commas to
   separate list elements.

   Currently, these equations only compile assignments that have one variable
   on the left and one expression on the right of the ':='. However, to allow
   for generalization, each should still be represented by a list, as in
   [[x],:=,[2]].

   Procedure calls take the form [[inouts],:,name,[ins]]. The inout and in
   parameters must always appear in lists, even if they are empty. A procedure
   call with out parameters is represented by an assignment in which the right
   hand side is *not* a list of expressions, but simply a procedure call. A
   stand-alone call thus takes the form: [[outs],:=,[[inouts],:,name,[ins]]].
   If a procedure has exactly one out parameter, then the following are
   equivalent: [[out],:=,[[inouts],:,name,[ins]]] (note that procedure call is
   not grouped on the right) and [[out],:=,[[[inouts],:,name,[ins]]]].

   Most Juno operators can appear without any special quoting. The known
   exceptions are: '|', '>='.

   The first argument to projections and existential quantifications must be a
   *list* of atoms, even if it is only a singleton. Thus, we must write:
   [[x],::,skip] and [e,[x],:,true].

   Function and predicate applications are also written as lists, but the
   function or predicate name is always the first element of the list. Thus,
   we write [+,2,3] and not [2,+,3]. Even the equality predicate and the
   logical predicates like "and" and "or" must be written in this form.

   The function for unary negation is denoted by 'neg' rather than '-' (which
   is the binary function for subtraction).

   The Juno functions () and [] are not supported directly. Use [cons,a,b] to
   form the Juno pair (a,b) and use [list,a,b,c] to form the Juno list [a,b,c].
*/

load(sets).                               /* set operations */
load(scope).                              /* predicate and function defs */
load(misc).			          /* miscellaneous predicates */
load(built-in).                           /* load database of built-in funcs */

/* Top-level Compilation Equation C ======================================== */

c(S) :-
  c1(S, S2),
/*write(S2), nl, */
  c2(S2, S3),
/*write(S3), nl. */
  c4(S3,s,h,a,g,[l],S4),
  write('   '), print(S4).

/* Compilation Equation C1 ================================================= */

/* Atomic Commands */
c1(abort, abort).                                                      /*  1 */
c1(skip, skip).                                                        /*  2 */
c1([X,:=,T], [X,:=,T]).                                                /*  3 */
c1([InOuts,:,Proc,Ins], [InOuts,:,Proc,Ins]).                          /*  4 */

/* Non Guard Arrow Commands */
c1([do,S,od], [do,Sres,od]) :- c1(S, Sres).                            /*  5 */
c1([if,S,fi], [if,Sres,fi]) :- c1(S, Sres).                            /*  6 */
c1([save,I,in,S,end], [save,I,in,Sres,end]) :- c1(S, Sres).            /*  7 */
c1([S,;,T], [Sres,;,Tres]) :- c1(S, Sres), c1(T, Tres).                /*  8 */
c1([S,'|',S2], [Sres,'|',S2res]) :- c1(S, Sres), c1(S2, S2res).        /*  9 */
c1([X,::,S], [X,::,Sres]) :- c1(S, Sres).                              /* 10 */

/* Guard Arrow Commands */
c1([P,->,[X,::,S]], [X,::,Sres]) :- !, c1([P,->,S], Sres).             /* 12 */
c1([P,->,[S,'|',S2]], [Sres,'|',S2res]) :-                             /* 13 */
  !, c1([P,->,S], Sres), c1([P,->,S2], S2res).
c1([P,->,[S,;,T]], [Sres,;,Tres]) :-                                   /* 14 */
  !, c1([P,->,S], Sres), c1(T, Tres).
c1([P,->,[P2,->,S]], Sres) :- !, c1([[and,P,P2],->,S], Sres).          /* 15 */
c1([P,->,T], [P,->,Tres]) :- c1(T, Tres).                              /* 11 */

/* Compilation Equation C2 ================================================= */

/* Non-Query Commands ------------------------------------------------------ */

/* Simple Commands */
c2(abort, abort).                                                      /*  1 */
c2(skip, skip).                                                        /*  2 */
c2([X,:=,T], [X,:=,T]).                                                /*  3 */
c2([InOuts,:,Proc,Ins], [InOuts,:,Proc,Ins]).                          /*  4 */
c2([do,S,od], [do,Sres,od]) :- c2(S, Sres).                            /*  5 */
c2([if,S,fi], [if,Sres,fi]) :- c2(S, Sres).                            /*  6 */
c2([save,I,in,S,end], [save,I,in,Sres,end]) :- c2(S, Sres).            /*  7 */
c2([S,;,T], [Sres,;,Tres]) :- c2(S, Sres), c2(T, Tres).                /*  8 */
c2([S,'|',S2], [Sres,'|',S2res]) :- c2(S, Sres), c2(S2, S2res).        /*  9 */

/* Transform Guard Arrows to Queries */
c2([P,->,T], [Pres,;,Tres]) :- c2([P,?,[]], Pres), c2(T, Tres).        /* 10 */

/* Projection (introduce C2') */
/* We have a new rule to handle projections containing hints. */
c2([X,::,S], [X2,::,[Inits,;,Sres]]) :-
  find_hints(X, X2, Inits), Inits \= [],
  !, c2p(S,X2,Sres).
c2([X,::,S], [X,::,Sres]) :- c2p(S,X,Sres).                            /* 11 */

/* Query Commands ---------------------------------------------------------- */

/* Atomic Queries */
c2([true,?,[]], skip).                                                 /* 12 */
c2([false,?,[]], fail).                                                /* 13 */

/* Propositional Connectives (introduce FLIP) */
c2([[not,P],?,[]], [flip,Pres]) :- !, c2([P,?,[]], Pres).              /* 14 */
c2([[or,P,Q],?,[]], [Pres,'|',Qres]) :-                                /* 15 */
  !, c2([P,?,[]], Pres), c2([Q,?,[]], Qres).
c2([[and,P,Q],?,[]], [Pres,;,Qres]) :-                                 /* 16 */
  !, c2([P,?,[]], Pres), c2([Q,?,[]], Qres).

/* Eliminate '#', '>', '>=' */
c2([[#,t1,t2],?,[]], [flip,[[=,t1,t2],?,[]]]).                         /* 17 */
c2([[>,t1,t2],?,[]], [[<,t2,t1],?,[]]).                                /* 18 */
c2([['>=',t1,t2],?,[]], [['<=',t2,t1],?,[]]).                          /* 19 */

/* Existential Quantification (introduce C2'') */
c2([[e,V,:,P],?,[]], Eres) :- !, c2pp([[e,V,:,P],?,[]], Eres).         /* 20 */

/* All Other Queries */
c2([P,?,[]], [P,?,[]]).                                                /* 21 */

/* Compilation Equation C2' ================================================ */

c2p([X,::,S],V, [X2,::,[Inits,;,Sres]]) :-
  find_hints(X, X2, Inits), Inits \= [],
  !, union(X2,V, XuV), c2p(S,XuV, Sres).
c2p([X,::,S],V, [X,::,Sres]) :- !, union(X,V, XuV), c2p(S,XuV, Sres).  /* 23 */
c2p([S,'|',S2],V, [Sres,'|',S2res]) :-                                 /* 24 */
  !, c2p(S,V,Sres), c2p(S2,V,S2res).
c2p([S,;,T],V, [Sres,;,T]) :- !, c2p(S,V,Sres).                        /* 25 */
c2p([P,->,T],V, [Pres,;,T]) :- !, c2pp([P,?,V], Pres).                 /* 26 */
c2p(T,V, [TrueQ,;,Tres]) :-                                            /* 22 */
  c2pp([true,?,V], TrueQ), c2(T, Tres).

/* Compilation Equation C2'' =============================================== */

c2pp([P,?,V], Res) :- a1(P, Pres), a2([Pres,?,V], Res).

/* Compilation Equation A1 ================================================= */

/* transform P into constraint normal form */
a1(P, Pres) :- b1(P, Pres2), b2(Pres2, Pres).

/* Compilation Equation B1 ================================================= */

/* eliminate functional equalities and functional arguments */

/* Compound Formulas */
b1([or,P,Q], [or,Pres,Qres]) :- !, b1(P, Pres), b1(Q, Qres).           /* 27 */
b1([and,P,Q], [and,Pres,Qres]) :- !, b1(P, Pres), b1(Q, Qres).         /* 28 */
b1([e,V,:,P], [e,V,:,Pres]) :- !, b1(P, Pres).                         /* 29 */

/* Eliminate Function Equalities */
b1([=,[F|Ft],[G|Gt]], [e,[C],:,[and,Fres,Gres]]) :-                    /* 30 */
  !, gensym(v, C), b1([=,C,[F|Ft]], Fres), b1([=,C,[G|Gt]], Gres).

/* Eliminate Functional Arguments */
b1([P|Args], [and,[e,[C],:,Gres],Pres]) :-                             /* 32 */
  P \= '=', P \= 'near',
  first_non_atomic(Args,G,N), G \= 0,
  !, gensym(v, C),
  b1([=,C,G],Gres),
  replace(Args,NewArgs,C,N),
  b1([P|NewArgs],Pres).
b1([=,V,[F|Args]], [and,[e,[C],:,Gres],Fres]) :-                       /* 33 */
  atomic(V),
  first_non_atomic(Args,G,N), G \= 0,
  !, gensym(v, C),
  b1([=,C,G],Gres),
  replace(Args,NewArgs,C,N),
  b1([=,V,[F|NewArgs]],Fres).
b1([=,[F|Args],V], [and,[e,[C],:,Gres],Fres]) :-
  atomic(V),
  first_non_atomic(Args,G,N), G \= 0,
  !, gensym(v, C),
  b1([=,C,G],Gres),
  replace(Args,NewArgs,C,N),
  b1([=,[F|NewArgs],V],Fres).

/* All Other Constraints */
b1(F,F).                                                               /* 38 */

/* Compilation Equation B2 ================================================= */

/* transform tree to normal form */

/* Recursively Transform Sub-Trees to Normal Form */
b2([or,P,Q], [or,Pres,Qres]) :- !, b2(P, Pres), b2(Q, Qres).           /* 39 */
b2([e,V,:,P], P2res) :- !, b2(P, P2), b2p([e,V,:,P2], P2res).          /* 40 */
b2([and,P,Q], PQres) :-                                                /* 41 */
  !, b2(P, P2), b2(Q, Q2), b2p([and,P2,Q2], PQres).
b2(F, F).                                                              /* 42 */

/* Push Root Node Down to Proper Level */
b2p([e,V,:,[or,P,Q]], [or,Pres,Qres]) :-                               /* 43 */
  !, b2p([e,V,:,P],Pres), b2p([e,V,:,Q],Qres).
b2p([and,[or,P1,P2],Q], [or,Res1,Res2]) :-                             /* 44 */
  !, b2p([and,P1,Q], Res1), b2p([and,P2,Q], Res2).
b2p([and,Q,[or,P1,P2]], [or,Res1,Res2]) :-
  !, b2p([and,Q,P1], Res1), b2p([and,Q,P2], Res2).
b2p([and,[e,V,:,P],Q], [e,V,:,Res]) :-                                 /* 45 */
  !, b2p([and,P,Q], Res).
b2p([and,Q,[e,V,:,P]], [e,V,:,Res]) :-
  !, b2p([and,Q,P], Res).
b2p(F, F).                                                             /* 46 */

/* Compilation Equation A2 ================================================= */

/* transform OR queries into ELSE statements */

a2([[or,P,Q],?,V], [Pres,'|',Qres]) :-                                 /* 47 */
  !, a2([P,?,V], Pres), a2([Q,?,V], Qres).
a2(S, Sres) :- S = [P,?,V], a3(S, Sres).                               /* 48 */

/* Compilation Equation A3 ================================================= */

/* These compilation equations remove E quantifiers from constraints in normal
   form. Eventually, we may want to do things differently, since this approach
   is slightly inefficient: it converts the temporary E variables into
   projected variables, so the compiler will waste time after a solve copying
   the results of the solve back into slots on the stack frame where the
   projected variables live. If we were to instead maintain the distinction
   between (temporary) E variables and projected variables, the compiler could
   simply drop the values found for the E variables after a solve.
*/

/* convert E to Projection */
a3([[e,V,:,P],?,VL], [V,::,Res]) :-                                    /* 49 */
  !, union(V, VL, VuVL),
  a3([P,?,VuVL], Res).

a3(C, Cres) :- C = [P,?,V], b3(C, Cres).                               /* 50 */

/* Compilation Equation B3 ================================================= */

/* Implementation Note: When we pick predicates and equalities out of the
   current constraint at this stage, we would always like to pick out those
   simple predicates that translate into queries *first*; only once all such
   simple predicates have been removed from the current constraint do we want
   to pick out simple predicates that translate into assignments.

   Since predicate calls always translate into queries, we look for those
   first. However, this implementation does not necessarily choose equalities
   that translate into queries over those that translate into assignments.
*/

/* Pull out simple predicates of the form "p(y,..,z)" */
b3([P,?,V], [I,;,Res]) :-
  find_pcall(P, V, Call),
  Call = [Pred|_], Pred \= '=',
  !, remove_pred(P, Call, Pres),
  I = [Call,?,[]],
  b3([Pres,?,V], Res).

/* Pull out simple predicates of the form "x = y" */
b3([P,?,V], Qres) :-
  find_equal(P, Equal),
  !, remove_pred(P, Equal, Pres),
  Equal = [=,X,Y],
  /* 4 cases to consider based on which of X,Y are in the set V */
  ( (member(X, V), member(Y, V), !,                             /* X, Y in V */
     I = [[X],:=,[Y]], Qres = [Res,;,I],
     replace_all(Pres,P2res,X,Y), remove_atom(V,X,Vres))
  ; (P2res = Pres, Qres = [I,;,Res],
     ( (member(X, V), !, I = [[X],:=,[Y]], remove_atom(V,X,Vres))  /* X in V */
     ; (member(Y, V), !, I = [[Y],:=,[X]], remove_atom(V,Y,Vres))  /* Y in V */
     ; (           I = [[=,X,Y],?,[]], Vres = V)))),              /* neither */
  b3([P2res,?,Vres], Res).

/* Pull out simple predicates of the form "x = f(y,..,z)" */
b3([P,?,V], [I,;,Res]) :-
  find_fcall(P, V, Call),
  !, remove_pred(P, Call, Pres),
  Call = [=,A1,A2],
  /* set X to the atomic variable, F to the function call */
  ( (atomic(A1), !, X = A1, F = A2)
  ; (atomic(A2), !, X = A2, F = A1)),
  ( (member(X, V), !, I = [[X],:=,[F]], remove_atom(V,X,Vres))
  ; (           I = [[=,X,F],?,[]], Vres = V)),
  b3([Pres,?,Vres], Res).

/* No transformation otherwise */
b3(C, Cres) :-
  ( (b4(C, C4), !, c2pp(C4, Cres))
  ; Cres = C).

/* Compilation Equation B4 ================================================= */

b4([P,?,V], [Pres,?,V]) :- !, b4(P, Pres).
b4([and,P,Q], [and,Pres,Q]) :- b4(P, Pres), !.
b4([and,P,Q], [and,P,Qres]) :- !, b4(Q, Qres).

/* Eliminate Predicate and Function Calls */
b4(P, Pres) :-                                                         /* 35 */
  P = [_|Args],
  is_atomic_list(Args),
  pred(P, Pdef),
  !, a1(Pdef, Pres).
b4([=,V,F], Fres) :-                                                   /* 36 */
  F = [_|Args],
  atomic(V),
  is_atomic_list(Args),
  func(F,V, Fdef),
  !, a1(Fdef, Fres).
b4([=,F,V], Fres) :-
  F = [_|Args],
  atomic(V),
  is_atomic_list(Args),
  func(F,V, Fdef),
  !, a1(Fdef, Fres).

/* Compilation Equation C4 ================================================= */

/* Commands ---------------------------------------------------------------- */

/* SKIP, ABORT */
c4(skip,S,H,A,G,L, [[=,pc,S],->,[pc,:=,H]]).
c4(abort,S,H,A,G,L, [[=,pc,S],->,[pc,:=,A]]).

/* Sequencing, Else */
c4([C1,;,C2],S,H,A,G,L, [B1,[],B2]) :-
  c4(C1, S,L,A,G,[0|L], B1), c4(C2, L,H,A,G,[1|L], B2).
c4([C1,'|',C2],S,H,A,G,L, [B1,[],B2]) :-
  c4(C1, S,H,A,L,[0|L], B1), c4(C2, L,H,A,G,[1|L], B2).

/* IF..FI, DO..OD */
c4([if,C,fi],S,H,A,G,L, B) :-
  c4(C,S,H,A,A,L, B).
c4([do,C,od],S,H,A,G,L, [B,[],[[=,pc,L],->,[pc,:=,S]]]) :-
  c4(C,S,L,A,H,[0|L], B).

/* Queries */
c4([true,?,[]],S,H,A,G,L, [[=,pc,S],->,[pc,:=,H]]) :- !.
c4([P,?,[]],S,H,A,G,L, Pres) :- !, c4p(P,S,H,G,L, Pres).
c4([P,?,V],S,H,A,G,L, [[=,pc,S],->,[solve,T,V,H,G]]) :- p_to_T(P,T).
p_to_T([and,P,Q], T) :- !, p_to_T(P,Tp), p_to_T(Q,Tq), union(Tp,Tq,T).
p_to_T(P, T) :- T = [P].

/* Projection */
/* Since we aren't handling variable allocations in the procedure frame or
   global table, ignore the variables in a projection for now. Presumably, c3
   will remove projections before this stage. */
c4([V,::,C],S,H,A,G,L, B) :- c4(C,S,H,A,G,L, B).

/* Flip */
c4([flip,C],S,H,A,G,L, Cres) :- c4(C,S,G,A,H,L, Cres).

/* Procedure Call (command) */
c4([Outs,:=,Proc],S,H,A,G,L, Res) :-
  Proc = [InOuts,:,Name,Ins], Outs = [_|_], InOuts = [_|_], Ins = [_|_],
  !, length(Outs, OutCnt),
  Res = [PushRes,[],[CallRes,[],PopRes]],
  PushRes = [[=,pc,S],->,[[bc,incsp,OutCnt],;,[pc,:=,L]]],
  c4e2(Proc,L,[1|L],A,[0|L],OutCnt, CallRes),
  PopRes = [[=,pc,[1|L]],->,[Pops,;,[pc,:=,H]]],
  pop_list(Outs, Pops).

/* Assignment (must come after procedure call) */
c4(Assign,S,H,A,G,L, Res) :- Assign = [_,:=,_], c4c2(Assign,S,H,A,G,L,[], Res).

/* assignment base case */
c4c2([[],:=,[]],S,H,A,G,L,Vars, [[=,pc,S],->,[Pops,;,[pc,:=,H]]]) :-
  reverse(Vars, RevVars),
  pop_list(RevVars, Pops).

/* assignment recursive case */
c4c2([[V1|Vlist],:=,[T1|Tlist]],S,H,A,G,L,Vars, Res) :-
  Res = [PushRes,[],[Res2,[],PopRes]],
  L1 = [0|L],
  c4e(T1,S,L1,L,[0|L1], PushRes),
  c4c2([Vlist,:=,Tlist],L1,H,A,G,[1|L1],[V1|Vars], Res2),
  PopRes = [[=,pc,L],->,PopCmd],
  length(Vars, NumVars),
  ( (NumVars = 0, !, PopCmd = [pc,:=,A])
  ; (PopCmd = [[bc,decsp,NumVars],;,[pc,:=,A]])).

/* Expressions ------------------------------------------------------------- */

/* Constants and Variables */
c4e(X,S,D,U,L, [[=,pc,S],->,[[bc,push,X],;,[pc,:=,D]]]) :-
  atomic(X).
c4e(X,S,D,U,L, [[=,pc,S],->,[[bc,push,X],;,[pc,:=,D]]]) :-
  X = [Y,'.',Z], atom(Y), atom(Z).

/* Functional Procedure Calls */
c4e(Proc,S,D,U,L, Res) :-
  Proc = [InOuts,:,Name,Ins], InOuts = [_|_], Ins = [_|_], !,
  Res = [PushRes,[],Res2],
  PushRes = [[=,pc,S],->,[[bc,incsp,1],;,[pc,:=,L]]],
  c4e2(Proc,L,D,U,[0|L],1, Res2).

/* Functional procedure call with inouts */
c4e2(Proc,S,D,U,L,Depth, Res) :-
  Proc = [InOuts,:,Name,Ins], InOuts = [_|_], Ins = [_|_], !,
  Res = [PushRes,[],[Res2,[],PopRes]],
  PushRes = [[=,pc,S],->,[PushInOuts,;,[pc,:=,L]]],
  push_list(InOuts,PushInOuts),
  length(InOuts, InOutCnt), Depth2 is Depth + InOutCnt,
  c4e2([Name|Ins],L,[1|L],U,[0|L],Depth2, Res2),
  PopRes = [[=,pc,[1|L]],->,[PopInOuts,;,[pc,:=,D]]],
  pop_list(InOuts,PopInOuts).

/* Built-In Functions */
c4e(F,S,D,U,L, Res) :- F = [Name|_], func(Name), !, c4e2(F,S,D,U,L,0, Res).

/* User-Defined Functions */
c4e(F,S,D,U,L, Res) :-
  F = [_|_],
  Res = [PushRes,[],Res2],
  PushRes = [[=,pc,S],->,[[bc,incsp,1],;,[pc,:=,L]]],
  c4e2(F,L,D,U,[0|L],1, Res2).

/* built-in base case */
c4e2([F],S,D,U,L,Depth, [[=,pc,S],->,[Call,;,Jump]]) :-
  func(F), !,
  /* handle 'cons', 'list' specially */
  ( (F = 'cons', !, Call = [bc,F])
  ; (F = 'list', !, Call = [bc,F,Depth])
  ; Call = [bc,F,U]),
  Jump = [pc,:=,D].

/* user-defined base case */
c4e2([F],S,D,U,L,Depth, [[=,pc,S],->,[Call,;,Jump]]) :-
  Call = [bc,call,F,Depth],
  Jump = [[bc,cjump,D],;,[pc,:=,U]].

/* recursive case */
c4e2([F,T1|Args],S,D,U,L,Depth, Res) :-
  Res = [PushRes,[],[Res2,[],PopRes]],
  L1 = [0|L], Depth2 is Depth + 1,
  c4e(T1,S,L1,L,[0|L1], PushRes),
  c4e2([F|Args],L1,D,U,[1|L1],Depth2, Res2),
  PopRes = [[=,pc,L],->,PopCmd],
  ( (Depth = 0, !, PopCmd = [pc,:=,U])
  ; (PopCmd = [[bc,decsp,Depth],;,[pc,:=,U]])).

/* Predicates -------------------------------------------------------------- */

/* User Defined and Built-In Predicates */
c4p(P,S,T,F,L, Res) :- P = [_|_], c4p2(P,S,T,F,L,0, Res).

/* built-in base case */
c4p2([P],S,T,F,L,Depth, [[=,pc,S],->,[Call,;,Jump]]) :-
  pred(P), !,
  Call = [bc,P],
  Jump = [[bc,cjump,T],;,[pc,:=,F]].

/* user-defined base case */
c4p2([P],S,T,F,L,Depth, [[=,pc,S],->,[Call,;,Jump]]) :-
  Call = [bc,call,P,Depth],
  Jump = [[bc,cjump,T],;,[pc,:=,F]].

/* recursive case */
c4p2([P,T1|Args],S,T,F,L,Depth, Res) :-
  Res = [PushRes,[],[Res2,[],PopRes]],
  L1 = [0|L], Depth2 is Depth + 1,
  c4e(T1,S,L1,L,[0|L1], PushRes),
  c4p2([P|Args],L1,T,F,[1|L1],Depth2, Res2),
  PopRes = [[=,pc,L],->,PopCmd],
  ( (Depth = 0, !, PopCmd = [pc,:=,F])
  ; (PopCmd = [[bc,decsp,Depth],;,[pc,:=,F]])).

/* ================ Predicates for Pretty Printing "Bytecode" ============== */

/* commands */
portray([A,[],B]) :- !, print(A), write('[] '), print(B).
portray([A,->,B]) :- !, print(A), write(' -> '), print(B), nl.
portray([A,:=,B]) :- !, print(A), write(' := '), print(B).
portray([A,;,B])  :- !, print(A), write('; '), print(B).

/* predicates */
portray([=,A,B])  :- !, print(A), write(' = '), print(B).

/* functions */
portray([bc|X])   :- !, write('BC('), printlist(X), write(')').
portray([solve|X]):- !, write('Solve('), printlist(X), write(')').

/* labels */
portray([l])      :- !, write('L').
portray([0|T])    :- !, print(T), write('0').
portray([1|T])    :- !, print(T), write('1').

/* all other cases */
portray(X)        :- !, write(X).

/* printlist */
printlist([]).
printlist([T])    :- print(T).
printlist([H|T])  :- print(H), write(','), printlist(T).
