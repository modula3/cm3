/* Last modified on Fri Jun 26 18:54:14 PDT 1992 by heydon                   */

/* These are miscellaneous predicates defined for the Juno compilation
   equations.
*/

load('/udir/heydon/libi/proglog/sets').

/* Miscellaneous Functions ================================================= */

/* length(L, Len) -- true iff L is a list and its length is Len */
length([], 0).
length([_|T], Len) :- length(T, Len2), Len is Len2 + 1.

/* is_atomic_list(L) -- true iff all elements of list L are atomic */
is_atomic_list([]).
is_atomic_list([H|T]) :- atomic(H), is_atomic_list(T).

/* first_non_atomic(L,V,P) -- true iff first non-atomic value in list L is at
   position P (counting from 1) and has value V. If L is all atomic values,
   then P is the length of L and V is the atomic value 0.
*/
first_non_atomic([],0,0).
first_non_atomic([NonAtomic|_],NonAtomic,1) :- NonAtomic = [_|_].
first_non_atomic([H|T],V,P) :- atomic(H), first_non_atomic(T,V,P2), P is P2+1.

/* replace(L1,L2,V,P) -- true iff L2 is the list formed by replacing top-level
   element P (counting from 1) of list L1 with the value V.
*/
replace([H|T],[V|T],V,1).
replace([H|T1], [H|T2], V, P) :- P > 1, P2 is P-1, replace(T1,T2,V,P2).

/* replace_all(L1,L2,Old,New) -- true iff L2 is the list formed by replacing
   *all* occurrences (at any level) of Old by New in L1.
*/
replace_all([],[],Old,New) :- !.
replace_all(Old,New,Old,New) :- !.
replace_all(A,A,Old,New) :- atomic(A), !.
replace_all([Old|T1],[New|T2],Old,New) :-
  !, replace_all(T1,T2,Old,New).
replace_all([H1|T1],[H2|T2],Old,New) :-
  replace_all(H1,H2,Old,New),
  replace_all(T1,T2,Old,New).

/* collect_E_vars(P, Pres, Vars) */
collect_E_vars(P, Pres, Vars) :- collect_E_vars2(P, [], Pres, Vars).
collect_E_vars2([e,V,:,P], InVars, Pres, OutVars) :-
  !, union(InVars,V,VuInVars),
  collect_E_vars2(P, VuInVars, Pres, OutVars).
collect_E_vars2(P, InVars, P, InVars).

/* find_equal(NFC, Res) -- true iff Res is a simple equality predicate "x = y"
   of the normal form constraint NFC (which is assumed to be simply a
   conjunction of simple predicates).
*/
find_equal([and,P,Q], Res) :- find_equal(P, Res), !.
find_equal([and,P,Q], Res) :- !, find_equal(Q, Res).
find_equal(P, P) :- P = [=,X,Y], atomic(X), atomic(Y).

/* find_fcall(NFC, VL, Res) -- true iff Res is a simple predicate of the normal
   form constraint NFC (which is assumed to be simply a conjunction of simple
   predicates). Res must be of the form: "x = f(y,..,z)" or "f(y,..,z) = x",
   where the variables "y,..,z" must *not* appear in the list of variables VL.
*/
find_fcall([and,P,Q], VL, Res) :- find_fcall(P, VL, Res), !.
find_fcall([and,P,Q], VL, Res) :- !, find_fcall(Q, VL, Res).
find_fcall(P, VL, P) :-
  P = [=,X,Y], atomic(X), all_bound(Y, VL), !.
find_fcall(P, VL, P) :-
  P = [=,X,Y], atomic(Y), all_bound(X, VL), !.

/* find_pcall(NFC, VL, Res) -- true iff Res is a simple predicate of the normal
   form constraint NFC (which is assumed to be simply a conjunction of simple
   predicates). Res must be of the form: "p(y,..,z)", where the variables
   "y,..,z" must *not* appear in the list of variables VL.
*/
find_pcall([and,P,Q], VL, Res) :- find_pcall(P, VL, Res), !.
find_pcall([and,P,Q], VL, Res) :- !, find_pcall(Q, VL, Res).
find_pcall(P, VL, P) :- all_bound(P, VL), !.

/* all_bound(F, VL) -- true iff F is a function/predicate call all of whose
   arguments are variables not appearing in the list VL.
*/
all_bound([F|Args], VL) :- intersection(Args, VL, R), R = [].

/* remove_pred(NFC, Pred, Res) -- true iff Res is the result of removing all
   occurrences of the simple predicate Pred from the normal form constraint
   NFC (which is assumed to be simply a conjunction of simple predicates).
*/
remove_pred(Pred, Pred, true) :- !.
remove_pred([and,Pred,P], Pred, P) :- !.
remove_pred([and,P,Pred], Pred, P) :- !.
remove_pred([and,P,Q], Pred, [and,Pres,Qres]) :-
  !, remove_pred(P, Pred, Pres), remove_pred(Q, Pred, Qres).
remove_pred(P, Pred, P).

/* remove_atom(L, Atom, Res) -- true iff Res is the result of removing all
   occurrences of Atom from the top level of the list of atoms L.
*/
remove_atom([], Atom, []).
remove_atom([H|T], H, T) :- !.
remove_atom([H|T], Atom, [H|R]) :- remove_atom(T, Atom, R).

/* push_list(V, Pushes) */
push_list([V], [bc,push,V]).
push_list([V|Vlist], [[bc,push,V],;,PushRest]) :- push_list(Vlist, PushRest).

/* pop_list(V, Pops) */
pop_list([V], [V,:=,[bc,pop]]).
pop_list([V|Vlist], [PopRest,;,[V,:=,[bc,pop]]]) :- pop_list(Vlist, PopRest).

/* reverse(L1,L2) -- true iff L2 is the reverse list of L1 */
reverse(L1,L2) :- reverse2(L1,[],L2).
reverse2([],L,L).
reverse2([H|L1],L2,L3) :- reverse2(L1,[H|L2],L3).

/* find_hints(NearVarList, VarList, Inits) -- true iff VarList is a list of
   the variables in NearVarList and Inits is a command that assigns all
   hinted variables in NearVarList with their hints. If there are no hints in
   NearVarList, then Inits is the empty list.
*/
find_hints([], [], []).
find_hints([H|T1], [H|T2], Inits) :-
  atomic(H),
  !, find_hints(T1, T2, Inits).
find_hints([[V,~,E]|T1], [V|T2], NewInits) :-
  atomic(V),
  find_hints(T1, T2, Inits),
  semi_cons([[V],:=,[E]], Inits, NewInits).
semi_cons(S, [], S) :- !.
semi_cons(S, T, [S,;,T]).
