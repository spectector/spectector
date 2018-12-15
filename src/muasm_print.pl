:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

:- doc(title, "Pretty printing for µAsm execution traces").

:- use_module(engine(messages_basic), [lformat/1]).
:- use_module(library(terms_vars)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(format)).
:- use_module(concolic(symbolic), [unassign/2, sym_to_map/2]).

% Pretty print configuration and traces with symbolic information and
% other annotations. The relation between symbolic variables is
% preserved in the output.
:- export(pretty_print/1).
pretty_print(CTs) :-
	\+ \+ pretty_print_(CTs).

pretty_print_(CTs0) :-
	CTs1 = ~remap(CTs0),
	CTs = ~noarr(CTs1),
	varset(CTs, Vars),
	unassign(Vars, Map),
	numbervars(Vars, 0, _),
	write('Assignments:'), nl,
	write('  '), write_map(Map), nl,
	print_items(CTs).

% turn dic into map
remap([]) := [].
remap([X|Xs]) := [~remap1(X)| ~remap(Xs)].

remap1((C,T)) := (~remap_conf(C),T) :- !.
remap1(triple(C0,T,C)) := triple(~remap_conf(C0),T,~remap_conf(C)) :- !.
remap1(X) := X.

remap_conf(xc(I,c(M,A),S)) := xc(I,c(~sym_to_map(M),~sym_to_map(A)),S) :- !.
remap_conf(c(M,A)) := c(~sym_to_map(M),~sym_to_map(A)) :- !.

% remove array variables from symbolic constraints % TODO: make it optional
noarr([]) := [].
noarr([X|Xs]) := [~noarr1(X)| ~noarr(Xs)].

noarr1((C,T)) := (C,~noarr_trace(T)) :- !.
noarr1(triple(C0,T,C)) := triple(C0,~noarr_trace(T),C) :- !.
noarr1(X) := X.

noarr_trace([]) := [] :- !.
noarr_trace([sym(Sym)|Xs]) := ~noarr_trace(Xs) :- noarr_sym(Sym), !. % (ignore)
noarr_trace([sym(element(_,K,V))|Xs]) := [sym(element(K,V))| ~noarr_trace(Xs)] :- !.
noarr_trace([sym(update(_,K,V,_))|Xs]) := [sym(update(K,V))| ~noarr_trace(Xs)] :- !.
noarr_trace([X|Xs]) := [X| ~noarr_trace(Xs)].

noarr_sym(arr_eq(_,_)) :- !. % (ignore)
noarr_sym(element(_,K,_)) :- atom(K), !. % (ignore)
noarr_sym(update(_,K,_,_)) :- atom(K), !. % (ignore)

% print items
print_items([]).
print_items([X|Xs]) :-
	print_item(X),
	print_items(Xs).

print_item(msg(Msg)) :- !,
	lformat(Msg), nl.
print_item((C,T)) :- !,
	write('conf:'), nl, print_conf(C),
	write('trace:'), nl, print_trace(T).
print_item(triple(C0,T,C)) :- !,
	write('initial conf:'), nl, print_conf(C0),
	write('trace:'), nl, print_trace(T),
	write('final conf:'), nl, print_conf(C).

print_conf(c(M,A)) :- !,
	write('  m='), write_map(M), nl,
	write('  a='), write_map(A), nl.
print_conf(xc(I,c(M,A),S)) :- !,
	write('  i='), writeq(I), nl,
	write('  m='), write_map(M), nl,
	write('  a='), write_map(A), nl,
	write('  s='), writeq(S), nl.

print_trace([]).
print_trace([X|Xs]) :-
	write('  '),
	( integer(X) ->
	    write(X), write(:)
	; X = sym(X0) ->
	    write('# '), writeq(X0), nl
	; writeq(X), nl
	),
	print_trace(Xs).

write_map(Map) :- write('['), write_map_(Map), write(']').

write_map_([]).
write_map_([X]) :- !, write_kv(X).
write_map_([X|Xs]) :- write_kv(X), write(','), write_map_(Xs).

write_kv(K=V) :- !, write_x(K), write('='), write_x(V).
write_kv(X) :- !, writeq(X). % TODO: this should not happen

write_x(X) :- integer(X), X > 15, !, format("0x~16r", [X]).
write_x(X) :- writeq(X).

