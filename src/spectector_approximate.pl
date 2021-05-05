
:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

%:- doc(title, "Approximate Counting of Leaking Memory Locations").

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(format)).
:- use_module(library(streams)).

:- use_module(muasm_semantics).
:- use_module(muasm_program).
:- use_module(muasm_print).
:- use_module(spectector_flags).
:- use_module(spectector_stats).
:- use_module(concolic(ciaosmt)).
:- use_module(concolic(concolic), [pathgoal/2, sym_filter/2, conc_stats/3, set_nextpath_timeout/1]).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(engine(attributes)).


:- export(approximate_check/2).

approximate_check(Low, C0) :-
	find_leak(Low, C0, Leak),
	log('Leak found:'),
	log(Leak).

find_leak(Low, C0, NewLeak) :-
	concrun(C0, (_, Trace)),
	C0 = xc(_, C0a, _),
	erase_constraints(C0a, InGoal, _),
	erase_constraints(Trace, _, _),
	find_leak_on_path(Low, C0a, InGoal, Trace, NewLeak).


find_leak_on_path(Low, C0a, InGoal, Trace, NewLeak) :-
	InGoalA = InGoal,
	TraceA = Trace,
	
	rename_symspec(Low,
	               C0a, InGoalA, TraceA, [],
	               C0b, InGoalB, TraceB, [],
	               LowGoal),
	C0a = c(Ma, _),
	C0b = c(Mb, _),

	differdisj(TraceA, TraceB, OrCond),
	% TODO: Fix, failing should be accounted as no leakage
	\+ OrCond = [], % (just fail)
	DiffGoal = [~or_cond(OrCond) = bv(1, 64)],
	
	NewLowGoal=[forall([Location::bitvec(64)], (Location \= LeakingLocation -> select(Ma, Location) = select(Mb, Location)))],

	PathGoal = ~append(InGoalA,
	             ~append(~pathgoal(~sym_filter(TraceA)),
	               ~append(InGoalB, ~pathgoal(~sym_filter(TraceB))))),
	

	add_constraints(PathGoal),
	add_constraints(DiffGoal),
	add_constraints(LowGoal),
	add_constraints(NewLowGoal),
	log('constraints added, starting solving'),
	try_solve(Status),
	log('solved !'),
	(Status = sat ->
		log('SAT !'),
		erase_constraints([LeakingLocation], _, Model),
	log(Model),
		Model=[=(_, LeakingLocationValue)],
		format("0x~16r is leaking\n", LeakingLocationValue),
		NewLeak=LeakingLocationValue

	; Status = unknown ->
		log('Unknown'),
		fail
	;
		log('UNSAT !'),
		fail
	).

%% Everything below is a copy-paste from spectector_noninter

rename_symspec(Low,
               CSymA, InGoalA, TraceA, ExtraA,
               CSymB, InGoalB, TraceB, ExtraB,
               LowGoal) :-
    filter_nonspec(TraceA, NTraceA),
    copy_term((CSymA,InGoalA,TraceA,NTraceA,ExtraA),(CSymB,InGoalB,TraceB,NTraceB,ExtraB)),
    %
    CSymA = c(_,A), CSymB = c(_,A), % TODO: all registers are low (we could also pass them explicitly)
    unif_obs(NTraceA,NTraceB),
    unif_low_goal(Low, CSymA, CSymB, LowGoal, []).

% generate a goal to unify low values in both configurations Ca and Cb
unif_low_goal([], _, _) --> [].
unif_low_goal([X|Xs], Ca, Cb) -->
    unif_low_goal1(X, Ca, Cb),
    unif_low_goal(Xs, Ca, Cb).

unif_low_goal1(X, Ca, Cb) -->
    { Ca = c(Ma, Aa) },
    { Cb = c(Mb, Ab) },
    ( { atom(X) } -> [element(Aa, X, V), element(Ab, X, V)] % TODO: not needed since all registers are low, fix solver to deal with these if needed
    ; { integer(X) } -> [element(Ma, X, V), element(Mb, X, V)]
    ; { throw(unknown_low(X)) }
    ),
    !.

unif_obs([], []).
unif_obs([X|Xs], [Y|Ys]) :-
    ( X = load(A) -> Y = load(A)
    ; X = store(A) -> Y = store(A)
    ; weak_sni, X = value(A) -> Y = value(A) % For weak speculative non-interference, we consider also value(N) observations in the non-speculative trace
    ; true
    ),
    unif_obs(Xs,Ys).

% Obtain a non-speculative trace from a speculative trace
% (removes rollbacks)
filter_nonspec([start(I)|Xs], NTrace) :-
    append(Spec, [End|Rest], Xs),
    ( End = commit(I)
    ; End = rollback(I)
    ),
    !,
    ( End = commit(I) -> Rest2 = ~append(Spec, Rest)
    ; Rest2 = Rest
    ),
    filter_nonspec(Rest2, NTrace).
filter_nonspec([X|Xs], [X|NTrace]) :-
    filter_nonspec(Xs, NTrace).
filter_nonspec([], []).

% Given traces Xs and Ys, obtain a disjunction representing that at
% least one of the load or store addresses differ.
differdisj([], [], []).
differdisj([X|Xs], [Y|Ys], OrCond) :-
    ( differ(X, Y, Cond) -> OrCond = [Cond|OrCond0]
    ; OrCond = OrCond0
    ),
    differdisj(Xs, Ys, OrCond0).

or_cond([X]) := R :- !, R = X.
or_cond([X|Xs]) := X\/(~or_cond(Xs)).

% TODO: use booleans instead of bitvectors
differ(load(A), load(B), ite(A\=B, bv(1,64), bv(0,64))) :- A \== B.
differ(store(A), store(B), ite(A\=B, bv(1,64), bv(0,64))) :- A \== B.


:- use_module(engine(messages_basic), [message/2]).

log(X) :- message(user, X).

