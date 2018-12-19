%%   Copyright 2018 The Spectector authors
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

:- doc(title, "Speculative non-interference check").

:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(streams)).

:- use_module(muasm_semantics).
:- use_module(muasm_program).
:- use_module(muasm_print).
:- use_module(concolic(symbolic)).
:- use_module(concolic(concolic), [pathgoal/2, sym_filter/2]).

:- export(noninter_check/2).
% `Low` is a list of register names or memory indices that are "low".
% `C0` is the initial configuration.
noninter_check(Low, C0) :-
	log('[exploring paths]'),
	( % (failure-driven loop)
	  (C,Trace) = ~concrun(C0),
	    ( member(timeout, Trace) ->
	        log('[timeout... exploring new path]'),
	        message(warning, 'timeout -- ignoring large path (refine initial configuration or assume unsafe)'),
		fail
	    ; true
	    ),
	    log('[path found]'),
	    pretty_print([triple(C0,Trace,C)]),
	    log('[checking speculative non-interference]'),
	    C0 = xc(_,C0n,_),
	    noninter_cex(Low, C0n, Trace, Safe), % TODO: Time this operation (for each path)
	    ( Safe = no ->
	        !, % stop on first unsafe path
		log('[program is unsafe]')
	    ; log('[path is safe]'),
	      fail % go for next path
	    )
	; log('[program is safe]')
	).


% Obtain a counter example for speculative non-interference.
% NOTE: see the paper for details, this check works on a single trace
%   at a time.

noninter_cex(Low, C0, Trace, Safe) :-
	( Mode = data ; Mode = control ),
	noninter_cex_(Mode, Low, C0, Trace),
	!,
	Safe = no.
noninter_cex(_, _, _, Safe) :-
	Safe = yes.

noninter_cex_(Mode, Low, C0a, TraceA0) :-
	erase_and_dump_constrs(C0a, InGoalA),
	erase_model([InGoalA,TraceA0]), % remove all other concrete assignments
	%
	( Mode = data ->
	    % Data-based leak
	    TraceA = TraceA0,
	    rename_symspec(Low,
	                   C0a, InGoalA, TraceA, [],
			   C0b, InGoalB, TraceB, [],
			   LowGoal),
	    member2(X, Y, TraceA, TraceB),
	    differ(X, Y, Cond), DiffGoal = [Cond]
	; Mode = control ->
	    % Control-based leak
	    select_spec_cond(TraceA0, TraceA, CondA), % select cond/1 in speculative fragments
	    rename_symspec(Low,
	                   C0a, InGoalA, TraceA, CondA,
			   C0b, InGoalB, TraceB, CondB,
			   LowGoal),
	    NegCondB = ~negcond(CondB), DiffGoal = [CondA,NegCondB],
	    X = sym(cond(CondA)), Y = sym(cond(NegCondB))
	; throw(unknown_mode(Mode))
	),
	% Get input model for two different heaps
	Goal = ~append(InGoalA,
	          ~append(~pathgoal(~sym_filter(TraceA)),
		    ~append(InGoalB,
		      ~append(~pathgoal(~sym_filter(TraceB)),
		        ~append(LowGoal,DiffGoal))))),
	get_model(Goal),
	%
	show_cex(C0a, TraceA, C0b, TraceB, X, Y).
	
show_cex(C0a, _TraceA, C0b, _TraceB, X, Y) :-
	log('[path is unsafe, showing counter-example initial configurations A and B]'),
	pretty_print([
	  msg('Case A:'),
	  (C0a,[X]), % TODO: where?
	  % (C0a,~append(TraceA, [X])),
	  msg('Case B:'),
	  (C0b,[Y]) % TODO: where?
	  % (C0b,~append(TraceB, [Y]))
		     ]).

% Obtain a copy of the trace, unifying:
%  - variables corresponding to low memory and registers
%    (all registers in initial configuration are implicitly low)
%  - the nonspec obs
% ExtraA and ExtraB are used to apply renaming to additional
% constraints not in the traces.
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

member2(X, Y, [X|_], [Y|_]).
member2(X, Y, [_|Xs], [_|Ys]) :-
	member2(X, Y, Xs, Ys).

differ(load(A), load(B), (A\=B)) :- A \== B.
differ(store(A), store(B), (A\=B)) :- A \== B.

% Obtain trace prefixes before a C cond(_) during speculation (fail if
% there are no more prefixes)
select_spec_cond([start(I)|Xs], Trace, C) :-
	append(Spec, [End|Rest], Xs),
	End = rollback(I), % only rollbacks
	!,
	( % obtain cond and drop the rest of spec
	  append(BeforeC, [sym(cond(C))|AfterC], Spec),
	  Trace = ~append([start(I)|BeforeC], Trace0),
%	  Trace0 = ~only_specmarks(~append(AfterC, Rest)) % drop all (wrong, not enough)
	  Trace0 = ~append(~only_specmarks(AfterC), [End|Rest]) % drop spec (it should be fine)
%	  Trace0 = ~append(AfterC, [End|Rest]) % drop spec (spurious during spec, is it fine?)
	; % continue with other speculative fragment
	  append([start(I)|Spec], [End|Trace0], Trace),
	  select_spec_cond(Rest, Trace0, C)
	).
select_spec_cond([X|Xs], [X|Trace], C) :-
	select_spec_cond(Xs, Trace, C).

only_specmarks([], []).
only_specmarks([X|Xs], [X|Ys]) :-
	( X = start(_) ; X = commit(_) ; X = rollback(_) ), !,
	only_specmarks(Xs, Ys).
only_specmarks([_|Xs], Ys) :-
	only_specmarks(Xs, Ys).

% ---------------------------------------------------------------------------
% (log messages)

:- use_module(engine(messages_basic), [message/2]).

log(X) :- message(user, X).
