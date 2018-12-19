%%   Copyright 2018 Marco Guarnieri, José Francisco Morales, Andrés Sánchez
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

:- module(_, [], [assertions, fsyntax, datafacts, dcg, hiord]).

:- doc(title, "µAsm semantics").

:- use_module(library(lists)).
:- use_module(muasm_program).
:- use_module(spectector_flags).
:- use_module(concolic(concolic)).
:- use_module(concolic(symbolic)).

% ---------------------------------------------------------------------------
:- doc(section, "Evaluation (both spec and non-spec)").

:- export(mrun/2).
% Run c/2 or xc/3 configurations
mrun(C0) := CT :-
	with_trace(Trace, mrun_(C0,C)),
	CT = (C,Trace).

% Run c/2 or xc/3 configurations
mrun_(C0) := C :- C0 = c(_,_), !, C = ~run(C0, ~get_step_limit).
mrun_(C0) := C :- C0 = xc(_,_,_), !, C = ~xrun(C0, ~get_step_limit).

:- export(new_c/3).
new_c(M,A) := c(~map_to_sym(M), ~map_to_sym(A)).

:- export(new_xc/3).
new_xc(M,A) := xc(0,~new_c(M,A),[]).

% Examples:
%   C = ~new_c([], [pc=0]), (C,Trace) = ~concrun(~p_2_5,C0).

:- export(concrun/2).
% Execute mrun/2 using concolic search (see conc_call/3)
concrun(C0) := CT :-
	( C0 = c(_,_) -> InConf = C0
	; C0 = xc(_,C1,_) -> InConf = C1
	; fail
	),
	conc_call(mrun_(C0,C), InConf, Trace),
	CT = (C,Trace).

% ---------------------------------------------------------------------------
:- doc(section, "Preliminaries").

:- use_package(.(muasm_syntax)).

% Registers
reg(X) :- atom(X).

% Configuration <m,a>
conf(c(_M,_A)).

% ---------------------------------------------------------------------------
:- doc(section, "Expression evaluation").

% Symbolic expression evaluation (replaces registers by its symbolic
% values). This maps from the language expressions to the solver
% expressions.

ev(X,_A) := R :- var(X), !, R=X.
ev(pc,A) := R :- !, R = ~element0(A,pc). % TODO: slow if we use element/2, fix?
ev(X,A) := R :- reg(X), !, R = ~elementF(A,X). % TODO: can we use element/2?
ev(N,_A) := N :- integer(N), !.
ev(+X,A) := ~ev(X,A) :- !.
ev(-X,A) := R :- !, R = -(~ev(X,A)).
ev(X+Y,A) := R :- !, R = ~ev(X,A) + ~ev(Y,A).
ev(X-Y,A) := R :- !, R = ~ev(X,A) - ~ev(Y,A).
ev(X*Y,A) := R :- !, R = ~ev(X,A) * ~ev(Y,A).
ev(X<<Y,A) := R :- !, R = ~ev(X,A) << ~ev(Y,A).
ev(X>>Y,A) := R :- !, R = ~ev(X,A) >> ~ev(Y,A).
ev(ashr(X,Y),A) := R :- !, R = ashr(~ev(X,A), ~ev(Y,A)).
ev(X/\Y,A) := R :- !, R = ~ev(X,A) /\ ~ev(Y,A).
ev(X\/Y,A) := R :- !, R = ~ev(X,A) \/ ~ev(Y,A).
ev(X#Y,A) := R :- !, R = ~ev(X,A) # ~ev(Y,A).
ev(X=Y,A) := R :- !, R = (~ev(X,A) = ~ev(Y,A)).
ev(X\=Y,A) := R :- !, R = (~ev(X,A) \= ~ev(Y,A)).
ev(uge(X,Y),A) := R :- !, R = uge(~ev(X,A), ~ev(Y,A)).
ev(ug(X,Y),A) := R :- !, R = ug(~ev(X,A), ~ev(Y,A)).
ev(ul(X,Y),A) := R :- !, R = ul(~ev(X,A), ~ev(Y,A)).
ev(ule(X,Y),A) := R :- !, R = ule(~ev(X,A), ~ev(Y,A)).
ev(X>=Y,A) := R :- !, R = (~ev(X,A) >= ~ev(Y,A)).
ev(X>Y,A) := R :- !, R = (~ev(X,A) > ~ev(Y,A)).
ev(X<Y,A) := R :- !, R = (~ev(X,A) < ~ev(Y,A)).
ev(X=<Y,A) := R :- !, R = (~ev(X,A) =< ~ev(Y,A)).
ev(ite(X,Then,Else),A) := R :- !, R = ite(~ev(X,A), ~ev(Then,A), ~ev(Else,A)). % (internal, for "cmov")
ev(X, _) := _ :- throw(error(unknown_expr(X), ev/3)).

% ---------------------------------------------------------------------------
:- doc(section, "Command evaluation (non-speculative)").

%:- export(run/2).
run(Conf, Timeout) := Conf :- Timeout =< 0, !,
	trace(timeout).
run(Conf, Timeout) := Conf2 :-
	( stop(Conf) -> Conf2 = Conf
	; tracepc(Conf),
	  Conf1 = ~run1(Conf),
	  Timeout1 is Timeout - 1,
	  Conf2 = ~run(Conf1, Timeout1)
	).

% Add raw PC to the trace (just for readability in print_trace)
tracepc(c(_,A)) :- trace(~pc(A)).

% TODO: pc|->bottom needed?
stop(c(_,A)) :- \+ _ = ~p(~pc(A)).

run1(Conf) := Conf2 :-
	Conf = c(_,A),
	Conf2 = ~run_(~p(~pc(A)),Conf).

%run_(I,C) := _ :- display(i(I,C)), nl, fail.
% Skip
run_(skip,c(M,A)) := c(M,A2) :-
	A2 = ~update0(A,pc,~incpc(A)).
% Barrier
run_(spbarr,c(M,A)) := c(M,A2) :-
	A2 = ~update0(A,pc,~incpc(A)).
% Assign
run_(X<-E,c(M,A)) := c(M,A2) :-
	symtmp(~ev(E,A), Tmp),
	A1 = ~update(A,X,Tmp),
	A2 = ~update0(A1,pc,~incpc(A1)).
% ConditionalUpdate (depending on the contition EP, it does 1 or 2)
run_(cmov(EP,X<-E),C0) := C :- !,
	C = ~run_(X<-ite(~bv(EP),E,X), C0).
% Load
run_(load(X,E),c(M,A)) := c(M,A2) :-
	N = ~ev(E,A),
	trace(load(N)),
	V = ~element(M,N),
	A1 = ~update(A,X,V),
	A2 = ~update0(A1,pc,~incpc(A1)).
% Store
run_(store(X,E),c(M,A)) := c(M2,A2) :-
	Xv = ~ev(X,A),
	Ev = ~ev(E,A),
	M2 = ~update(M,Ev,Xv),
	trace(store(Ev)),
	A2 = ~update0(A,pc,~incpc(A)).
% Beqz-1 and Beqz-2
run_(beqz(X,L),c(M,A)) := c(M,A2) :-
	Xv = ~ev(X,A),
	V = ~conc_cond(Xv=0),
	( V=1 -> L2 = L ; L2 = ~incpc(A) ),
	trace(pc(L2)),
	A2 = ~update0(A,pc,L2).
% Jmp	
run_(jmp(E),c(M,A)) := c(M,A2) :-
	La = ~ev(E,A), % TODO: useful? it will show indirect jumps more easily
	trace(pc(La)),
	L = ~concretize(La), % TODO: make symbolic if E is symbolic
	A2 = ~update0(A,pc,L).

pc(A) := ~concretize(~ev(pc,A)).

incpc(A) := ~concretize(~ev(pc+1,A)).

% make sure that the expression is boolean
bv(A) := A :- nonvar(A), bv_(A), !.
bv(A) := (A\=0).

bv_(_=_).
bv_(_\=_).
bv_(uge(_,_)).
bv_(ug(_,_)).
bv_(ul(_,_)).
bv_(ule(_,_)).
bv_(_>=_).
bv_(_>_).
bv_(_<_).
bv_(_=<_).

% ---------------------------------------------------------------------------
:- doc(section, "Preliminaries for speculative execution").

% Speculative state

% spec(Id,N,L,Conf)
% <id,n,l,sigma>: N x N x Labels x Conf  (SpecS=list of speculative states)
%
%   id: identifier
%   n: remaining speculative window
%   l: label where it started
%   sigma: initial configuration

% Decrement the speculative window of all spec/4
decr([]) := [].
decr([spec(Id,N,L,Conf,GoodL)|S]) := [spec(Id,N1,L,Conf,GoodL)| ~decr(S)] :-
	( N > 0 -> N1 is N - 1; N1 = 0 ).

% Set all speculative windows to 0
zeroes([]) := [].
zeroes([spec(Id,_,L,Conf,GoodL)|S]) := [spec(Id,0,L,Conf,GoodL)| ~zeroes(S)].

enabled([]).
enabled([spec(_Id,N,_L,_Conf,_GoodL)|S]) :- N > 0, enabled(S).

% xc(Ctr,Conf,S)
% <ctr,sigma,s>: N x Conf x SpecS   (ExtConf)
% 
%   ctr: counter for ids
%   sigma: configuration
%   s: list of speculative states

% ---------------------------------------------------------------------------
:- doc(section, "Branch prediction").

% Branch predictor
bp(xc(_Ctr,c(_M,A),_S)) := t(L, N, GoodL) :-
	Ins = ~p(~pc(A)), !, 
	bp_(Ins, A, L, N, GoodL).

:- compilation_fact(mispredict).
:- if(defined(mispredict)).
% Beqz-1 and Beqz-2
bp_(beqz(X,L),A,L2,N,GoodL2) :-
	reg(X),
	Xv = ~ev(X,A),
	V = ~conc_cond(Xv=0),
	% Miss-prediction all the time
	( V=0 -> L2 = L ; L2 = ~incpc(A) ),
	( V=1 -> GoodL2 = L ; GoodL2 = ~incpc(A) ), % (keep it)
	get_window_size(N).
% Jmp	
bp_(jmp(E),A,L,N,L) :-
	Ev = ~ev(E,A),
	L = ~concretize(Ev), N = 1.
:- else.
% Beqz-1 and Beqz-2
bp_(beqz(X,L),A,L2,N,L2) :-
	reg(X),
	Xv = ~ev(X,A),
	V = ~conc_cond(Xv=0),
	% Good prediction
	( V=1 -> L2 = L ; L2 = ~incpc(A) ),
	N = 1. % (irrelevant if we predict correctly)
% Jmp	
bp_(jmp(E),A,L,N,L) :-
	Ev = ~ev(E,A),
	L = ~concretize(Ev), N = 1.
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Speculative execution").

% NOTE: in S we keep the reverse order

%:- export(xrun/2).
xrun(Conf, Timeout) := Conf :- Timeout =< 0, !,
	trace(timeout).
xrun(xc(Ctr,Conf,S), Timeout) := XC2 :-
	( stop(Conf), S = [] -> XC2 = xc(Ctr,Conf,S) % Note: S=[] (so that spec continue decr otherwise)
	; XC1 = ~xrun1(xc(Ctr,Conf,S)),
	  Timeout1 is Timeout - 1,
	  XC2 = ~xrun(XC1,Timeout1)
	).

% Se-NoJump & Se-ExBarrier
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
	enabled(S),
	\+ _ = ~bp(xc(Ctr,Conf,S)),
	!,
	( stop(Conf) -> Conf2 = Conf % Note: case for stop/1 (so that spec continue decr)
	; tracepc(Conf),
	  Conf2 = ~run1(Conf)
	),
	( Conf = c(_M,A), spbarr = ~p(~pc(A)) ->
	    S2 = ~zeroes(S)
	; S2 = ~decr(S)
	),
	XC2 = xc(Ctr,Conf2,S2).
% Se-Jump
xrun1(xc(Ctr,Conf,S)) := XC2 :-
	enabled(S),
	tracepc(Conf),
	t(L,N,GoodL) = ~bp(xc(Ctr,Conf,S)),
	!,
	trace(start(Ctr)), trace(pc(L)),
	Conf = c(M,A),
	Conf2 = c(M,~update0(A,pc,L)), % TODO: it was mset/3 before
	Ctr1 is Ctr + 1,
	XC2 = xc(Ctr1,Conf2,[spec(Ctr,N,L,Conf,GoodL) | ~decr(S)]).
% Se-Commit
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
	append(SPrime, [spec(Id,0,L,_ConfPrime,GoodL)|S2], S),
	enabled(SPrime),
	L = GoodL,
	!,
	tracepc(Conf),
	trace(commit(Id)),
	XC2 = xc(Ctr,Conf,~append(SPrime,S2)).
% Se-Rollback-1
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
	S = [spec(Id,0,L,ConfPrime,GoodL)|S2],
	\+ L = GoodL,
	!,
	tracepc(Conf),
	trace(rollback(Id)),
	ConfPrime = c(M,A0), A = ~update0(A0,pc,GoodL),
	trace(pc(GoodL)),
	XC2 = xc(Ctr,c(M,A),S2).
% Se-Rollback-2
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
	Spec = spec(_Id,0,L,_ConfPrime,GoodL),
	append(SPrime, [Spec|S2], S),
	\+ SPrime = [],
	enabled(SPrime),
	\+ L = GoodL,
	!,
	XC2 = xc(Ctr,Conf,~append(~zeroes(SPrime),[Spec|S2])).

