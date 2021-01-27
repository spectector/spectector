% Copyright 2018 The Spectector authors
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% ===========================================================================

:- module(_, [], [assertions, fsyntax, datafacts, hiord]).

:- doc(title, "µAsm semantics").

:- use_module(library(lists)).
:- use_module(muasm_program).
:- use_module(spectector_flags).
:- use_module(concolic(concolic)).
:- use_module(concolic(ciaosmt)).
:- use_module(spectector_stats).
:- use_module(engine(messages_basic), [message/2]).

% ---------------------------------------------------------------------------
:- doc(section, "Evaluation (both spec and non-spec)").

:- export(mrun/2).
% Run c/2 or xc/3 configurations
mrun(C0) := CT :-
    with_trace(Trace, mrun_(C0,C)),
    CT = (C,Trace).

% Run c/2 or xc/3 configurations
mrun_(C0) := ~run(C0, ~get_limit(step)) :- C0 = c(_,_), !.
mrun_(C0) := ~xrun(C0, ~get_limit(step)) :- C0 = xc(_,_,_), !.

:- export(new_c/3).
new_c(M,A) := c(~new_array(bitvec(64), bitvec(64), M),
                ~new_array(regs, bitvec(64), A)). % TODO: 'regs' type is dummy here

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
    conc_call(mrun_(C0,C), InConf, Trace), % TODO: Produce stats if the call fails
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
ev(pc,A) := R :- !, R = ~element(A,pc).
ev(X,_A) := R :- loc(X,N0), !,
    % TODO: resolve symbol location statically instead?
    ( integer(N0) -> R = bv(N0,64) ; R = N0 ).
ev(X,A) := R :- reg(X), !, R = ~element(A,X).
ev(N,_A) := bv(N,64) :- integer(N), !.
%ev(N,_A) := N :- integer(N), !.
ev(+X,A) := ~ev(X,A) :- !.
ev(-X,A) := R :- !, R = -(~ev(X,A)).
ev(X+Y,A) := R :- !, R = ~ev(X,A) + ~ev(Y,A).
ev(X-Y,A) := R :- !, R = ~ev(X,A) - ~ev(Y,A).
ev(X*Y,A) := R :- !, R = ~ev(X,A) * ~ev(Y,A).
ev(X/Y,A) := R :- !, R = ~ev(X,A) // ~ev(Y,A).
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
run(Conf, Timeout) := Conf1 :- Timeout =< 0, !,
    Conf1= ~run_(stop_ins,Conf).

run(Conf, Timeout) := Conf2 :-
    ( stop(Conf) -> Conf2 = Conf
    ; trace_rawpc(Conf),
      track_ins(Conf),
      Conf1 = ~run1(Conf),
      Timeout1 is Timeout - 1,
      Conf2 = ~run(Conf1, Timeout1)
    ).

% Add raw PC to the trace (just for readability in print_trace)
trace_rawpc(c(_,A)) :- trace(~pc(A)).

% TODO: pc|->bottom needed?
stop(c(_,A)) :- \+ _ = ~p(~pc(A)).

run1(Conf) := Conf2 :-
    Conf = c(_,A),
    Conf2 = ~run_(~p(~pc(A)),Conf).

%run_(I,C) := _ :- display(user_error, i(I,C)), nl(user_error), fail.
% Skip
run_(skip,c(M,A)) := c(M,A2) :-
    A2 = ~update(A,pc,bv(~incpc(A),64)).
% Stop instruction
run_(stop_ins,c(M,A)) := c(M,A2) :-
    A2 = ~update(A,pc,bv(-1,64)). % TODO: Standard site to jump?
% Unknown
run_(unsupported_ins(I),C0) := C :-
    message(warning, ['Pass through an unsupported instruction! ', I]),
    increment_unsupported_instructions,
    ( skip_unsupported ->
        C = ~run_(skip,C0)
    ; C = ~run_(stop_ins,C0)
    ).
run_(unknown_pc(L),C0) := C :-
    message(warning, ['Pass through a non declared Label! ', L]), % TODO: inefficient! (remember somewhere else)
    new_unknown_label(string(~atom_codes(L))),
    C = ~run_(stop_ins,C0).
run_(indirect_jump(L),C0) := C :-
    message(warning, ['Pass through an indirect jump, register: ', L]), % TODO: inefficient! (remember somewhere else)
    new_indirect_jump(string(~atom_codes(L))),
    C = ~run_(stop_ins,C0).
% 
% Barrier
run_(spbarr,c(M,A)) := c(M,A2) :-
    A2 = ~update(A,pc,bv(~incpc(A),64)).
% Assign
run_(X<-E,c(M,A)) := c(M,A2) :-
    Ev = ~ev(E,A),
    ( is_const_exp(Ev) ->
        Tmp = Ev
    ; is_bool_exp(Ev) ->
        % Make sure that E is bitvec(64)
        assign(Tmp, ite(Ev,bv(1,64),bv(0,64)))
    ; assign(Tmp, Ev)
    ),
    A1 = ~update(A,X,Tmp),
    A2 = ~update(A1,pc,bv(~incpc(A1),64)).
% ConditionalUpdate (depending on the contition EP, it does 1 or 2)
run_(cmov(EP,X<-E),C0) := C :- !,
    C = ~run_(X<-ite(~ensure_bool(EP),E,X), C0).
% Load
run_(load(X,E),c(M,A)) := c(M,A2) :-
    N = ~ev(E,A),
    trace(load(~bv2int(N))),
    V = ~element(M,N),
    ( weak_sni -> trace(value(V))
    ; true
    ),
    ( integer(X) -> X1=bv(X,64) % TODO: use ev(X,A) for regs instead?
    ; X1=X
    ),
    A1 = ~update(A,X1,V),
    A2 = ~update(A1,pc,bv(~incpc(A1),64)).
% Store % TODO: Notify about possible injection on stack (return direction)
run_(store(X,E),c(M,A)) := c(M2,A2) :-
    Xv = ~ev(X,A),
    Ev = ~ev(E,A),
    M2 = ~update(M,Ev,Xv),
    trace(store(~bv2int(Ev))),
    ( weak_sni -> trace(value(~bv2int(Xv)))
    ; true
    ),
    A2 = ~update(A,pc,bv(~incpc(A),64)).
% Beqz-1 and Beqz-2
run_(beqz(X,L),c(M,A)) := c(M,A2) :-
    Xv = ~ev(X,A),
%    V = ~conc_cond(Xv=0),
    V = ~conc_cond(Xv=bv(0,64)),
    ( V=true -> L2 = bv(L,64) ; L2 = bv(~incpc(A),64) ),
    trace_pc(L2), track_branch(L2),
    A2 = ~update(A,pc,L2).
% Jmp   
run_(jmp(E),c(M,A)) := c(M,A2) :-
    La = ~ev(E,A), % TODO: useful? it will show indirect jumps more easily
    trace_pc(La),
    % TODO: make symbolic if E is symbolic -> warning when is symbolic?
    concretize(La), % (make sure that La has a concrete value assigned)
    L = ~bv2int(~eval(La)), % evaluate La
    track_branch(L),
    A2 = ~update(A,pc,bv(L,64)).

pc(A) := L :-
    Ev = ~ev(pc,A),
    concretize(Ev),
    L = ~bv2int(~eval(Ev)).

incpc(A) := L :-
    Ev = ~ev(pc+1,A),
    concretize(Ev),
    L = ~bv2int(~eval(Ev)). % TODO: Numeric labels as pc counters?

% make sure that the expression is boolean
ensure_bool(A) := A :- is_bool_exp(A), !.
ensure_bool(A) := (A\=bv(0,64)). % E.g., for flag registers
%ensure_bool(A) := (A\=0).

% extract integer from bitvec, keep unchanged otherwise
bv2int(X) := Y :- nonvar(X), X=bv(Y0,_), !, Y=Y0.
bv2int(X) := X.
%bv2int(X) := _ :- throw(error(not_bv(X), bv2int/2)).

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
decr([spec(Id,N,L,Conf,GoodL)|S]) := [spec(Id,N1,L,Conf,GoodL)|S] :-
    ( N > 0 -> N1 is N - 1; N1 = 0 ).

% Set all speculative windows to 0
zeroes([]) := [].
zeroes([spec(Id,_,L,Conf,GoodL)|S]) := [spec(Id,0,L,Conf,GoodL)| S].

enabled([]).
enabled([spec(_Id,N,_L,_Conf,_GoodL)|_S]) :- N > 0.

window([]) := N :-
    get_window_size(N).
window([spec(_Id,N,_L,_Conf,_GoodL)|_S]) := N1 :-
    ( N > 0 -> N1 is N - 1; N1 = 0 ).

% xc(Ctr,Conf,S)
% <ctr,sigma,s>: N x Conf x SpecS   (ExtConf)
% 
%   ctr: counter for ids
%   sigma: configuration
%   s: list of speculative states

% ---------------------------------------------------------------------------
:- doc(section, "Branch prediction").

% Branch predictor
bp(xc(_Ctr,c(_M,A),S)) := t(L, N, GoodL) :-
    Ins = ~p(~pc(A)), !, 
    bp_(Ins, A, L, N, GoodL, S).

:- compilation_fact(mispredict).
:- if(defined(mispredict)).
% Beqz-1 and Beqz-2
bp_(beqz(X,L),A,L2,N,GoodL2, S) :-
    reg(X),
    Xv = ~ev(X,A),
%    V = ~conc_cond(Xv=0),
    V = ~conc_cond(Xv=bv(0,64)),
    % Miss-prediction all the time
    ( V=false -> L2 = L ; L2 = ~incpc(A) ),
    ( V=true -> GoodL2 = L ; GoodL2 = ~incpc(A) ), % (keep it)
    N = ~window(S).
:- else.
% Beqz-1 and Beqz-2
bp_(beqz(X,L),A,L2,N,L2, _) :-
    reg(X),
    Xv = ~ev(X,A),
%    V = ~conc_cond(Xv=0),
    V = ~conc_cond(Xv=bv(0,64)),
    % Good prediction
    ( V=true -> L2 = L ; L2 = ~incpc(A) ),
    N = 1. % (irrelevant if we predict correctly)
:- endif.
% Jmp   
bp_(jmp(E),A,L,N,L, _) :-
    Ev = ~ev(E,A),
    L = ~bv2int(~eval(Ev)),
    N = 0.

% ---------------------------------------------------------------------------
:- doc(section, "Speculative execution").

% NOTE: in S we keep the reverse order

%:- export(xrun/2).
xrun(Conf, Timeout) := Conf :- Timeout =< 0, !,
    trace(timeout).
xrun(xc(Ctr,Conf,S), Timeout) := XC2 :-
    ( stop(Conf), S = [] -> XC2 = xc(Ctr,Conf,S) % Note: S=[] (so that spec continue decr otherwise)
    ; XC1 = ~xrun1(xc(Ctr,Conf,S)),
      ( stop(Conf) -> Timeout1 = Timeout
      ; Timeout1 is Timeout-1,
        track_ins(Conf)
      ),
      XC2 = ~xrun(XC1,Timeout1)
    ).

% Se-NoJump & Se-ExBarrier
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
    enabled(S),
    \+ _ = ~bp(xc(Ctr,Conf,S)),
    !,
    ( stop(Conf) -> Conf2 = Conf % Note: case for stop/1 (so that spec continue decr)
    ; trace_rawpc(Conf),
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
    trace_rawpc(Conf),
    t(L,N,GoodL) = ~bp(xc(Ctr,Conf,S)),
    !,
    trace(start(Ctr)), trace_pc(L), track_branch(L),
    Conf = c(M,A),
    Conf2 = c(M,~update(A,pc,bv(L,64))), % TODO: it was mset/3 before
    Ctr1 is Ctr + 1,
    XC2 = xc(Ctr1,Conf2,[spec(Ctr,N,L,Conf,GoodL) | S]).
% Se-Commit
xrun1(xc(Ctr,Conf,S)) := XC2 :-
    S = [spec(Id,0,L,_ConfPrime,GoodL)|S2],
    enabled(S2),
    L = GoodL,
    !,
    trace_rawpc(Conf),
    trace(commit(Id)),
    XC2 = xc(Ctr,Conf,S2).

% Se-Rollback-1
xrun1(xc(Ctr,Conf,S)) := XC2 :- 
    S = [spec(Id,0,L,ConfPrime,GoodL)|S2],
    enabled(S2),
    \+ L = GoodL,
    !,
    trace_rawpc(Conf),
    trace(rollback(Id)),
    ConfPrime = c(M,A0), A = ~update(A0,pc,bv(GoodL,64)),
    trace_pc(GoodL), track_branch(GoodL),
    XC2 = xc(Ctr,c(M,A),S2).

trace_pc(L) :- trace(pc(~bv2int(L))).

% Keep statistic and counters for executed instructions
track_ins(Conf) :-
    inc_executed_ins,
    ( track_all_pc -> Conf = c(_M, A), inc_pc(~pc(A))
    ; true
    ).

% Special case for tracking branch instructions
track_branch(L) :-
    ( track_all_pc -> true
    ; inc_pc(L)
    ). % TODO: Outside of the path (there can be side-effects)
