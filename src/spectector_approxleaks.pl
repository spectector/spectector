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

:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

:- doc(title, "Spectector Approximate Counting").

:- use_module(library(global_vars)).
:- use_module(library(arrays)).
:- use_module(library(sort)).
:- use_module(library(hiordlib)).
:- use_module(library(aggregates)).
:- use_module(library(streams)).
:- use_module(library(listing)).
:- use_module(library(random)).
:- use_module(library(format)).

:- use_module(library(lists)).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(write)).
:- use_module(library(dict)).
:- use_module(library(stream_utils)).
:- use_module(library(streams)).
:- use_module(library(pathnames)).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(library(read)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(terms_io), [file_to_terms/2]).
:- use_module(engine(basic_props), [num/1]).

:- use_module(muasm_semantics).
:- use_module(muasm_program).
:- use_module(muasm_print).
:- use_module(spectector_flags).
:- use_module(concolic(ciaosmt)).
:- use_module(concolic(concolic), [pathgoal/2, sym_filter/2, conc_stats/3, set_nextpath_timeout/1]).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(library(terms_vars)).

:- data time_control/1.
:- data time_data/1.
:- data time_trace/1.
:- data termination/1.
:- data data_check/1.
:- data control_check/1.


% code to find the median of a list
findMedian([H|_],0,H):- !.
findMedian([_|T],C,X):-
    C1 is C-1,
    findMedian(T,C1,X).

% unify the median of L to x
median(L,X):-
    sort(L,SortedL),
    length(SortedL,Len),
    Len2 is Len//2,
    findMedian(SortedL,Len2,X).

% create random list X of booleans with length len (I*J in our case)
createAlfalist(Len, X, RandomCounter):-
    %log_message('INSIDE CREATE ALFA:'),
    %log_message(Len),
    ( (0 < Len) ->
        srandom(~round(2**(RandomCounter+Len))),
        createAlfalist(~(Len-1), CurrX, RandomCounter),
        A = ~random(0,1),
        X=[A|CurrX]
    ;
    X=[]
    ).
    

createHashList(Len, X, RandomCounter):-
    %log_message('INSIDE CREATE HASH:'),
    %log_message(Len),
    ( (0 < Len) ->
        srandom(~round(2**(RandomCounter+Len))),
        createHashList(~(Len-1), CurrX, RandomCounter),
        Max = ~round(2**64),
        A = ~round(~random(0.0,Max)),
        %log_message('A:'),
        %log_message(A),
        X=[A|CurrX]
    ;
    X=[]
    ). 



hash_constraints([A|Hash], [Al|Alfa], LeakingLocation, [HG|HashGoal]) :-
    %log_message('starting hash'),
    assert_fun(sum_bits(Input::bitvec(64))::bitvec(1),
	   let([T32=extract(31,0,Input)#extract(63,32,Input)],
	      let([T16=extract(15,0,T32)#extract(31,16,T32)],
	          let([T8=extract(7,0,T16)#extract(15,8,T16)],
	              let([T4=extract(3,0,T8)#extract(7,4,T8)],
	                  let([T2=extract(1,0,T4)#extract(3,2,T4)],
	                      extract(0,0,T2)#extract(1,1,T2))))))),
    HG = (sum_bits(LeakingLocation/\bv(A,64)) = bv(Al,1)),
    %log_message('hashing :'),
    %log_message(bv(A, 64)),
    %log_message('HG:'),
    %log_message(HG),
    hash_constraints(Hash, Alfa, LeakingLocation, HashGoal).


hash_constraints([], [], _, []).
    %log_message('empty').

is_list(X) :-
        var(X), !,
        fail.
is_list([]).
is_list([_|T]) :-
        is_list(T).

exclude_already_found_goal([A|AlreadyFound], LeakingLocation, [G|Goal]) :-
	G = (LeakingLocation \= bv(A, 64)),
    %log_message('excluding :'),
    %log_message(bv(A, 64)),
    %log_message(AlreadyFound),
	exclude_already_found_goal(AlreadyFound, LeakingLocation, Goal).

exclude_already_found_goal([], _, []).

% Start with algorithm 3 as in the paper, because Algorithm 1 & 2 are the original oens from math paper
% Algorithm 3
:- export(approxleaks/4).
approxleaks(Epsilon,Teta,Low,C0):-
    srandom(562),
    log_message('STARTING APPROX'),
    Pivot is ~(round(2*(3*exp(0.5)*((1+(1/Epsilon))**2)))),
    T is ~(35*log(3/Teta)),
    log_message('Calling APPROXleaks_'),
    approxleaks_(Low, C0, 0, [], Pivot, T, [], Leaks).

% we use a help function
approxleaks_(Low, C0, Counter, L, Pivot, T, AlreadyFoundLeakage, Leaks ):-
    %log_message('Step'),
    ( (Counter < T) ->
        %log_message('Step'),
        approxleaksCore(Pivot, C, Low, C0, AlreadyFoundLeakage, Leaks, Counter),
        log_message('CORE FINISHED'),
        log_message(C),
        %log_message('Step'),
        %log_message(Leaks),
        %log_message('Already:'),
        %log_message(AlreadyFoundLeakage),
        %log_message('comparing'),
        ((C > -1) -> 
            CurrL = [C|L]
            ;
            CurrL=L),
        
        NewCounter = ~(Counter+1),
        log_message('COUNTER'),
        log_message(NewCounter),
        approxleaks_(Low, C0, NewCounter, CurrL, Pivot, T, Leaks, NewLeakageList)
    ;
        log_message('Leaks found:'),
        log_message(AlreadyFoundLeakage),
        log_message('getting final count'),
        log_message(L),
        median(L,X),
        log_message(X)
    ).
    

 
% Algorithm 4
approxleaksCore(Pivot, C, Low, C0, AlreadyFoundLeakage, Leaks, RandomCounter):-
    Bound = ~(Pivot+1),
    log_message('-c0'),
    log_message(C0),
    copy_term(C0, C1),
    boundedLeaksEnumeration(0, Bound, N, Low, C1, C0, AlreadyFoundLeakage, NewLeaks),
    log_message('insideCore N is:'),
    log_message(N),
    ( ( N =< Pivot) -> 
        Leaks = NewLeaks,
        C is N % stop here & return
    ;
        log_message('starting with CoreLoop'),
        L = ~round(~(log(Pivot)-1)),% we need lower round here       
        approxleaksCoreLoop(L , L, Pivot, Low, C0, NewLeaks, Leaks, C, RandomCounter)
    ).   
    
approxleaksCoreLoop(InitialI, L, Pivot, Low, C0, AlreadyFoundLeakage, Leaks, C, RandomCounter):-
        %log_message('initialI:'),
        %log_message(InitialI),
        %log_message('L:'),
        %log_message(L),
        I = ~(InitialI+1),
        %log_message('I is:'),
        %log_message(I),
        %log_message('creating lists!'),
        % n (in the algorithm) here is the length of memory so 64bits
        % this creates the random list of lists for function H from Hxor(64,i−l,3)
        %log_message(~(I-L)),
        createAlfalist(~(I-L), Alfa, RandomCounter ), 
        %log_message('created ALFA!'),
        createHashList(~(I-L),A, RandomCounter), 
        %log_message('created HashList'),
        Bound = ~(Pivot+1),
        %log_message('BOUND is ready'),
        copy_term(C0, C1),
        log_message('called BoundedLeaks_'),
        boundedLeaksEnumeration_(A, Alfa, 0, Bound, N, Low, C1, C0, AlreadyFoundLeakage, NewLeaks),
        log_message('DONE WITH BOUNDED_ and N is:'),
        log_message(N),
        % a question here : 1≤N≤pivot or (i=n) is N or n ???
        ( ((1 =< N) , (N =< Pivot) ; (N =:= I)) ->
            %log_message(' AND I here is:'),
            %log_message(I),
            approxleaksCoreLoop(I, L, Pivot, Low, C0, NewLeaks, Leaks, C, RandomCounter) 
        ;
        Leaks = NewLeaks,
        %log_message('Step in core loop'),
        %log_message('NewN :'),
        log_message(N),
        ( ((N > Pivot) ; (N =:= 0)) -> 
            C = -1 
        ; 
            C = ~(N*(2**(I-L)))
        ),
        log_message('C :'),
        log_message(C)

        ).

        
% Algorithm 5 the version without Alpha & a
boundedLeaksEnumeration(Count, Bound, N, Low, C1, C2, AlreadyFoundLeakage, LeakageList):-
      copy_term(C2, C0),
      log_message(Count),
      log_message(' Count vs Bound :'),
      log_message(Bound),
      
      ( (Count < Bound) -> 
        ( boundedLeak_Check_Goal(Low, C0,  C2, N,  Count, Bound, AlreadyFoundLeakage, LeakageList) ->
            true
            ;
            log_message('no leaks'),
            N = ~(Count+0),
            LeakageList=AlreadyFoundLeakage)
    ; 
            log_message('[Count is bigger then Bound]'),
            N = ~(Count+0),
            LeakageList=AlreadyFoundLeakage
        ).
       

% Loop inside boundedLeaksEnumeration
boundedLeak_Check_Goal(Low, C0, C2, N, Count, Bound, AlreadyFoundLeakage, LeakageList):-
    %log_message('here'),
    log_message(C0),
    concrun(C0, (_, Trace)),   
    %log_message('get next path'),
    C0 = xc(_, C0a, _), 
    
    %log_message('set conf'),
    leak_check_path(Trace, Low, Safe, C0a, AlreadyFoundLeakage, NewLeakage),

    %log_message('runed leak_check_path'),
    ( termination(_) -> 
        log_message('[path is unsafe]'),
        CurrentLeakages = [NewLeakage|AlreadyFoundLeakage],
        NewCount = ~(Count+1),
        boundedLeaksEnumeration(NewCount, Bound ,N , Low, C0, C2, CurrentLeakages, LeakageList)
        
    ;
        log_message('[path is safe]'),
        CurrentLeakages = AlreadyFoundLeakage,
        fail
        %copy_term(C2, C1),
        %boundedLeak_Check_Goal(Low, C0, C2, N,  Count, Bound, CurrentLeakages, LeakageList)
        
    ).

% Normal version without Crypto

leak_check_path(Trace, Low, Safe, C0a, AlreadyFoundLeakage, NewLeak) :-
    log_message('[checking presence of leak]'),
    ( leak_check_path_mode_specific(data, Trace, Low, C0a, AlreadyFoundLeakage, NewLeak) ->
        log_message('[found data leak]'),
        Safe = data,
        set_fact(termination(data))
    ; leak_check_path_mode_specific(control, Trace, Low, C0a, AlreadyFoundLeakage, NewLeak) ->
        log_message('[found control leak]'),
        Safe = control,
        set_fact(termination(control))
    ; Safe = yes
    ).

% Check leakage for a given trace
leak_check_path_mode_specific(Mode, TraceA0, Low, C0a, AlreadyFoundLeakage, NewLeak) :-
  
    erase_constraints(C0a, InGoalA, _),
    erase_constraints(TraceA0, _, _), 
    
    log_message('erased'),
    ExcludeAlreadyFoundGoal = ~exclude_already_found_goal(AlreadyFoundLeakage, LeakingLocation),
    log_message('excluded'),
    ( Mode = data ->
        % Data-based leak
        TraceA = TraceA0,
        rename_symspec(Low,
                       C0a, InGoalA, TraceA, [],
                       C0b, InGoalB, TraceB, [],
                       LowGoal),
        differdisj(TraceA, TraceB, OrCond),
        \+ OrCond = [], % (just fail)
        % DiffGoal = [~or_cond(OrCond)], % TODO: fixme, allow \/ on booleans
        DiffGoal = [~or_cond(OrCond) = bv(1,64)]
    ; Mode = control ->
        % Control-based leak
        % (nondet)
        select_spec_cond(TraceA0, TraceA, CondA), % select cond/1 in speculative fragments
        rename_symspec(Low,
                       C0a, InGoalA, TraceA, CondA,
                       C0b, InGoalB, TraceB, CondB,
                       LowGoal),
        NegCondB = ~negbool(CondB), DiffGoal = [CondA,NegCondB],
        WhereA = [sym(cond(CondA))], WhereB = [sym(cond(NegCondB))]
    ; throw(unknown_mode(Mode))
    ),

    C0a = c(Ma, _),
    C0b = c(Mb, _),

    NewLowGoal=[forall([Location::bitvec(64)], (Location \= LeakingLocation -> select(Ma, Location) = select(Mb, Location)))],

	PathGoal = ~append(InGoalA,
	             ~append(~pathgoal(~sym_filter(TraceA)),
	               ~append(InGoalB, ~pathgoal(~sym_filter(TraceB))))),
	
    log_message('setting constrains'),
	add_constraints(PathGoal),
	add_constraints(DiffGoal),
	add_constraints(LowGoal),
    add_constraints(ExcludeAlreadyFoundGoal),
	add_constraints(NewLowGoal),
	log_message('constraints added, starting solving'),
    SolverTO= -1,
    set_solver_opt(timeout, SolverTO), %-1 NoninterMax
	try_solve(Status),
	log_message('solved !'),
	(Status = sat ->
		log_message('SAT !'),
		erase_constraints([LeakingLocation], _, Model),
		Model=[=(_, LeakingLocationValue)],
        % unassign([X], Model),
		format("0x~16r is leaking\n", LeakingLocationValue),
		NewLeak=LeakingLocationValue

	; Status = unknown ->
		log_message('Unknown'),
		fail
	;
		log_message('UNSAT !'),
		fail
	).


% Algorithm 5 with alpha & the crypto funcion 
boundedLeaksEnumeration_(A, Alfa, Count, Bound, N, Low, C1, C2, AlreadyFoundLeakage, LeakageList):-
    copy_term(C2, C0),
    log_message(Count),
    log_message(' Count vs Bound :_'),
    log_message(Bound),
    %log_message('ALREADY FOUND_:'),
    %log_message(AlreadyFoundLeakage),
    ( (Count < Bound) -> 
        ( boundedLeak_Check_Goal_(A, Alfa, Low, C0,  C2, N,  Count, Bound, AlreadyFoundLeakage, LeakageList) ->
            true
            ;
            log_message('no leaks'),
            N = ~(Count+0),
            LeakageList=AlreadyFoundLeakage)
    ; 
            log_message('[Count is bigger then Bound]'),
            N = ~(Count+0),
            %log_message('fuck this shit'),
            LeakageList=AlreadyFoundLeakage
            %log_message('DONE!')
    ).  
    
    

% loop inside boundedLeaksEnumeration_
boundedLeak_Check_Goal_(A, Alfa, Low, C0, C2, N, Count, Bound, AlreadyFoundLeakage, LeakageList):-
    %log_message('here_'),
    log_message(C0),
    concrun(C0, (_, Trace)),   
    %log_message('get next path'),
    C0 = xc(_, C0a, _), 
    %log_message('set conf'),
    leak_check_path_(A, Alfa, Trace, Low, Safe, C0a, AlreadyFoundLeakage, NewLeakage),
    %log_message('runed leak_check_path'),
    ( termination(_) -> 
        log_message('[path is unsafe]'),
        CurrentLeakages = [NewLeakage|AlreadyFoundLeakage],
        NewCount = ~(Count+1),
        boundedLeaksEnumeration_(A, Alfa, NewCount, Bound ,N , Low, C0, C2, CurrentLeakages, LeakageList)
    ;
        log_message('[path is safe]'),
        CurrentLeakages = AlreadyFoundLeakage,
        copy_term(C2, C1),
        boundedLeak_Check_Goal_(A, Alfa, Low, C1, C2, N,  Count, Bound, CurrentLeakages, LeakageList)
        ).


leak_check_path_(A, Alfa, Trace, Low, Safe, C0a, AlreadyFoundLeakage, NewLeak) :-
    log_message('[checking presence of leak]'),
    ( perform_data, leak_check_path_mode_specific_(A, Alfa, data, Trace, Low, C0a, AlreadyFoundLeakage, NewLeak) ->
        log_message('[found data leak]'),
        Safe = data,
        set_fact(termination(data))
    ; perform_control, leak_check_path_mode_specific_(A, Alfa, control, Trace, Low, C0a, AlreadyFoundLeakage, NewLeak) ->
        log_message('[found control leak]'),
        Safe = control,
        set_fact(termination(control))
    ; Safe = yes
    ).

    
leak_check_path_mode_specific_(A, Alfa, Mode, TraceA0, Low, C0a, AlreadyFoundLeakage, NewLeak) :-
    erase_constraints(C0a, InGoalA, _),
    erase_constraints(TraceA0, _, _), 
    %log_message('erased'),
    ExcludeAlreadyFoundGoal = ~exclude_already_found_goal(AlreadyFoundLeakage, LeakingLocation),
    %log_message('excluded'),
    HashGoal= ~hash_constraints(A, Alfa, LeakingLocation),
    %log_message('HashGoal is ready'),
    ( Mode = data ->
        % Data-based leak
        TraceA = TraceA0,
        rename_symspec(Low,
                       C0a, InGoalA, TraceA, [],
                       C0b, InGoalB, TraceB, [],
                       LowGoal),
        differdisj(TraceA, TraceB, OrCond),
        \+ OrCond = [], % (just fail)
        % DiffGoal = [~or_cond(OrCond)], % TODO: fixme, allow \/ on booleans
        DiffGoal = [~or_cond(OrCond) = bv(1,64)]
    ; Mode = control ->
        % Control-based leak
        % (nondet)
        select_spec_cond(TraceA0, TraceA, CondA), % select cond/1 in speculative fragments
        rename_symspec(Low,
                       C0a, InGoalA, TraceA, CondA,
                       C0b, InGoalB, TraceB, CondB,
                       LowGoal),
        NegCondB = ~negbool(CondB), DiffGoal = [CondA,NegCondB],
        WhereA = [sym(cond(CondA))], WhereB = [sym(cond(NegCondB))]
    ; throw(unknown_mode(Mode))
    ),

    C0a = c(Ma, _),
    C0b = c(Mb, _),

    NewLowGoal=[forall([Location::bitvec(64)], (Location \= LeakingLocation -> select(Ma, Location) = select(Mb, Location)))],

	PathGoal = ~append(InGoalA,
	             ~append(~pathgoal(~sym_filter(TraceA)),
	               ~append(InGoalB, ~pathgoal(~sym_filter(TraceB))))),
	
    %log_message('setting constrains_'),
	add_constraints(PathGoal),
	add_constraints(DiffGoal),
    add_constraints(HashGoal),
	add_constraints(LowGoal),
    add_constraints(ExcludeAlreadyFoundGoal),
	%add_constraints(NewLowGoal),
	%log_message('constraints added, starting solving_'),
    set_solver_opt(timeout, 5000),
	try_solve(Status),
	log_message('solved !'),
	(Status = sat ->
		log_message('SAT !'),
		erase_constraints([LeakingLocation], _, Model),
		Model=[=(_, LeakingLocationValue)],
        % unassign([X], Model),
		format("0x~16r is leaking\n", LeakingLocationValue),
		NewLeak=LeakingLocationValue

	; Status = unknown ->
		log_message('Unknown'),
        NewLeak=0,
		fail
	;
		log_message('UNSAT !'),
        NewLeak=0,
		fail
	).

% adding the code from Spectector_noninter


% Rest normal code




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

% Obtain trace prefixes before a C cond(_) during speculation (fail if
% there are no more prefixes)
select_spec_cond([start(I)|Xs], Trace, C) :-
    append(Spec, [End|Rest], Xs),
    End = rollback(I), % only rollbacks
    !,
    ( % obtain cond and drop the rest of spec
      append(BeforeC, [sym(cond(C))|AfterC], Spec),
      Trace = ~append([start(I)|BeforeC], Trace0),
%      Trace0 = ~only_specmarks(~append(AfterC, Rest)) % drop all (wrong, not enough)
      Trace0 = ~append(~only_specmarks(AfterC), [End|Rest]) % drop spec (it should be fine)
%      Trace0 = ~append(AfterC, [End|Rest]) % drop spec (spurious during spec, is it fine?)
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

:- export(trace_length/2).
trace_length(Xs, N) :-
    trace_length_(Xs, 0, N).

trace_length_([], N, N).
trace_length_([X|Xs], N0, N) :-
    ( X = sym(cond(_)) ->
        N1 is N0 + 1
    ; X = load(_) ->
        N1 is N0 + 1
    ; X = store(_) ->
        N1 is N0 + 1
    ; N1 = N0
    ),
    trace_length_(Xs, N1, N).

all_conc_stats_unknown :-
    findall(St, conc_stats(_,_,St), L),
    L \= [],
    all_conc_stats_unknown_(L).

all_conc_stats_unknown_([unknown|L]) :- all_conc_stats_unknown_(L).
all_conc_stats_unknown_([]).


% ---------------------------------------------------------------------------
% (log messages)

:- use_module(engine(messages_basic), [message/2]).

log_message(X) :- message(user, X).

