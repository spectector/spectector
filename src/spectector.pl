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

:- module(_, [], [assertions, fsyntax, dcg]).

:- doc(title, "Spectector").
:- doc(subtitle, "SPECulative deTECTOR").

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

:- use_module(concolic(symbolic), [set_ext_solver/1, get_ext_solver/1]).
:- use_module(muasm_translator(muasm_parser)).
:- use_module(muasm_translator(x86_to_muasm)).

:- use_module(spectector_flags).
:- use_module(spectector_stats).
:- use_module(muasm_semantics).
:- use_module(muasm_program).
:- use_module(muasm_print).
:- use_module(spectector_noninter).

% ---------------------------------------------------------------------------

:- export(main/1).
main(Args) :-
	( parse_args(Args, Opts, Files) ->
	    ( member(help, Opts) ->
	        show_help
	    ; Files = [File] ->
  	        run(File, Opts)
	    ; short_help, halt(1)
	    )
	; short_help, halt(1)
	).

% ---------------------------------------------------------------------------

show_help :-
	write_string(
"Usage: spectector [<options>...] <file>

  -h,--help        Show this help
  -s,--spec        Use speculative semantics (default)
  --solver S       Use symbolic solver S
      Solvers:
        z3:        Z3 SMT (default)
  -n,--nonspec     Use non-speculative semantics
  -w,--window N    Size of speculative window
  --steps N        Execution step limit
  -e,--entry FUNC  Entry point of the program
  --conf-file FILE Read the initial configuration from a file
  -c,--conf CONF   Initial configuration ('c(M,A)')
  -a,--analysis ANA
      Analysis algorithm:
        none:     do nothing (useful to show the input program)
        reach:    reachability using concolic execution
        reach1:   like reach, but stop at first path
        noninter: non-interference check (default)
  --low LOW        Low registers or memory addresses for noninter
  --stats FILE     Show all the statistics on the file passed as
                   an output (in JSON format), to get the results
                   by stdout, the argument should be 'stdout'
  --noinit         Memory sections declared are ignored
  --keep-sym VAR   Ignore the specified variables initialization
  --heap N         Heap memory direction
  --stack STACK    Initial stack values ('stack(sp,bp,return)')
  --term-stop-spec If the final of the program is reached during
                   the speculation, it keeps stuck until
                   speculation ends
  --no-show-conf   Configurations are not printed
  --skip-unknown   Treat unknown instrunctions as 'skip'
  --weak           Check the security condition under the weak
                   specification (values in memory must match)

The input program can be a .muasm file (muAsm), a .asm file (Intel
syntax), or a .s file (gnu assembler).

").

short_help :-
	write_string(
"Unrecognized arguments, use '-h' for help\n"
).

opt('-h', '--help', As, As, [help]).
opt('-n', '--nospec', As, As, [nospec]).
opt('-s', '--spec', As, As, [spec]).
opt('', '--noinit', As, As, [noinit]).
opt('', '--solver', [Solver|As], As, [solver(Solver)]).
opt('', '--conf-file', [ConfFile|As], As, [conf_file(ConfFile)]).
opt('-c', '--conf', [ConfAtm|As], As, [Opt]) :-
	atom_codes(ConfAtm, ConfStr),
	read_from_string_atmvars(ConfStr, Conf),
	( Conf = c(M,A) -> true
	; throw(wrong_conf(ConfAtm))
	),
	Opt = c(M,A).
opt('', '--stack', [StackAtm|As], As, [Opt]) :-
	atom_codes(StackAtm, StackStr),
	read_from_string_atmvars(StackStr, Stack),
	( Stack = stack(B,S,R) -> true
	; throw(wrong_stack(StackAtm))
	),
	Opt = stack(B,S,R).
opt('-w', '--window', [NAtm|As], As, [Opt]) :-
	atom_codes(NAtm, NStr),
	number_codes(N, NStr),
	Opt = window(N).
opt('-e', '--entry', [Entry|As], As, [entry(Entry)]). % TODO: Setup for numeric entry points?
opt('', '--steps', [NAtm|As], As, [Opt]) :-
	atom_codes(NAtm, NStr),
	number_codes(N, NStr),
	Opt = step(N).
opt('', '--heap', [NAtm|As], As, [Opt]) :-
	atom_codes(NAtm, NStr),
	number_codes(N, NStr),
	Opt = heap(N).
opt('-a', '--analysis', [Ana|As], As, [ana(Ana)]).
opt('', '--keep-sym', [IgnAtm|As], As, [keep_sym(Ign)]) :-
	atom_codes(IgnAtm, IgnStr),
	read_from_string_atmvars(IgnStr, Ign).
opt('', '--low', [LowAtm|As], As, [low(Low)]) :-
	atom_codes(LowAtm, LowStr),
	read_from_string_atmvars(LowStr, Low).
opt('-r', '--reduce', As, As, [reduce]).
opt('', '--term-stop-spec', As, As, [term_stop_spec]).
opt('', '--weak', As, As, [weak]).
opt('', '--stats', [StatsOut|As], As, [stats(StatsOut)]).
opt('', '--no-show-conf', As, As, [no_show_conf]).
opt('', '--skip-unknown', As, As, [skip_unknown]).

parse_args([Arg|Args], Opts, File) :-
	( opt(Arg, _, Args, Args0, OptsA) % short
	; opt(_, Arg, Args, Args0, OptsA) % long
	),
	!,
	append(OptsA, Opts0, Opts),
	parse_args(Args0, Opts0, File).
parse_args([F|Args], Opts, [F|Files]) :- !,
	parse_args(Args, Opts, Files).
parse_args([], [], []).

% ---------------------------------------------------------------------------

% TODO: add more options:
%   - allow max_paths (max number of explored paths) -> Use the flag
:- export(run/2).
run(PrgFile, Opts) :-
	path_split(PrgFile, Path, PrgNameExt),
	path_splitext(PrgNameExt, _PrgBasename, Ext),
	( ConfContents = ~file_to_terms(~get_conf_file(Opts,Path))
	; ConfContents = []
	),
	Options = ~flatten([Opts, ConfContents]),
	%
	( member(nospec, Opts) -> SpecOpt = nospec
	; SpecOpt = spec % (default)
	),
	% Initial configurations
	extract_query(c(M0,A0), Options, [[],[]]),
	% Specification of the analysis
	extract_query(ana(Ana0), Options, [noninter]),
	% Set up heap direction
	extract_query(heap(HeapDir), Options, [1024]),
	% Ignore specified variable initializations
	extract_query(keep_sym(KeepS), Options, [[]]),
	% Entry point
	extract_query(entry(Entry), Options, [0]),
	% Set up stack
	extract_query(stack(Bp, Sp, Return), [Options], [0xf00000, 0xf000000, 0xf000]),
	( Ana0 = noninter ->
	  extract_query(low(Low), Options, [[]]),
	  Ana = noninter(Low)
	; Ana = Ana0
	),
	( member(term_stop_spec, Options) -> set_term_stop_spec
	; true % (use default)
	),
	( member(weak, Options) -> set_weak
	; true
	),
	( member(stats(StatsOut), Options) -> set_stats
	; true
	),
	init_general_stats,
	( member(skip_unknown, Options) -> init_ignore_unknown_instructions
	; true
	),
	( member(solver(Solver), Options) -> set_ext_solver(Solver)
	; true % (use default)
	),
	( member(window(WSize), Options) -> set_window_size(WSize)
	; true % (use default)
	),
	( member(no_show_conf, Options) -> true
	; set_print_configurations % (use default)
	),
	( member(step(SLimit), Options) -> set_step_limit(SLimit)
	; true % (use default)
	),
	init_paths, % Initialize number of paths traced
	( Ext = '.s' ->
	    Prg = ~translate_x86_to_muasm(gas, PrgFile, Dic, KeepS,  HeapDir, Heap)
	; Ext = '.asm' ->
	    Prg = ~translate_x86_to_muasm(intel, PrgFile, Dic, KeepS, HeapDir, Heap)
	; Ext = '.muasm' ->
	    Prg = ~(muasm_parser:parse_file(PrgFile, Dic))
	; throw(unknown_extension(PrgFile))
	),
	( member(noinit, Options) -> Memory = [], Assignments = []
	; Heap = c(Memory, Assignments)
	),
	M1 = ~append(M0, [Sp=Return|Memory]),
	A1 = ~append(A0, [pc=Entry, sp=Sp, bp=Bp|Assignments]),
	translate_labels(M1, Dic, M),
	translate_labels(A1, Dic, A),
        load_program(Prg), % (This instantiates labels too)
	% write(labels(Dic)), nl,
	%
	write('---------------------------------------------------------------------------'), nl,
	write('prg='), writeq(PrgNameExt), write(', '), % program
	write(SpecOpt), write(', '), % spec or nonspec
	( SpecOpt = spec -> write('window_size='), write(~get_window_size), write(', ') % speculative window size
	; true
	),
	write('solver='), write(~get_ext_solver), write(', '), % external solver
	write('ana='), write(Ana), nl, % kind of analysis
	( print_configurations ->
	  write('m='), write(M), nl, % initial memory
	  write('a='), write(A), nl % initial registers
	; true
	),
	%
	C0 = ~initc(SpecOpt, M, A),
	write('program:'), nl,
	show_program,
	statistics(walltime, [T0, _]),
	runtest2(Ana, C0),
	statistics(walltime,[T, _]),
	Time is T - T0,
	( stats ->
	    new_general_stat(total_time=Time),
	    print_all_stats(StatsOut)
	; true
	).

translate_labels([], _, []).
translate_labels([K=V|KVs], Dic, [K=V2|KVs2]) :-
	( dic_get(Dic, V, V1) -> V2 = V1
	; V2 = V
	),
	translate_labels(KVs, Dic, KVs2).

initc(nospec, M, A) := ~new_c(M, A).
initc(spec, M, A) := ~new_xc(M, A).

runtest2(none, _C0) :- !.
runtest2(reach, C0) :- !,
	( % (failure-driven loop)
	  (C,Trace) = ~concrun(C0),
	     pretty_print([triple(C0,Trace,C)]),
	     fail
	; true
	).
runtest2(reach1, C0) :- !,
	( (C,Trace) = ~mrun(C0) -> true ; fail ), % (only first path)
	pretty_print([triple(C0,Trace,C)]).
runtest2(noninter(Low), C0) :- !,
	noninter_check(Low, C0).

get_conf_file(Opts,Path) := ConfFile :-
	( member(conf_file(ConfFile), Opts) ->
	    true
	; path_concat(Path, 'config', ConfFile),
	  file_exists(ConfFile)
	).

% Query on the list of lists, if there's no coincidence, returns a default value
:- export(extract_query/3).
extract_query(Query, L, _) :- member(Query, L), !.
extract_query(Query, _, Default) :- Query =.. [_|Default].