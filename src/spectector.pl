:- module(_, [], [assertions, fsyntax]).

:- doc(title, "Spectector").
:- doc(subtitle, "SPECulative deTECTOR").

:- use_module(library(lists)).
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
  --conf-file FILE Read the initial configuration from a file
  -c,--conf CONF   Initial configuration ('c(M,A)')
  -a,--analysis ANA
      Analysis algorithm:
        none:     do nothing (useful to show the input program)
        reach:    reachability using concolic execution
        reach1:   like reach, but stop at first path
        noninter: non-interference check (default)
  --low LOW        Low registers or memory addresses for noninter
  --statistics     Show the time that the solver takes

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
opt('', '--solver', [Solver|As], As, [solver(Solver)]).
opt('', '--conf-file', [ConfFile|As], As, [conf_file(ConfFile)]).
opt('-c', '--conf', [ConfAtm|As], As, [Opt]) :-
	atom_codes(ConfAtm, ConfStr),
	read_from_string_atmvars(ConfStr, Conf),
	( Conf = c(M,A) -> true
	; throw(wrong_conf(ConfAtm))
	),
	Opt = c(M,A).
opt('-w', '--window', [NAtm|As], As, [Opt]) :-
	atom_codes(NAtm, NStr),
	number_codes(N, NStr),
	Opt = window(N).
opt('', '--steps', [NAtm|As], As, [Opt]) :-
	atom_codes(NAtm, NStr),
	number_codes(N, NStr),
	Opt = step(N).
opt('-a', '--analysis', [Ana|As], As, [ana(Ana)]).
opt('', '--low', [LowAtm|As], As, [low(Low)]) :-
	atom_codes(LowAtm, LowStr),
	read_from_string_atmvars(LowStr, Low).
opt('-r', '--reduce', As, As, [reduce]).
opt('', '--statistics', As, As, [statistics]).

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
%   - allow max_paths (max number of explored paths)
% DONE?  - show statistics

:- export(run/2).
run(PrgFile, Opts) :-
	path_split(PrgFile, Path, PrgNameExt),
	path_splitext(PrgNameExt, _PrgBasename, Ext),
	%
	( member(nospec, Opts) -> SpecOpt = nospec
	; SpecOpt = spec % (default)
	),
	( member(c(M0,A0), Opts) -> true % TODO: replace symbolic labels!
	; ConfFile = ~get_conf_file(Opts,Path) -> file_to_terms(ConfFile, [c(M0,A0)|_])
	; M0 = [], A0 = [pc=0]
	),
	( member(ana(Ana0), Opts) -> true
	; Ana0 = noninter % (default)
	),
	( Ana0 = noninter ->
	    ( member(low(Low), Opts) -> true
	    ; Low = [] % (default)
	    ),
	    Ana = noninter(Low)
	; Ana = Ana0
	),
	( member(solver(Solver), Opts) -> set_ext_solver(Solver)
	; true % (use default)
	),
	( member(window(WSize), Opts) -> set_window_size(WSize)
	; true % (use default)
	),
	( member(step(SLimit), Opts) -> set_step_limit(SLimit)
	; true % (use default)
	),
	%
	( Ext = '.s' ->
	    Prg = ~translate_x86_to_muasm(gas, PrgFile, Dic)
	; Ext = '.asm' ->
	    Prg = ~translate_x86_to_muasm(intel, PrgFile, Dic)
	; Ext = '.muasm' ->
	    Prg = ~(muasm_parser:parse_file(PrgFile, Dic))
	; throw(unknown_extension(PrgFile))
	),
        load_program(Prg), % (This instantiates labels too)
	% write(labels(Dic)), nl,
	translate_labels(M0, Dic, M),
	translate_labels(A0, Dic, A),
	%
	write('---------------------------------------------------------------------------'), nl,
	write('prg='), writeq(PrgNameExt), write(', '), % program
	write(SpecOpt), write(', '), % spec or nonspec
	( SpecOpt = spec -> write('window_size='), write(~get_window_size), write(', ') % speculative window size
	; true
	),
	write('solver='), write(~get_ext_solver), write(', '), % external solver
	write('ana='), write(Ana), nl, % kind of analysis
	write('m='), write(M), nl, % initial memory
	write('a='), write(A), nl, % initial registers
	%
	C0 = ~initc(SpecOpt, M, A),
	write('program:'), nl,
	show_program,
	( member(statistics, Opts) ->
	    statistics(walltime, [T0, _])
	; true
	),
	runtest2(Ana, C0),
	( member(statistics, Opts) ->
	    statistics(walltime,[T, _]),
	    Time is T - T0,
	    write('done in '),
	    write(Time),
	    write(' ms'),
	    nl
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
