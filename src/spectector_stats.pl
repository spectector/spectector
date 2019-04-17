 % Copyright 2019 The Spectector authors
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

:- module(_, [], [datafacts, fsyntax]).

%:- doc(title, "Spectector statistics").

:- use_module(library(stream_utils), [write_string/2]).
:- use_module(library(write), [write/2]).
:- use_module(library(streams), [nl/0]).
:- use_module(library(lists), [select/3, append/3]).
:- use_module(engine(stream_basic), [open/3, close/1]).
:- use_module(library(pillow/json), [json_to_string/2]).
:- use_module(library(aggregates), [findall/3]).
%:- use_module(concolic(concolic_stats)).

:- data paths_json/1.

:- export(set_paths_json/1).
set_paths_json(JSON) :- set_fact(paths_json(JSON)).

% Main structure, it's a list, which element has the statistics of the path
:- data paths/1.
:- data n_paths/1.
:- export(init_paths/0).
init_paths :- retractall_fact(paths(_)), set_fact(n_paths(0)), restore_path_info.
restore_path_info :- init_pc_freq, init_unsupported_instructions,
	init_path_stats, init_unknown_labels, init_formulas_length,
	init_indirect_jumps, restart_ins_executed.
:- export(new_path/1).
new_path(Stats) :- % Input must be a list with the format [k1=v1,k2=v2...]
	findall(PC=Val, pc_freq(PC, Val), PCFreqs),
	n_paths(N0), N1 is N0 + 1, set_fact(n_paths(N1)),
	unsupported_instructions(Unknown),
	findall(L, list_stats(unknown_labels, L), UL),
	findall(F, list_stats(formulas_length, F), TL),
	findall(J, list_stats(indirect_jumps, J), IJ),
	ins_executed(IE),
	PATH = json([pc=json(PCFreqs), unsupported_ins=Unknown, unknown_labels=UL,
	                        indirect_jumps=IJ, steps=IE,
				formulas_length=TL|~append(Stats, ~findall(Stat, list_stats(path, Stat)))]),
	assertz_fact(paths(N0=PATH)),
	json_to_string(PATH, StringJSON),
	open(~paths_json, append, Stream),
	write_string(Stream, "\"path"),
	write(Stream, N0),
	write_string(Stream, "\"="),
	write_string(Stream, StringJSON),
	write_string(Stream, ","),
	close(Stream),
	restore_path_info.


:- data list_stats/2.
init_list_stats :- retractall_fact(list_stats(_,_)).

init_path_stats :- retractall_fact(list_stats(path, _)).
:- export(add_path_stat/1).
add_path_stat(Stat) :- assertz_fact(list_stats(path, Stat)).

init_formulas_length :- retractall_fact(list_stats(formulas_length, _)).
:- export(add_formula_length/1).
add_formula_length(L) :- assertz_fact(list_stats(formulas_length, L)).

:- export(init_analysis_stats/0).
init_analysis_stats :- retractall_fact(list_stats(analysis_stats, _)).
:- export(new_analysis_stat/1).
new_analysis_stat(Stat) :- assertz_fact(list_stats(analysis_stats, Stat)).

:- export(init_general_stats/0).
init_general_stats :- retractall_fact(list_stats(general_stats, _)).
:- export(new_general_stat/1).
new_general_stat(Stat) :- assertz_fact(list_stats(general_stats, Stat)).

:- export(init_unknown_labels/0).
init_unknown_labels :- retractall_fact(list_stats(unknown_labels, _)).
:- export(new_unknown_label/1).
new_unknown_label(L) :- assertz_fact(list_stats(unknown_labels, L)).

:- export(init_indirect_jumps/0).
init_indirect_jumps :- retractall_fact(list_stats(indirect_jumps, _)).
:- export(new_indirect_jump/1).
new_indirect_jump(Reg) :- assertz_fact(list_stats(indirect_jumps, Reg)).

:- data pc_freq/2.
init_pc_freq :- retractall_fact(pc_freq(_,_)).
:- export(inc_pc/1).
inc_pc(PC) :-
	( retract_fact(pc_freq(PC, N0)) ->
	    N1 is N0 + 1,
	    assertz_fact(pc_freq(PC,N1))
	; assertz_fact(pc_freq(PC, 1))
	).

:- data last_time/1. % To measure times
:- export(last_time/1).
:- export(set_last_time/1).
set_last_time(T) :- set_fact(last_time(T)).

:- export(assert_analysis_stat/2). % Format and emit
assert_analysis_stat(Entry, Output) :-
	( Output = stdout -> % If stdout
	  OutStream = user_output,
	  File=false
	; open(Output, append, OutStream),
	  File=string(~atom_codes(Output))
	),
	n_paths(L),
	Paths = [length=L|~findall(PATHS, paths(PATHS))],
	Stats = ~append(~findall(AS, list_stats(analysis_stats, AS)),
	                ~findall(GS, list_stats(general_stats, GS))),
	JSONcontents = [file=File,paths=json(Paths),entry=string(~atom_codes(Entry))|Stats],
	json_to_string(json(JSONcontents), Str),
	write_string(OutStream, Str),
	write_string(OutStream, ","), nl,
	close(OutStream).

:- data unsupported_instructions/1.
:- export(unsupported_instructions/1).
:- export(init_unsupported_instructions/0).
init_unsupported_instructions :- set_fact(unsupported_instructions(0)).
:- export(increment_unsupported_instructions/0).
increment_unsupported_instructions :-
	unsupported_instructions(N0), N1 is N0 + 1,
	set_fact(unsupported_instructions(N1)).

:- data ins_executed/1.
:- export(restart_ins_executed/0).
restart_ins_executed :- set_fact(ins_executed(0)).
:- export(inc_executed_ins/0).
inc_executed_ins :- InsExecuted is ~ins_executed + 1, set_fact(ins_executed(InsExecuted)).

% TODO: For getting lines of code
% :-use_module(library(process)).
% process_call(path(cloc), ['/tmp/test2.s', '--json', '--hide-rate'], [stdout(string(Out))]), string_to_json(Out, Json), json_get(Json, 'Assembly', AsJson), json_get(AsJson, code, R).