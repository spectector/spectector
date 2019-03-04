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

:- use_module(library(write)).
:- use_module(library(lists), [select/3, append/3]).
:- use_module(engine(stream_basic)).
:- use_module(library(pillow/json)).
%:- use_module(concolic(concolic_stats)).

% Main structure, it's a list, which element has the statistics of the path
:- data paths/1.
:- data n_paths/1.
:- export(init_paths/0).
init_paths :- set_fact(paths([])), set_fact(n_paths(0)), restore_path_info.
restore_path_info :- init_program_counters, init_unknown_instructions,
	init_path_stats, init_unknown_labels, init_formulas_length.
:- export(new_path/1).
new_path(Stats) :- % Input must be a list with the format [k1=v1,k2=v2...]
	paths(Paths), program_counters(PC),
	n_paths(N0),
	N1 is N0 + 1, set_fact(n_paths(N1)),
	unknown_instructions(Unknown),
	unknown_labels(UL),
	formulas_length(TL),
	set_fact(paths([N0=json([pc=json(PC),unknown_ins=Unknown,unknown_labels=json(UL),
				formulas_length=TL|~append(Stats, ~path_stats)])|Paths])),
	restore_path_info.

:- data path_stats/1.
init_path_stats :- set_fact(path_stats([])).
:- export(add_path_stat/1).
add_path_stat(Stat) :- set_fact(path_stats([Stat|~path_stats])).

:- data formulas_length/1.
:- export(formulas_length/1).
:- export(init_formulas_length/0).
init_formulas_length :- set_fact(formulas_length([])).
:- export(add_formula_length/1).
add_formula_length(L) :- set_fact(formulas_length([L|~formulas_length])).

:- data general_stats/1.
:- export(init_general_stats/0).
init_general_stats :- set_fact(general_stats([])).
:- export(new_general_stat/1).
new_general_stat(Stat) :- set_fact(general_stats([Stat|~general_stats])).

:- data program_counters/1.
init_program_counters :- set_fact(program_counters([])).
:- export(add_program_counters/1).
add_program_counters(PC) :-
	program_counters(PVC),
	( select(PC=N0, PVC, L),
	  N1 is N0 + 1,
	  set_fact(program_counters([PC=N1|L]))
	; set_fact(program_counters([PC=1|PVC]))
	).

:- data last_time/1. % To measure times
:- export(last_time/1).
:- export(set_last_time/1).
set_last_time(T) :- set_fact(last_time(T)).

:- data unknown_labels/1.
:- export(init_unknown_labels/0).
init_unknown_labels :- set_fact(unknown_labels([])).
:- export(new_general_stat/1).
new_unknown_label(Stat) :- set_fact(unknown_labels([Stat|~unknown_labels])).


:- export(print_all_stats/1). % Format and emit
% TODO: measure timeout
print_all_stats(Output) :-
	general_stats(Stats),
	paths(Paths0), n_paths(L),
	Paths1 = [length=L|Paths0],
	( Output = stdout -> % If stdout
	  OutStream = user_output,
	  File=false
	; open(Output, write, OutStream),
	  File=string(~atom_codes(Output))
	),
	json_to_string(json([file=File,paths=json(Paths1)|Stats]), Str),
	atom_codes(Json, Str),
	write(OutStream, Json).

:- data unknown_instructions/1.
:- export(unknown_instructions/1).
:- export(init_unknown_instructions/0).
init_unknown_instructions :- set_fact(unknown_instructions(0)).
:- export(increment_unknown_instructions/0).
increment_unknown_instructions :-
	unknown_instructions(N0), N1 is N0 + 1,
	set_fact(unknown_instructions(N1)).

% TODO: For getting lines of code
% :-use_module(library(process)).
% process_call('/bin/cloc', ['/tmp/test2.s', '--json', '--hide-rate'], [stdout(string(Out))]), string_to_json(Out, Json), json_get(Json, 'Assembly', AsJson), json_get(AsJson, code, R).