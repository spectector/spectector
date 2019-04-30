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

:- module(_, [], [assertions, datafacts]).

:- doc(title, "Flags").

:- data window_size/1.

:- export(set_window_size/1).
set_window_size(N) :-
	set_fact(window_size(N)).

:- export(get_window_size/1).
get_window_size(N) :-
	( window_size(N0) -> N0 = N
	; N = 200 % default
	).

:- data limit_value/2.

:- export(set_limit/2).
set_limit(Name, Val) :-
	retractall_fact(limit_value(Name, _)),
	assertz_fact(limit_value(Name, Val)).

:- export(get_limit/2).
get_limit(Name, Val) :-
	( limit_value(Name, Val0) -> Val = Val0
	; default_limit(Name, Val)
	).

default_limit(step, 100000).
default_limit(full_timeout, 0). % no timeout
default_limit(noninter_timeout, 0). % no timeout
default_limit(nextpath_timeout, 0). % no timeout

:- export(term_stop_spec/0).
:- data term_stop_spec/0.

:- export(set_term_stop_spec/0).
set_term_stop_spec :- set_fact(term_stop_spec).

:- export(weak_sni/0).
:- data weak_sni/0.

:- export(set_weak_sni/0).
set_weak_sni :- set_fact(weak_sni).

:- export(stats/0).
:- data stats/0.

:- export(set_stats/0).
set_stats :- set_fact(stats).

:- export(print_defs/0).
:- data print_defs/0.

:- export(set_print_defs/0).
set_print_defs :- set_fact(print_defs).

:- export(track_all_pc/0).
:- data track_all_pc/0.

:- export(set_track_all_pc/0).
set_track_all_pc :- set_fact(track_all_pc).

:- export(explored_paths_left/1).
:- data explored_paths_left/1.

:- export(set_explored_paths_left/1).
set_explored_paths_left(N) :- set_fact(explored_paths_left(N)).

% TODO: this counter should be in spectector_noninter state
:- export(new_explored_path/0).
new_explored_path :- explored_paths_left(N0), N is N0 - 1, set_fact(explored_paths_left(N)).

:- export(skip_unsupported/0).
:- data skip_unsupported/0.

:- export(set_skip_unsupported/0).
set_skip_unsupported :- set_fact(skip_unsupported).