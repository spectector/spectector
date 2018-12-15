:- module(_, [], [assertions, datafacts]).

:- doc(title, "Flags").

:- data window_size/1.

:- export(set_window_size/1).
set_window_size(N) :-
	set_fact(window_size(N)).

:- export(get_window_size/1).
get_window_size(N) :-
	( window_size(N0) -> N0 = N
	; N = 100 % default
	).

:- data step_limit/1.

:- export(set_step_limit/1).
set_step_limit(N) :-
	set_fact(step_limit(N)).

:- export(get_step_limit/1).
get_step_limit(N) :-
	( step_limit(N0) -> N0 = N
	; N = 100000 % default
	).