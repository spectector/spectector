:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

:- doc(title, "Program DB and assembler").

:- use_module(library(operators)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(engine(messages_basic), [message/2]).

:- export(p/2).
:- data p/2.

:- export(load_program/1).
% load_program(+Ins): 
%   Load a program from Ins, filling instruction addresses and
%   resolving labels.
load_program(Ins) :-
	muasm_operators,
	retractall_fact(p(_,_)),
	asm(Ins, 0, PIns),
	( ground(PIns) -> true
	; message(warning, 'the program contains unresolved labels')
	),
	( member(p(Addr,I), PIns),
	    assertz_fact(p(Addr,I)),
	    fail
	; true
	).

% (this just patch labels)
asm([], _, []).
asm([I|Is], Addr, Is2) :- I = label(Addr0), !,
	% unify label with address
	Addr = Addr0,
	asm(Is, Addr, Is2).
asm([I|Is], Addr, [I2|Is2]) :-
	I2 = p(Addr, I),
	Addr1 is Addr + 1,
	asm(Is, Addr1, Is2).

:- export(show_program/0).
show_program :-
	( p(Addr,I),
	    display('  '), display(Addr), display(': '), write(I), nl,
	    fail
	; true
	).

% TODO: only for parsing and pretty printing
% :- export(muasm_operators/0).
muasm_operators :-
	op(980, xfx, [(<-)]).

