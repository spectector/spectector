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

:- doc(title, "Program DB and assembler").

:- use_module(library(operators)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(engine(messages_basic), [message/2]).

:- export(p/2).
:- data p/2.

:- export(loc/2).
:- data loc/2.

:- export(load_program/2).
% load_program(+Ins,+Locs): 
%   Load a program from Ins, filling instruction addresses and
%   resolving labels.
load_program(Ins,Locs) :-
        muasm_operators,
        retractall_fact(p(_,_)),
        retractall_fact(loc(_,_)),
        asm(Ins, 0, PIns),
        ( ground(PIns) -> true
        ; message(warning, 'the program contains unresolved labels')
        ),
        ( member(p(Addr,I), PIns),
            assertz_fact(p(Addr,I)),
            fail
        ; true
        ),
        ( member(Name=Addr, Locs),
            assertz_fact(loc(Name,Addr)),
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
        ),
        ( loc(Name,Addr),
            display('sym '), display(Name), display('='), write(Addr), nl,
            fail
        ; true
        ).

% TODO: only for parsing and pretty printing
% :- export(muasm_operators/0).
muasm_operators :-
        op(980, xfx, [(<-)]).

