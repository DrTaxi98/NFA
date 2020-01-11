%%%% -*- Mode: Prolog -*-

/*    nfa.pl: regular languages compiler and NFA tester
      Copyright (C) 2020 Andrea Tassi

      This program is free software: you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
      the Free Software Foundation, either version 3 of the License, or
      (at your option) any later version.

      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
      GNU General Public License for more details.

      You should have received a copy of the GNU General Public License
      along with this program. If not, see <https://www.gnu.org/licenses/>.

      Author's email address: andreatassi98@gmail.com
*/

/* Facts: 'seq', 'or', 'star' and 'plus'
   are reserved functors.
*/
reserved_functor(seq).
reserved_functor(or).
reserved_functor(star).
reserved_functor(plus).

% True when Functor is not a reserved functor.
is_not_reserved_functor(Functor) :-
    reserved_functor(Functor),
    !,
    fail.
is_not_reserved_functor(_).

/* True when Arity respects the arity of the functor
   (the first argument).
*/
functor_arity(seq, Arity) :-
    Arity >= 2,
    !.
functor_arity(or, Arity) :-
    Arity >= 2,
    !.
functor_arity(star, Arity) :-
    Arity is 1,
    !.
functor_arity(plus, Arity) :-
    Arity is 1,
    !.

/* True when Term is a symbol of the alphabet
   (atomic or compound without a reserved functor).
*/
is_alphabetical_symbol(Term) :-
    atomic(Term),
    !.
is_alphabetical_symbol(Term) :-
    compound(Term),
    !,
    functor(Term, Functor, _),
    is_not_reserved_functor(Functor).

/* True when RE is a regular expression
   (terms with reserved functors must respect their arity).
*/
is_regexp(RE) :-
    is_alphabetical_symbol(RE),
    !.
is_regexp(RE) :-
    compound(RE),
    functor(RE, Functor, Arity),
    reserved_functor(Functor),
    !,
    functor_arity(Functor, Arity),
    RE =.. [_ | Args],
    are_args_regexps(Args).

/* True when the arguments of a compound term
   are regular expressions.
*/
are_args_regexps([]) :-
    !.
are_args_regexps([Arg | OtherArgs]) :-
    !,
    is_regexp(Arg),
    are_args_regexps(OtherArgs).

/* True when RE is compilable into an automata,
   which is added to the Prolog database.
   FA_Id becomes the automata identificator
   (it must be a Prolog term without variables).
*/
nfa_regexp_comp(FA_Id, RE) :-
    nonvar(FA_Id),
    is_regexp(RE),
    RE =.. [Functor | Args],
    thompsons_construction(FA_Id, Functor, Args).

/* True when the automata is identificated by FA_Id
   and Symbol is a symbol of the alphabet that generates
   two states and a delta transition between them.
*/
thompsons_construction(FA_Id, Symbol, []) :-
    !,
    is_alphabetical_symbol(Symbol),
    gensym(q, Initial),
    assert(nfa_initial(FA_Id, Initial)),
    gensym(q, Final),
    assert(nfa_final(FA_Id, Final)),
    assert(nfa_delta(FA_Id, Initial, Symbol, Final)).
/* True when the automata identificated by FA_Id
   is built to accept a regexp with the functor 'seq'.
*/
thompsons_construction(FA_Id, seq, [Arg]) :-
    !,
    nfa_regexp_comp(FA_Id, Arg).
thompsons_construction(FA_Id, seq, [Arg | OtherArgs]) :-
    !,
    nfa_regexp_comp(FA_Id, Arg),
    retract(nfa_initial(FA_Id, FirstInitial)),
    retract(nfa_final(FA_Id, FirstFinal)),
    thompsons_construction(FA_Id, seq, OtherArgs),
    retract(nfa_initial(FA_Id, SecondInitial)),
    assert(nfa_initial(FA_Id, FirstInitial)),
    assert(nfa_delta(FA_Id, FirstFinal, epsilon, SecondInitial)).
/* True when the automata identificated by FA_Id
   is built to accept a regexp with the functor 'or'.
*/
thompsons_construction(FA_Id, or, [Arg]) :-
    !,
    nfa_regexp_comp(FA_Id, Arg).
thompsons_construction(FA_Id, or, [Arg | OtherArgs]) :-
    !,
    gensym(q, Initial),
    nfa_regexp_comp(FA_Id, Arg),
    retract(nfa_initial(FA_Id, FirstInitial)),
    retract(nfa_final(FA_Id, FirstFinal)),
    thompsons_construction(FA_Id, or, OtherArgs),
    retract(nfa_initial(FA_Id, SecondInitial)),
    retract(nfa_final(FA_Id, SecondFinal)),
    gensym(q, Final),
    assert(nfa_initial(FA_Id, Initial)),
    assert(nfa_final(FA_Id, Final)),
    assert(nfa_delta(FA_Id, Initial, epsilon, FirstInitial)),
    assert(nfa_delta(FA_Id, Initial, epsilon, SecondInitial)),
    assert(nfa_delta(FA_Id, FirstFinal, epsilon, Final)),
    assert(nfa_delta(FA_Id, SecondFinal, epsilon, Final)).
/* True when the automata identificated by FA_Id
   is built to accept a regexp with the functor 'star'.
*/
thompsons_construction(FA_Id, star, Arg) :-
    !,
    thompsons_construction(FA_Id, plus, Arg),
    nfa_initial(FA_Id, Initial),
    nfa_final(FA_Id, Final),
    assert(nfa_delta(FA_Id, Initial, epsilon, Final)).
/* True when the automata identificated by FA_Id
   is built to accept a regexp with the functor 'plus'.
*/
thompsons_construction(FA_Id, plus, [Arg]) :-
    !,
    gensym(q, Initial),
    nfa_regexp_comp(FA_Id, Arg),
    gensym(q, Final),
    retract(nfa_initial(FA_Id, InternalInitial)),
    assert(nfa_initial(FA_Id, Initial)),
    retract(nfa_final(FA_Id, InternalFinal)),
    assert(nfa_final(FA_Id, Final)),
    assert(nfa_delta(FA_Id, Initial, epsilon, InternalInitial)),
    assert(nfa_delta(FA_Id, InternalFinal, epsilon, InternalInitial)),
    assert(nfa_delta(FA_Id, InternalFinal, epsilon, Final)).

/* True when Input for the automata identificated
   by FA_Id it's completely consumed and the automata
   is in a final state.
*/
nfa_test(FA_Id, Input) :-
    nfa_initial(FA_Id, Initial),
    nfa_accept(FA_Id, Input, Initial).

/* True when the input is completely consumed
   and the automata FA_Id is in a final state.
*/
nfa_accept(FA_Id, [], Final) :-
    nfa_final(FA_Id, Final),
    !.
% epsilon-transition, where Input is not consumed.
nfa_accept(FA_Id, Input, FirstState) :-
    nfa_delta(FA_Id, FirstState, epsilon, SecondState),
    nfa_accept(FA_Id, Input, SecondState),
    !.
/* True when the first symbol of the input is 'epsilon'
   and the state doesn't change.
*/
nfa_accept(FA_Id, [epsilon | OtherSymbols], FirstState) :-
    !,
    nfa_accept(FA_Id, OtherSymbols, FirstState).
/* True when the first symbol of the input is consumed
   and the automata FA_Id passes to another state.
*/
nfa_accept(FA_Id, [Symbol | OtherSymbols], FirstState) :-
    !,
    is_alphabetical_symbol(Symbol),
    nfa_delta(FA_Id, FirstState, Symbol, SecondState),
    nfa_accept(FA_Id, OtherSymbols, SecondState).

/* True when all the defined automatas are removed
   from the Prolog database.
*/
nfa_clear :-
    nfa_clear(_),
    fail.
nfa_clear.

/* True when the automata FA_Id is removed
   from the Prolog database.
*/
nfa_clear(FA_Id) :-
    retract(nfa_initial(FA_Id, _)),
    retract(nfa_final(FA_Id, _)),
    retract(nfa_delta(FA_Id, _, _, _)),
    fail.
nfa_clear(_).

/* True when all the defined automatas
   are listed with their structure.
*/
nfa_list :-
    nfa_initial(FA_Id, _),
    nfa_list(FA_Id),
    fail.
nfa_list.

/* True when the automata FA_Id
   is listed with his structure.
*/
nfa_list(FA_Id) :-
    listing(nfa_initial(FA_Id, _)),
    listing(nfa_final(FA_Id, _)),
    listing(nfa_delta(FA_Id, _, _, _)).

%%%% -*- eof: nfa.pl -*-
