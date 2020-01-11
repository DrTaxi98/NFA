Regular Expressions Compilation into Non-Deterministic Finite State Automatas

The file nfa.pl is a Prolog library for regular expressions compilation
into non-deterministic finite state automatas.

Predicates to use:
1. is_regexp/1: true when the argument is a regular expression
   (the alphabet is made of Prolog terms).
2. nfa_regexp_comp/2: true when the second argument is a regular expression
   compilable into an automata, identificated by the first argument.
3. nfa_test/2: true when the second argument is an input accepted by
   the automata identificated by the first argument.
4. nfa_clear/0, nfa_clear/1: true when all the automatas/the automata
   defined by the first argument are/is removed.
5. nfa_list/0, nfa_list/1: true when all the automatas/the automata
   defined by the first argument are/is listed.
   WARNING: if you use these two predicates without using before
   nfa_regexp_comp/2, these two predicates generate an error.
   To avoid it, you can use the predicate nfa_clear/0 and then
   one of this two predicates (but, obviously, the listed database would
   be empty).