Regular Expressions Compilation into Non-Deterministic Finite State Automatas

The file nfa.pl is a Lisp library for regular expressions compilation
into non-deterministic finite state automatas.

Functions to use:
1. (is-regexp RE) returns true if RE is a regular expression,
   NIL otherwise (the alphabet is made of S-exps).
2. (nfa-regexp-comp RE) returns the automata obtained by the compilation of RE,
   if it is a regular expression, NIL otherwise.
   If it can't compile RE, it returns NIL.
3. (nfa-test FA Input) returns true when Input for the automata FA
   is completely consumed and it is in a final state.
   Input is a Lips list of symbols of the alphabet.
   If FA doesn't have an automata structure, it generates an error.
   Otherwise, it returns T if it recognises Input, NIL if it doesn't.