args=" --target:library"
args="$args data-struct.fs"
args="$args nfa2dfa/nfa2dfa.fs nfa2dfa/nfaparser.fs"
args="$args grammar2set/grammarparser.fs grammar2set/grammar2set.fs"
args="$args propmachine/propparser.fs propmachine/prop2table.fs"
args="$args comcalc/mathparser.fs comcalc/comcalc.fs"
args="$args fsmaskiner.fs"
args="$args --standalone"
fsharpc  $args 
