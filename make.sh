args=" --target:library"
args="$args data-struct.fs"
args="$args nfa2dfa/nfa2dfa.fs nfa2dfa/nfaparser.fs"
args="$args grammar2set/grammarparser.fs grammar2set/grammar2set.fs"
args="$args propmachine/propparser.fs propmachine/prop2table.fs"
args="$args comcalc/mathparser.fs comcalc/comcalc.fs"
args="$args grammar2slr/grammarparser_ext.fs grammar2slr/grammar2slr.fs grammar2slr/grammar2js_table.fs"
#MbSql
cd ../../Yacc/FsYacc-MySql/
./build_parser.sh
cd ../../F#/brkmnd.com-maskiner
args="$args ../../Yacc/FsYacc-MySql/lib/FsLexYacc.Runtime.Lexing.fs"
args="$args ../../Yacc/FsYacc-MySql/lib/FsLexYacc.Runtime.Parsing.fs"
args="$args ../../Yacc/FsYacc-MySql/absyn.fs"
args="$args ../../Yacc/FsYacc-MySql/traverse.fs"
args="$args ../../Yacc/FsYacc-MySql/parser.fs"
args="$args ../../Yacc/FsYacc-MySql/lexer.fs"
args="$args ../../Yacc/FsYacc-MySql/MbSqlDriver.fs"
args="$args FsMaskiner.fs"
args="$args --standalone"
fsharpc $args 
