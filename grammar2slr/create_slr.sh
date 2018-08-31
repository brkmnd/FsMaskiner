fsharpc ../data-struct.fs ../nfa2dfa/converter.fs ../grammar2set/grammarparser.fs grammarparser_ext.fs ../grammar2set/grammar2set.fs grammar2slr.fs grammar2js_table.fs create_slr.fsx -o:table_exe
mono table_exe $1
rm table_exe
firefox my_parser.html
