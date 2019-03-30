var productions_str = {
    0:{prod:"__",rside:["Stmt"]},
    1:{prod:"Stmt",rside:["Term0"]},
    2:{prod:"Stmt",rside:["Def","scolon","Stmt"]},
    3:{prod:"Def",rside:["let","var","eq","Term0"]},
    4:{prod:"Term0",rside:["Term1"]},
    5:{prod:"Term0",rside:["lambda","var","dot","Term0"]},
    6:{prod:"Term1",rside:["Term2"]},
    7:{prod:"Term1",rside:["Term1","Term2"]},
    8:{prod:"Term2",rside:["lpar","Term0","rpar"]},
    9:{prod:"Term2",rside:["var"]}
};
var actionTable = {
    0:{
        "dot":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'eq'"); },
        "lambda":function(lang){ return lang.shift(6); },
        "let":function(lang){ return lang.shift(7); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given '$'"); }
        },
    1:{
        "dot":function(lang){ return lang.error("expected 'scolon', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'scolon', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'scolon', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'scolon', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'scolon', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'scolon', but given 'rpar'"); },
        "scolon":function(lang){ return lang.shift(10); },
        "var":function(lang){ return lang.error("expected 'scolon', but given 'var'"); },
        "$":function(lang){ return lang.error("expected 'scolon', but given '$'"); }
        },
    2:{
        "dot":function(lang){ return lang.error("expected 'eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'eoi', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'eoi', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'eoi', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'eoi', but given 'var'"); },
        "$":function(lang){ return lang.accept(); }
        },
    3:{
        "dot":function(lang){ return lang.error("expected 'eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'eoi', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'eoi', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'eoi', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'eoi', but given 'var'"); },
        "$":function(lang){ return lang.reduce(1); }
        },
    4:{
        "dot":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.reduce(4); },
        "scolon":function(lang){ return lang.reduce(4); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.reduce(4); }
        },
    5:{
        "dot":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.reduce(6); },
        "rpar":function(lang){ return lang.reduce(6); },
        "scolon":function(lang){ return lang.reduce(6); },
        "var":function(lang){ return lang.reduce(6); },
        "$":function(lang){ return lang.reduce(6); }
        },
    6:{
        "dot":function(lang){ return lang.error("expected 'var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'var', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'var', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'var', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'var', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(12); },
        "$":function(lang){ return lang.error("expected 'var', but given '$'"); }
        },
    7:{
        "dot":function(lang){ return lang.error("expected 'var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'var', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'var', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'var', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'var', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(13); },
        "$":function(lang){ return lang.error("expected 'var', but given '$'"); }
        },
    8:{
        "dot":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'eq'"); },
        "lambda":function(lang){ return lang.shift(6); },
        "let":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'let'"); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.error("expected 'lambda','lpar','var', but given '$'"); }
        },
    9:{
        "dot":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.reduce(9); },
        "rpar":function(lang){ return lang.reduce(9); },
        "scolon":function(lang){ return lang.reduce(9); },
        "var":function(lang){ return lang.reduce(9); },
        "$":function(lang){ return lang.reduce(9); }
        },
    10:{
        "dot":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'eq'"); },
        "lambda":function(lang){ return lang.shift(6); },
        "let":function(lang){ return lang.shift(7); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.error("expected 'lambda','let','lpar','var', but given '$'"); }
        },
    11:{
        "dot":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.reduce(7); },
        "rpar":function(lang){ return lang.reduce(7); },
        "scolon":function(lang){ return lang.reduce(7); },
        "var":function(lang){ return lang.reduce(7); },
        "$":function(lang){ return lang.reduce(7); }
        },
    12:{
        "dot":function(lang){ return lang.shift(16); },
        "eq":function(lang){ return lang.error("expected 'dot', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'dot', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'dot', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'dot', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'dot', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'dot', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'dot', but given 'var'"); },
        "$":function(lang){ return lang.error("expected 'dot', but given '$'"); }
        },
    13:{
        "dot":function(lang){ return lang.error("expected 'eq', but given 'dot'"); },
        "eq":function(lang){ return lang.shift(17); },
        "lambda":function(lang){ return lang.error("expected 'eq', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'eq', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'eq', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'eq', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'eq', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'eq', but given 'var'"); },
        "$":function(lang){ return lang.error("expected 'eq', but given '$'"); }
        },
    14:{
        "dot":function(lang){ return lang.error("expected 'rpar', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'rpar', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'rpar', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'rpar', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'rpar', but given 'lpar'"); },
        "rpar":function(lang){ return lang.shift(18); },
        "scolon":function(lang){ return lang.error("expected 'rpar', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'rpar', but given 'var'"); },
        "$":function(lang){ return lang.error("expected 'rpar', but given '$'"); }
        },
    15:{
        "dot":function(lang){ return lang.error("expected 'eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'eoi', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'eoi', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'eoi', but given 'scolon'"); },
        "var":function(lang){ return lang.error("expected 'eoi', but given 'var'"); },
        "$":function(lang){ return lang.reduce(2); }
        },
    16:{
        "dot":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'eq'"); },
        "lambda":function(lang){ return lang.shift(6); },
        "let":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'let'"); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.error("expected 'lambda','lpar','var', but given '$'"); }
        },
    17:{
        "dot":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'eq'"); },
        "lambda":function(lang){ return lang.shift(6); },
        "let":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'let'"); },
        "lpar":function(lang){ return lang.shift(8); },
        "rpar":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'rpar'"); },
        "scolon":function(lang){ return lang.error("expected 'lambda','lpar','var', but given 'scolon'"); },
        "var":function(lang){ return lang.shift(9); },
        "$":function(lang){ return lang.error("expected 'lambda','lpar','var', but given '$'"); }
        },
    18:{
        "dot":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'lpar','rpar','scolon','var','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.reduce(8); },
        "rpar":function(lang){ return lang.reduce(8); },
        "scolon":function(lang){ return lang.reduce(8); },
        "var":function(lang){ return lang.reduce(8); },
        "$":function(lang){ return lang.reduce(8); }
        },
    19:{
        "dot":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'lpar'"); },
        "rpar":function(lang){ return lang.reduce(5); },
        "scolon":function(lang){ return lang.reduce(5); },
        "var":function(lang){ return lang.error("expected 'rpar','scolon','eoi', but given 'var'"); },
        "$":function(lang){ return lang.reduce(5); }
        },
    20:{
        "dot":function(lang){ return lang.error("expected 'scolon', but given 'dot'"); },
        "eq":function(lang){ return lang.error("expected 'scolon', but given 'eq'"); },
        "lambda":function(lang){ return lang.error("expected 'scolon', but given 'lambda'"); },
        "let":function(lang){ return lang.error("expected 'scolon', but given 'let'"); },
        "lpar":function(lang){ return lang.error("expected 'scolon', but given 'lpar'"); },
        "rpar":function(lang){ return lang.error("expected 'scolon', but given 'rpar'"); },
        "scolon":function(lang){ return lang.reduce(3); },
        "var":function(lang){ return lang.error("expected 'scolon', but given 'var'"); },
        "$":function(lang){ return lang.error("expected 'scolon', but given '$'"); }
        }
    };
var gotoTable = {
    0:{
        "Def":function(lang){ return lang.some(1); },
        "Stmt":function(lang){ return lang.some(2); },
        "Term0":function(lang){ return lang.some(3); },
        "Term1":function(lang){ return lang.some(4); },
        "Term2":function(lang){ return lang.some(5); }
        },
    1:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    2:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    3:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    4:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.some(11); }
        },
    5:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    6:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    7:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    8:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.some(14); },
        "Term1":function(lang){ return lang.some(4); },
        "Term2":function(lang){ return lang.some(5); }
        },
    9:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    10:{
        "Def":function(lang){ return lang.some(1); },
        "Stmt":function(lang){ return lang.some(15); },
        "Term0":function(lang){ return lang.some(3); },
        "Term1":function(lang){ return lang.some(4); },
        "Term2":function(lang){ return lang.some(5); }
        },
    11:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    12:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    13:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    14:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    15:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    16:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.some(19); },
        "Term1":function(lang){ return lang.some(4); },
        "Term2":function(lang){ return lang.some(5); }
        },
    17:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.some(20); },
        "Term1":function(lang){ return lang.some(4); },
        "Term2":function(lang){ return lang.some(5); }
        },
    18:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    19:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        },
    20:{
        "Def":function(lang){ return lang.none(); },
        "Stmt":function(lang){ return lang.none(); },
        "Term0":function(lang){ return lang.none(); },
        "Term1":function(lang){ return lang.none(); },
        "Term2":function(lang){ return lang.none(); }
        }
    };
var tokens = [
{name:"eq",cap:false,reg:"="},
{name:"lambda",cap:false,reg:"\\\\"},
{name:"dot",cap:false,reg:"."},
{name:"lpar",cap:false,reg:"("},
{name:"rpar",cap:false,reg:")"},
{name:"scolon",cap:false,reg:";"},
{name:"let",cap:false,reg:"let"},
{name:"var",cap:true,reg:"[a-z]+"}
]
var btokens = [" "];