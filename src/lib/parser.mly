// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.lfundecs> program

%nonassoc LT
%left PLUS
%left IF

%%
program:
| x=nonempty_list(fundec) EOF       { $loc , x }                              // regra 1 

exps:
| x=separated_nonempty_list(COMMA, exp) { x }                                 // regras 16 e 17

exp:
| x=LITINT                          { $loc , Absyn.IntExp x }                 // regra 9
| x=exp op=operator y=exp           { $loc , Absyn.OpExp (op, x, y) }         // regras 11 e 12
| x=ID                              { $loc , Absyn.IdExp x }                  // regra 10
| IF x=exp THEN y=exp ELSE z=exp    { $loc , Absyn.ConditionalExp (x, y, z) } // regra 13
| ID LPAREN y=exps RPAREN           { $loc , Absyn.FunctionsExp y }           // regra 14
| LET x=ID EQ y=exp IN z=exp        { $loc , Absyn.DeclarationExp (x, y, z) } // regra 15

%inline operator:
| PLUS { Absyn.Plus }                                               
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }              // regra 4

typeid:
| INT x=ID   { (Absyn.Int, x) }                                               // regra 5
| BOOL x=ID  { (Absyn.Bool, x) }                                              // regra 6

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }                              // regra 7 e 8
